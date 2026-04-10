import torch
# We initially set the seeds across the libraries we will use.
# See https://docs.pytorch.org/docs/stable/notes/randomness.html for details.
torch.manual_seed(420)
torch.backends.cudnn.benchmark = False
torch.use_deterministic_algorithms(True)
torch.backends.cudnn.deterministic = True
import random
random.seed(420)

def seed_worker(worker_id):
    worker_seed = torch.initial_seed() % 2**32
    np.random.seed(worker_seed)
    random.seed(worker_seed)

g = torch.Generator()
g.manual_seed(420)

import torchvision.io as tio
import cv2
import csv
import pandas as pd
import time

# argparse is a utility for managing command-line parameters
import argparse
parser = argparse.ArgumentParser()
parser.add_argument("-a", "--arch", type=str, help="Name of backbone architecture")
parser.add_argument("-r", "--runs", type=int, help="Number of runs to perform")
parser.add_argument("--no-wandb", default=False, action="store_true", help="Avoid W&B logging")
parser.add_argument("--frozen-backbone", default=False, action="store_true", help="Freeze the backbone")
parser.add_argument("--deeper-MT-mlp", default=False, action="store_true", help="Use a deeper MLP for task solving")
parser.add_argument("--test-ensemble-preds", default=False, action="store_true", help="At test time, ensemble predictions across all the images of the video")
parser.add_argument("--log-predictions", default=False, action="store_true", help="Save predictions made on test set")
parser.add_argument("--subsample-test-frames", default=32, type=int, help="How many frames to use at test time")
parser.add_argument("--use-micro-in-training", default=False, action="store_true", help="Use micronutrients in training")
parser.add_argument("--setup", type=str, help="Which setup to consider. usa_orig/usa_corr -> American nutrition values, no/with corrections; ita_orig/ita_corr -> Italian nutrition values, no/with corrections")
parser.add_argument("--acc-threshold-classif-ingr", default=0.5, type=float, help="The threshold used in evaluating the classification")
parser.add_argument("--weighted-loss", default=False, action="store_true", help="Whether to use class weighting for the loss function")
parser.add_argument("--lambda-ingr", default=1.0, type=float, help="")
parser.add_argument("--use-ASL", default=False, action="store_true", help="Whether to use ASL loss function")
parser.add_argument("--ASL-gamma-neg", default=4.0, type=float, help="")
parser.add_argument("--ASL-gamma-pos", default=1.0, type=float, help="")
parser.add_argument("--use-focal", default=False, action="store_true", help="Whether to use Focal loss function")
parser.add_argument("--focal-gamma", default=2.0, type=float, help="")
parser.add_argument("--focal-alpha", default=0.25, type=float, help="")
parser.add_argument("--sep-cls", default=False, action="store_true", help="")
parser.add_argument("--use-mlml", default=False, action="store_true", help="")

args = parser.parse_args()

# parse_csv_file is used for loading the metadata in the original Nutrition5k database.
# It is not really used for this paper, but it's useful for reproducing older results.
# (see Bianco et al, "2D Prediction of the Nutritional Composition of Dishes from Food Images:
# Deep Learning Algorithm Selection and Data Curation Beyond the Nutrition5k Project" Nutrients 2025).
def parse_csv_file(file_path):
    data = {}
    
    with open(file_path, 'r', newline='') as csv_file:
        csv_reader = csv.reader(csv_file)
        # The CSV files contain one recipe per row. First, the row contains data for the recipe (ID, total calories, etc).
        # Then, for each recorded ingredient, it contains similar fields (in groups of 7 columns) at the ingredient level.
        # Everything is stored in a dictionary, which becomes the output of this function.
        for row in csv_reader:
            dish_info = {
                "Dish ID": row[0],
                "Total Calories": float(row[1]),
                "Total Mass": float(row[2]),
                "Total Fat": float(row[3]),
                "Total Carbohydrates": float(row[4]),
                "Total Protein": float(row[5]),
                "Ingredients": []
            }
            
            # Extract the ingredients in groups of 7 columns
            ingredient_groups = [row[i:i+7] for i in range(6, len(row), 7)]
            
            for ingredient_group in ingredient_groups:
                ingr_dict = {
                    "Ingredient ID": ingredient_group[0],
                    "Ingredient Name": ingredient_group[1].lower(),
                    "Ingredient Grams": float(ingredient_group[2]),
                    "Ingredient Calories": float(ingredient_group[3]),
                    "Ingredient Fat": float(ingredient_group[4]),
                    "Ingredient Carbohydrates": float(ingredient_group[5]),
                    "Ingredient Protein": float(ingredient_group[6])
                }
                dish_info["Ingredients"].append(ingr_dict)
            
            data[row[0]] = dish_info
    
    return data
    
# parse_usa_corr_file is a second loading function, dedicated for the USA corrected setup.
# It is similar to parse_csv_file, with only minor changes due to the different file format (Excel).
def parse_usa_corr_file(file_path, return_df_recipe_macro=True, recipe_data_file_path=None):
    _verbose = False
    data = pd.read_excel(file_path)
    if _verbose:
        print(data)
    dict_df = {}
    for row_ix, row in data.iterrows():
        dish_info = {
            "Dish ID": row[0],
            "Total Calories": float(row[1]),
            "Total Mass": float(row[2]),
            "Total Fat": float(row[3]),
            "Total Carbohydrates": float(row[4]),
            "Total Protein": float(row[5]),
            "Ingredients": []
        }
        
        # Extract the ingredients in groups of 7 columns
        ingredient_groups = [row[i:i+7] for i in range(6, len(row), 7)]
        
        for ingredient_group in ingredient_groups:
            if not any(ingredient_group.isna()):
                ingr_dict = {
                    "Ingredient ID": ingredient_group[0],
                    "Ingredient Name": ingredient_group[1].lower(),
                    "Ingredient Grams": float(ingredient_group[2]),
                    "Ingredient Calories": float(ingredient_group[3]),
                    "Ingredient Fat": float(ingredient_group[4]),
                    "Ingredient Carbohydrates": float(ingredient_group[5]),
                    "Ingredient Protein": float(ingredient_group[6])
                }
                dish_info["Ingredients"].append(ingr_dict)
        
        dict_df[row[0]] = dish_info
    # print(dict_df["dish_1565972591"])
    # print(dict_df["dish_1566245398"])
    if _verbose:
        print(dict_df[list(dict_df.keys())[0]])

    return dict_df
    

# parse_xlsx_file is the loading function for the ITA metadata files.
# They are Excel files, and have some differences in the name and setup of columns compared to the USA counterpart.
# Per-recipe information is available at recipe_data_file_path, whereas file_path points at per-ingredient(-per-recipe) information.
# Per-recipe information has one row per recipe, without ingredient-level information.
# Per-ingredient, on the other hand, has N rows per recipe, with N=number of ingredients in the recipe.
def parse_xlsx_file(file_path, return_df_recipe_macro=True, recipe_data_file_path=None):
    _verbose = False
    data = pd.read_excel(file_path, header=1)
    data.drop(0, inplace=True)  # duplicate row with Italian header
    data.rename(columns={"Unnamed: 0": "DISH ID", "Unnamed: 1": "INGREDIENT ID", "Unnamed: 2": "NAME", "Unnamed: 3": "total mass"}, inplace=True)

    # We drop the first block of metadata because they contain nutritional information per 100g.
    # We are interested in the second block of metadata, containing per-portion nutrition metadata.
    data.drop([data.columns[i] for i in range(4, 92, 1)], axis=1, inplace=True)  # drop the "per 100g" columns
    if _verbose:
        print("="*20)
        print(data.head())
    data.ffill(inplace=True)  # fill empty recipe_id with last valid (for ingredients)
    data.rename(columns={cn: cn[:-2] for cn in data.columns if cn.endswith(".1")}, inplace=True)  # remove the extra ".1" added for duplicate names
    if _verbose:
        print(data.head())
        try:
            print(data.groupby("DISH ID").get_group("dish_1561662216"))
        except:
            print(data.groupby("DISH ID").get_group("dish_1573762622"))

    if return_df_recipe_macro:

        # the following block of code normalizes the column names
        if file_path != recipe_data_file_path:
            df_recipes = pd.read_excel(recipe_data_file_path)
            cols = {c: c.replace("_", " ").lower() for c in df_recipes.columns if c.lower() not in ["dish_id"]}
            df_recipes.rename(columns=cols, inplace=True)
            df_recipes.rename(columns={
                'Energy, Rec with fibre (kcal)'.lower(): 'total calories',
                'Energy(kcal)'.lower(): 'total calories',
                'Available carbohydrates (MSE)'.lower(): 'total carbohydrates',
                'Total fat'.lower(): 'total fat',
                'Total protein'.lower(): 'total protein',
                'total_mass'.lower(): 'total mass'},
                inplace=True)  # to obtain a similar header as the N5K
    
            cols = {c: c.replace("_", " ").lower() for c in data.columns if c.lower() not in ["dish_id"]}
            data.rename(columns=cols, inplace=True)
            data.rename(columns={
                'Energy, Rec with fibre (kcal)'.lower(): 'ingredient calories',
                'Energy(kcal)'.lower(): 'ingredient calories',
                'Available carbohydrates (MSE)'.lower(): 'ingredient carbohydrates',
                'Total fat'.lower(): 'ingredient fat',
                'Total protein'.lower(): 'ingredient protein',
                'total mass'.lower(): 'ingredient mass'},
                inplace=True)  # to obtain a similar header as the N5K
    
        else:
            df_recipes = pd.read_excel(recipe_data_file_path)
            # print(list(df_recipes.columns))
            cols = {c: c.replace("_", " ").lower() for c in df_recipes.columns if c.lower() not in ["dish_id"]}
            df_recipes.rename(columns=cols, inplace=True)
            df_recipes.rename(columns={
                'dish_ID': 'DISH_ID',
                'DISH_id': 'DISH_ID',
                'energy': 'total calories',
                'energy (kcal)': 'total calories',
                'carb': 'total carbohydrates',
                'fat': 'total fat',
                'protein': 'total protein',
                # 'prot': 'total protein',
                'mass': 'total mass'},
                inplace=True)  # to obtain a similar header as the N5K

            cols = {c: c.replace("_", " ").lower() for c in data.columns if c.lower() not in ["dish_id"]}
            data.rename(columns=cols, inplace=True)
            data.rename(columns={
                'dish_ID': 'DISH_ID',
                'DISH_id': 'DISH_ID',
                'energy': 'ingredient calories',
                'energy (kcal)': 'ingredient calories',
                'carb': 'ingredient carbohydrates',
                'fat': 'ingredient fat',
                'protein': 'ingredient protein',
                # 'prot': 'ingredient protein',
                'mass': 'ingredient mass'},
                inplace=True)  # to obtain a similar header as the N5K

        # Create a dictionary with per-recipe information
        dict_df = {di: dd for di, dd in zip(df_recipes.DISH_ID, df_recipes.to_dict(orient="records"))}
        if _verbose:
            try:
                print(dict_df["dish_1561662216"])
            except:
                print(dict_df["dish_1573762622"])

        if _verbose:
            print(data.head())
        tmp = {}
        # We update dict_df to also include ingredient-level information, leveraging side effects and pointers in python.
        for k, v in dict_df.items():
            new_v = v
            new_v["Ingredients"] = [{
                    "Ingredient ID": ing["dish id"],
                    "Ingredient Name": ing["name"].lower(),
                    "Ingredient Grams": float(ing["ingredient mass"]),
                    "Ingredient Calories": float(ing["ingredient calories"]),
                    "Ingredient Fat": float(ing["ingredient fat"]),
                    "Ingredient Carbohydrates": float(ing["ingredient carbohydrates"]),
                    "Ingredient Protein": float(ing["ingredient protein"])
            } for ing_ix, ing in data.groupby("dish id").get_group(k).iterrows()]
            tmp[k] = new_v

        if _verbose:
            try:
                print(tmp["dish_1561662216"])
            except:
                print(tmp["dish_1573762622"])

        return dict_df
    
    return data

import os
from torch.utils.data import Dataset
from torchvision import transforms
from PIL import Image
import numpy as np
np.random.seed(420)
from sklearn.preprocessing import MinMaxScaler

# the CustomVideoDataset class creates a "container" for all the information related to a given split of the dataset.
# It receives:
# - the paths to the metadata files, and the RGB frames (filtered or original),
# - a list with the recipe names to be put in the split (e.g., validation split),
# - scaler (for data scaling, but we are not currently using it),
# - a boolean value, is_train, to enable/disable random/fixed selection of frames,
# - a boolean value, test_ensemble_preds, to enable/disable aggregated predictions at test time.
# Note that the actual visual inputs will be RGB frames, extracted from the initial videos (as detailed in previous works).
class CustomVideoDataset(Dataset):
    def __init__(self, meta_dish_cafe1_path, meta_dish_cafe2_path, meta_path, split_names,
                 frames_path="nutrition5k_dataset/imagery/side_angles/",
                 scaler=None, is_train=False, transform=None,
                 test_ensemble_preds=False,
                 meta_recipes_cafe1_path=None, meta_recipes_cafe2_path=None):
        if "xlsx" in meta_dish_cafe1_path:
            if "ita" in meta_dish_cafe1_path.lower():
                self.meta_dish_cafe1 = parse_xlsx_file(meta_dish_cafe1_path, True, meta_recipes_cafe1_path)
                self.meta_dish_cafe2 = parse_xlsx_file(meta_dish_cafe2_path, True, meta_recipes_cafe2_path)
            
            else:
                self.meta_dish_cafe1 = parse_usa_corr_file(meta_dish_cafe1_path, True, meta_recipes_cafe1_path)
                self.meta_dish_cafe2 = parse_usa_corr_file(meta_dish_cafe2_path, True, meta_recipes_cafe2_path)
            # Note: the following metadata file is loaded but not used
            try:
                self.meta_ingredients = pd.read_excel(meta_path)
            except:
                self.meta_ingredients = pd.read_csv(meta_path)
        else:
            self.meta_dish_cafe1 = parse_csv_file(meta_dish_cafe1_path)
            self.meta_dish_cafe2 = parse_csv_file(meta_dish_cafe2_path)
            self.meta_ingredients = pd.read_csv(meta_path)

        self.frames_path = frames_path
        self.transform = transform
        self.frames_annotation = []
        self.label_annotation = []
        self.ingredient_annotation = []
        self.unique_ingredients = {}

        for cafe in [self.meta_dish_cafe1]:
            for dish_id, dish_properties in cafe.items():
                # For each recipe, we check if it belongs to the current split
                # (otherwise it's ignored to avoid leaks)
                if dish_id in split_names:
                    folder_exists = os.path.exists(os.path.join(frames_path, dish_id, "frames_sampled5"))
                    num_frames = len(os.listdir(os.path.join(frames_path, dish_id, "frames_sampled5")))
                    # for some folders/recipes, no frames were extracted from the raw data; we skip them
                    if num_frames < 2:
                        # print(os.path.join(frames_path, dish_id, "frames_sampled5"))
                        pass
                    if folder_exists and num_frames > 0:
                        _frames_path = os.path.join(frames_path, dish_id, "frames_sampled5")
                        # Using the frame extraction scripts, an additional, unwanted nested folder may be created.
                        # If it's there, we remove it.
                        if os.path.exists(os.path.join(frames_path, dish_id, "frames_sampled5", "frames_sampled5")):
                            os.rmdir(os.path.join(frames_path, dish_id, "frames_sampled5", "frames_sampled5"))
                        # Then, in the frame annotations, we start to integrate all the useful info (frames path and metadata, mostly)
                        self.frames_annotation.append(_frames_path)
                        if 'Animal protein'.lower() in dish_properties:
                            __all_columns = [
                                dish_properties['Total Calories'.lower()],
                                dish_properties['Total Fat'.lower()],
                                dish_properties['Total Carbohydrates'.lower()],
                                dish_properties['Total Protein'.lower()],
                                dish_properties['Total Mass'.lower()],
                                
                                dish_properties['Animal protein'.lower()],
                                dish_properties['Vegetable protein'.lower()],
                                dish_properties['Animal fat'.lower()],
                                dish_properties['Vegetable fat'.lower()],
                                dish_properties['Cholesterol'.lower()],
                                dish_properties['Starch (MSE)'.lower()],
                                dish_properties['Soluble carbohydrates (MSE)'.lower()],
                                dish_properties['Dietary total fibre'.lower()],
                                dish_properties['Alcohol'.lower()],
                                dish_properties['Water'.lower()],
                                dish_properties['Iron'.lower()],
                                dish_properties['Calcium'.lower()],
                                dish_properties['Sodium'.lower()],
                                dish_properties['Potassium'.lower()],
                                dish_properties['Phosphorus'.lower()],
                                dish_properties['Zinc'.lower()],
                                dish_properties['Magnesium'.lower()],
                                dish_properties['Cupper'.lower()],
                                dish_properties['Selenium'.lower()],
                                dish_properties['Chloride'.lower()],
                                dish_properties['Iodine'.lower()],
                                dish_properties['Manganese'.lower()],
                                dish_properties['Sulfur'.lower()],
                                dish_properties['Vitamin B1, Thiamin'.lower()],
                                dish_properties['Vitamin B2, Riboflavin'.lower()],
                                dish_properties['Vitamin C'.lower()],
                                dish_properties['Niacin'.lower()],
                                dish_properties['Vitamin B6'.lower()],
                                dish_properties['Total folate'.lower()],
                                dish_properties['Pantothenic acid'.lower()],
                                dish_properties['Biotin'.lower()],
                                dish_properties['Vitamin B12'.lower()],
                                dish_properties['Retinol equivalents (RE)'.lower()],
                                dish_properties['Retinol'.lower()],
                                dish_properties['ß-carotene equivalents'.lower()],
                                dish_properties['Vitamin E (ATE)'.lower()],
                                dish_properties['Vitamin D'.lower()],
                                dish_properties['Vitamin K'.lower()],
                                dish_properties['Saturated fatty acids'.lower()],
                                dish_properties['Sum of butyric (C4:0), caproic (C6:0), caprylic (C8:0) and capric (C10:0) acids'.lower()],
                                dish_properties['Lauric acid (C12:0)'.lower()],
                                dish_properties['Myristic acid (C14:0)'.lower()],
                                dish_properties['Palmitic acid (C16:0)'.lower()],
                                dish_properties['Stearic acid (C18:0)'.lower()],
                                dish_properties['Arachidic acid (C20:0)'.lower()],
                                dish_properties['Behenic acid (C22:0)'.lower()],
                                dish_properties['Monounsaturated fatty acids'.lower()],
                                dish_properties['Myristoleic acid (C14:1)'.lower()],
                                dish_properties['Palmitoleic acid (C16:1ω-7)'.lower()],
                                dish_properties['Oleic acid (C18:1ω-9cis)'.lower()],
                                dish_properties['Eicosenoic acid (C20:1ω-9)'.lower()],
                                dish_properties['Erucic acid (C22:1ω-9)'.lower()],
                                dish_properties['Polyunsaturated fatty acids'.lower()],
                                dish_properties['Linoleic acid (C18:2ω-6)'.lower()],
                                dish_properties['Linolenic acid (C18:3ω-3)'.lower()],
                                dish_properties['Arachidonic acid (C20:4ω-6)'.lower()],
                                dish_properties['Eicosapentaenoic acid (EPA) (C20:5ω-3)'.lower()],
                                dish_properties['Docosahexaenoic acid (DHA) (C22:6ω-3)'.lower()],
                                dish_properties['Other polyunsaturated fatty acids'.lower()],
                                dish_properties['Tryptophan'.lower()],
                                dish_properties['Threonine'.lower()],
                                dish_properties['Isoleucine'.lower()],
                                dish_properties['Leucine'.lower()],
                                dish_properties['Lysine'.lower()],
                                dish_properties['Methionine'.lower()],
                                dish_properties['Cystine'.lower()],
                                dish_properties['Phenilalanine'.lower()],
                                dish_properties['Tyrosine'.lower()],
                                dish_properties['Valine'.lower()],
                                dish_properties['Arginine'.lower()],
                                dish_properties['Histidine'.lower()],
                                dish_properties['Alanine'.lower()],
                                dish_properties['Aspartic acid'.lower()],
                                dish_properties['Glutamic acid'.lower()],
                                dish_properties['Glycine'.lower()],
                                dish_properties['Proline'.lower()],
                                dish_properties['Serine'.lower()],
                                dish_properties['Glucose'.lower()],
                                dish_properties['Fructose'.lower()],
                                dish_properties['Galactose'.lower()],
                                dish_properties['Sucrose (MSE)'.lower()],
                                dish_properties['Maltose (MSE)'.lower()],
                                dish_properties['Lactose (MSE)'.lower()],
                            ]
                        elif 'Total Calories'.lower() in dish_properties:
                            __all_columns = [
                                dish_properties['Total Calories'.lower()],
                                dish_properties['Total Fat'.lower()],
                                dish_properties['Total Carbohydrates'.lower()],
                                dish_properties['Total Protein'.lower()],
                                dish_properties['Total Mass'.lower()],
                            ]
                        else:
                            __all_columns = [
                                dish_properties['Total Calories'],
                                dish_properties['Total Fat'],
                                dish_properties['Total Carbohydrates'],
                                dish_properties['Total Protein'],
                                dish_properties['Total Mass'],
                            ]
                        
                        self.label_annotation.append(__all_columns)
                        self.ingredient_annotation.append(dish_properties["Ingredients"])
                        for ing in dish_properties["Ingredients"]:
                            if ing["Ingredient Name"].lower() not in self.unique_ingredients: 
                                self.unique_ingredients[ing["Ingredient Name"].lower()] = len(self.unique_ingredients)
        
        print(len(self.label_annotation), len(self.ingredient_annotation))
        assert len(self.ingredient_annotation) == len(self.label_annotation)
        
        self.is_train = is_train
        # Scaling is not currently used
        if is_train:
            scaler.fit(self.label_annotation)
        # self.label_annotation = scaler.transform(self.label_annotation)
        
        self.test_ensemble_preds = test_ensemble_preds
        
        print(f"{len(self.frames_annotation)}")

    def __len__(self):
        return len(self.frames_annotation)

    # the following is the function dedicated to putting the info for one sample/recipe together, and providing it to the model
    def __getitem__(self, idx):
        frame_path = self.frames_annotation[idx]
        frame_names = os.listdir(frame_path)
        # We count the available frames
        frame_names = [fp for fp in frame_names if not os.path.isdir(os.path.join(frame_path, fp))]
        num_frames = len(frame_names)

        # Load the frame image.
        # If we are *not* doing prediction ensembling, we only pick one frame per recipe and used that for training/validation.
        # Note that we are only skipping prediction ensembling at training time, since that would require too much time.
        # Moreover, we are still using multiple different frames per recipe along the same training run:
        # during training, rnd_ix is decided randomly (*) based on the number of available frames.
        # (*) still, since all seeds are fixed, the same frames will be used if the code is ran again.
        if not self.test_ensemble_preds:
            if self.is_train:
                rnd_ix = np.random.randint(0, num_frames)
            else:
                rnd_ix = num_frames // 2
            frame = Image.open(os.path.join(frame_path, frame_names[rnd_ix]))
            
            # Apply transformations if provided
            if self.transform:
                frame = self.transform(frame)

        # If we ensemble predictions, then we load multiple frames per recipe, and pass them all to the model.
        # Then, independent predictions are made, and finally aggregated (e.g. through mean).
        # Note that we only do this at testing time, using all the available frames per recipe.
        else:
            frame = [Image.open(os.path.join(frame_path, frame_names[rnd_ix])) for rnd_ix in range(num_frames)]
            if self.transform:
                frame = [self.transform(_frame) for _frame in frame]

        ingredients = self.ingredient_annotation[idx]
        
        label = self.label_annotation[idx]  # Cals, Fats, Carbs, Proteins, Mass, Micro(1), ..., Micro(n)
        # If we loaded N frames, then we also repeat the recipe information N times, one per frame.
        if self.test_ensemble_preds:
            label = [label] * num_frames
            
        return frame, label, ingredients

# In the following, we hard-coded the paths to the considered datasets.
# For this paper, the interesting datasets are usa/ita_corr and usa/ita_corr_filt1.
# Paths are hard-coded but should be correct as they are relative to the repo folder.
if args.setup == "ita_orig":
    meta_dish_cafe1_path = 'metadata/ita_orig/Clean_DB1_ITAorig_FINALE_REP3.xlsx'
    meta_dish_cafe2_path = 'metadata/ita_orig/Clean_DB2_ITAorig_FINALE_REP3.xlsx'
    meta_path = 'metadata/ita_orig/Clean_DB1_itawr.xlsx'  # unused
    meta_recipes_cafe1_path = 'metadata/ita_orig/Clean_DB1_ITAorig_totali_FINALE.xlsx'
    meta_recipes_cafe2_path = 'metadata/ita_orig/Clean_DB2_ITAorig_totali_FINALE.xlsx'
elif args.setup in ["ita_corr", "ita_corr_filt1"]:
    meta_dish_cafe1_path = 'metadata/ita_corr/Clean_DB1_ITAcorr_FINALE_REP3.xlsx'
    meta_dish_cafe2_path = 'metadata/ita_corr/Clean_DB2_ITAcorr_FINALE_REP3.xlsx'
    meta_path = 'metadata/ita_corr/metadata_Clean_DBs.xlsx'
    meta_recipes_cafe1_path = 'metadata/ita_corr/CleanDB1_totali.xlsx'
    meta_recipes_cafe2_path = 'metadata/ita_corr/CleanDB2_totali.xlsx'
elif args.setup == "usa_orig":
    meta_dish_cafe1_path = 'metadata/usa_orig/dish_metadata_cafe1.csv'
    meta_dish_cafe2_path = 'metadata/usa_orig/dish_metadata_cafe2.csv'
    meta_path = 'metadata/usa_orig/ingredients_metadata.csv'
    meta_recipes_cafe1_path = 'metadata/usa_orig/dish_metadata_cafe1.csv'
    meta_recipes_cafe2_path = 'metadata/usa_orig/dish_metadata_cafe2.csv'
elif args.setup in ["usa_corr", "usa_corr_filt1"]:
    meta_dish_cafe1_path = 'metadata/usa_corr/dish_metadata_cafe1_USAcorr_FINALE_REP3.xlsx'
    meta_dish_cafe2_path = 'metadata/usa_corr/dish_metadata_cafe2_USAcorr_FINALE_REP3.xlsx'
    meta_path = 'metadata/usa_corr/metadata_ingredients_Rachi.xlsx'
    meta_recipes_cafe1_path = 'metadata/usa_corr/dish_metadata_cafe1.xlsx'
    meta_recipes_cafe2_path = 'metadata/usa_corr/dish_metadata_cafe2.xlsx'
else:
    assert False, "choose a setup in ['ita_corr', 'ita_orig', 'usa_orig', 'usa_corr', 'ita_corr_filt1', 'usa_corr_filt1']"


time_df = []

# We repeat the following training-validation-testing loop for R times, R=number of runs (5 in our case)
for run_ix in range(args.runs):
    time_tmp = {}
    times__training = time.time()
    times__preproc  = time.time()
    
    print(f"running run {run_ix+1}/{args.runs} ...")
    # First, we load the recipe names used for training according to Nutrition5k paper
    train_names = open('dish_ids/splits/rgb_train_ids.txt', 'r').readlines()
    train_names = [tn.strip() for tn in train_names]
    # From them, we subsample 10% as a validation set (used for deciding when to stop training, to avoid overfitting)
    val_names = np.random.choice(range(len(train_names)), size=int(len(train_names) * 0.1), replace=False)
    val_names = [train_names[i] for i in val_names]
    train_names = [tn for tn in train_names if tn not in val_names]

    # Second, we load the recipe names used for testing, again according to Nutrition5k
    test_names = open('dish_ids/splits/rgb_test_ids.txt', 'r').readlines()
    test_names = [tn.strip() for tn in test_names]
    print(f"split info: {len(train_names)}, {len(val_names)}, {len(test_names)}")

    # Note: image transforms are set to None here; they will be updated later to those used for pretraining the open source models
    transform = None
    scaler = MinMaxScaler()
    # Here we instantiate CustomVideoDataset for the three splits.
    # Main differences in input parameters:
    # - recipe names for the three splits ({train,val,test}_names)
    # - is_train set to True only for train_dataset
    # - setting test_ensemble_preds for test
    # Note: if we are using a filtered dataset (usa/ita_corr_filt1) then the frames path is slightly different.
    train_dataset = CustomVideoDataset(meta_dish_cafe1_path, meta_dish_cafe2_path, meta_path, train_names,
                                       frames_path=f"nutrition5k_dataset/imagery/side_angles{'_FILTERED' if 'filt1' in args.setup else ''}/",
                                       transform=transform, scaler=scaler, is_train=True,
                                       meta_recipes_cafe1_path=meta_recipes_cafe1_path, meta_recipes_cafe2_path=meta_recipes_cafe2_path)
    val_dataset = CustomVideoDataset(meta_dish_cafe1_path, meta_dish_cafe2_path, meta_path, val_names,
                                     frames_path=f"nutrition5k_dataset/imagery/side_angles{'_FILTERED' if 'filt1' in args.setup else ''}/",
                                     transform=transform, scaler=scaler,
                                     meta_recipes_cafe1_path=meta_recipes_cafe1_path, meta_recipes_cafe2_path=meta_recipes_cafe2_path)
    test_dataset = CustomVideoDataset(meta_dish_cafe1_path, meta_dish_cafe2_path, meta_path, test_names,
                                      frames_path=f"nutrition5k_dataset/imagery/side_angles{'_FILTERED' if 'filt1' in args.setup else ''}/",
                                      transform=transform, scaler=scaler, test_ensemble_preds=args.test_ensemble_preds,
                                      meta_recipes_cafe1_path=meta_recipes_cafe1_path, meta_recipes_cafe2_path=meta_recipes_cafe2_path)

    # We create map_ingredients2class which assigns a number/class to each ingredient.
    # It will be used for technical aspects (e.g., padding data tensors for batching, creating the labels for the ingredient detection loss)
    tmp_map = {
        **train_dataset.unique_ingredients,
        **val_dataset.unique_ingredients,
        **test_dataset.unique_ingredients,
    }
    tmp_map = sorted(tmp_map.keys())
    map_ingredients2class = {}
    c = 0
    for ing in tmp_map:
        if ing not in map_ingredients2class:
            map_ingredients2class[ing] = c
            c += 1

    # We dump the map mostly for debug purposes
    import json
    json.dump(map_ingredients2class, open(f"map_ingredients2class_run{run_ix}_{args.setup}.json", "w"))
    print(map_ingredients2class)

    # encode_ingredient_names creates a multi-hot representation for the ingredients present in the recipe.
    # It receives a list of ingredients (e.g., sugar, milk, flour), maps them to the correct class numbers (E.g., 25, 12, 36).
    # Then, after creating a list of 0s (length=number of ingredients in the dataset), it uses those numbers as indices
    # for "activating" some of the 0s (e.g., in positions 25, 12, 36). The multi-hot vector will be used as the supervision signal
    # for the ingredient detection loss (specifically, it is used for several loss we tested, as well as the ASL).
    def encode_ingredient_names(list_of_ingredients):
        ingr_classes = [map_ingredients2class[i["Ingredient Name"].lower()] for i in list_of_ingredients]
        multi_hot_vector = torch.zeros(len(map_ingredients2class))
        multi_hot_vector[ingr_classes] = 1
        return multi_hot_vector

    # There are however some loss functions (here, the Multi-Label Margin Loss) which require a list of indices (e.g., 25, 12, 36)
    # instead of the multi-hot representation. While we do not use MLML in the final version of the paper, this can be eventually used.
    def encode_ingredient_names_to_padded_list(list_of_ingredients):
        ingr_classes = [map_ingredients2class[i["Ingredient Name"].lower()] for i in list_of_ingredients]
        ingr_classes = torch.tensor(ingr_classes)
        return ingr_classes
    
    # collate_fn puts together several samples obtained from the instance of CustomVideoDataset to create batched data to speed-up the processing.
    def collate_fn(data):
        # In general, we create a tensor of the considered frames, and a tensor with the labels (total energy, proteins, etc for each recipe).
        # Then we create a tensor with the labels for the ingredient detection loss.
        # First, we do some slightly different operations based on whether we are ensembling predictions or not.
        if not isinstance(data[0][0], torch.Tensor):  #should only happen at test time, if args.test-ensemble-preds
            frames = torch.stack(data[0][0])
            labels = torch.tensor(data[0][1])

            ingredients = [d[2] for d in data]  # ingredients --> one list of ingredients, with each as a list of dicts
            # If we are using the MLML (see above), there is some additional padding to do, since we have a different 
            # amount of ingredients per recipe (and we are encoding them as the list of classes, and not as the multi-hot).
            # Otherwise, if we are using the multi-hot representation, we are simply stacking them in the "else" branch.
            if args.use_mlml:
                from torch.nn.utils.rnn import pad_sequence
                ingredients_classIDs = [encode_ingredient_names_to_padded_list(ingr) for ingr in ingredients]
                ingredients_classIDs += [torch.zeros(len(map_ingredients2class))]  # added for padding to num_ingredients
                ingredients_classIDs = pad_sequence(ingredients_classIDs, batch_first=True, padding_value=-1)
                ingredients_classIDs = ingredients_classIDs[:-1]  # removing the extra row
            else:
                ingredients_classIDs = torch.stack([encode_ingredient_names(ingr) for ingr in ingredients])
            # print("step1", ingredients_classIDs.shape)
            ingredients_classIDs = ingredients_classIDs.repeat(len(frames), 1)
            # print("step2", ingredients_classIDs.shape)
        else:
            frames = torch.stack([d[0] for d in data])
            labels = torch.stack([torch.tensor(d[1]) for d in data])

            ingredients = [d[2] for d in data]  # ingredients --> B list of ingredients, with each as a list of dicts
            if args.use_mlml:
                from torch.nn.utils.rnn import pad_sequence
                ingredients_classIDs = [encode_ingredient_names_to_padded_list(ingr) for ingr in ingredients]
                ingredients_classIDs += [torch.zeros(len(map_ingredients2class))]  # added for padding to num_ingredients
                ingredients_classIDs = pad_sequence(ingredients_classIDs, batch_first=True, padding_value=-1)
                ingredients_classIDs = ingredients_classIDs[:-1]  # removing the extra row
            else:
                ingredients_classIDs = torch.stack([encode_ingredient_names(ingr) for ingr in ingredients])
        
        return frames, labels, ingredients_classIDs  #cals, fats, carbs, proteins, mass

    # Access data samples. Just a test to visually see what we are loading.
    sample_frame, sample_label, ingredients = test_dataset[0]
    if args.test_ensemble_preds:
        print("Sample (list of) Frame Shape:", sample_frame[0].size, "len", len(sample_frame))
        print("Sample Label:", sample_label[0])
        print("Sample Ingredients:", ingredients[0])
    else:
        print("Sample Frame Shape:", sample_frame.size)
        print("Sample Label:", sample_label)
        print("Sample Ingredients:", ingredients)

    # We import existing backbones and their pretrained weights from torchvision.
    from torchvision.models import resnet50, resnet18, resnet34, resnet101, resnet152, vit_b_16, vit_b_32, vit_l_16, vit_l_32, vit_h_14, inception_v3
    from torchvision.models import ResNet50_Weights, ResNet18_Weights, ResNet34_Weights, ResNet101_Weights, ResNet152_Weights, Inception_V3_Weights
    from torchvision.models import ViT_B_32_Weights, ViT_B_16_Weights, ViT_L_32_Weights, ViT_L_16_Weights, ViT_H_14_Weights

    # For each model, we capture
    # - its "function", i.e., the definition of the neural network;
    # - feature size, i.e., how many features are extracted by the network;
    # - the transforms used for the pretraining (which tell us how to process new images);
    # - the expected image size, i.e., which image size was used during evaluation of the pretrained models;
    # - and finally a specific name for the pretrained weights.
    def get_model_weights_size_transforms(model_name):
        model_fn, ft_size, model_transforms, img_size, pt_name = {
            'IncV3': [inception_v3, 2048, Inception_V3_Weights.DEFAULT.transforms(), 299, 'IMAGENET1K_V1'],
            'R18_IN1k': [resnet18, 512, ResNet18_Weights.DEFAULT.transforms(), -1, 'IMAGENET1K_V1'], 
            'R34_IN1k': [resnet34, 512, ResNet34_Weights.DEFAULT.transforms(), -1, 'IMAGENET1K_V1'],
            'R50_IN1k': [resnet50, 2048, ResNet50_Weights.DEFAULT.transforms(), -1, 'IMAGENET1K_V2'],  # v2 has higher performance on ImageNet
            'R101_IN1k': [resnet101, 2048, ResNet101_Weights.DEFAULT.transforms(), -1, 'IMAGENET1K_V2'],
            'R152_IN1k': [resnet152, 2048, ResNet152_Weights.DEFAULT.transforms(), -1, 'IMAGENET1K_V2'],
            'ViT-B-16_IN1k': [vit_b_16, 768, ViT_B_16_Weights.IMAGENET1K_SWAG_E2E_V1.transforms(), 384, 'IMAGENET1K_SWAG_E2E_V1'],  # swag has better performance, but much higher gflops
            'ViT-B-32_IN1k': [vit_b_32, 768, ViT_B_32_Weights.DEFAULT.transforms(), 224, 'IMAGENET1K_V1'],
            'ViT-L-16_IN1k': [vit_l_16, 1024, ViT_L_16_Weights.DEFAULT.transforms(), 512, 'IMAGENET1K_SWAG_E2E_V1'],
            'ViT-L-32_IN1k': [vit_l_32, 1024, ViT_L_32_Weights.DEFAULT.transforms(), 224, 'IMAGENET1K_V1'],
            'ViT-H-14_IN1k': [vit_h_14, 1280, ViT_H_14_Weights.DEFAULT.transforms(), 518, 'IMAGENET1K_SWAG_E2E_V1']
        }[model_name]
        
        return model_fn, ft_size, model_transforms, img_size, pt_name

    import torch.nn as nn
    from torchvision.models.feature_extraction import create_feature_extractor, get_graph_node_names

    model_name = args.arch
    freeze_backbone = args.frozen_backbone
    dedicated_mlp = True

    # FoodInfoPredictionModel defines the network for our use case.
    # It incorporates a backbone, i.e., a neural network dedicated to feature extraction from visual data, 
    # and a head, i.e., a network-based component dedicated to problem solving.
    # To instantiate the class, we provide:
    # - information related to the backbone (e.g., the model function, expected image size, name, etc),
    # - freeze_backbone: (boolean) keep the backbone weights frozen (pretrained, unchanged during training) or not,
    #                    in our case: =True for Vision Transformer, =False otherwise.
    # - dedicated_mlp: (boolean) to use a dedicated Multilayer Perceptron for problem solving.
    #                  =False uses a single layer to jointly estimate energy, mass, etc. Tested in early experiments, suboptimal.
    #                  In our case, we use =True.
    # - deeper_MT_mlp: (boolean) to use a "deeper network" (2 linear layers, interleaved with ReLU non linearity)
    #                  for the private, per-task (energy estimation, mass est., etc) problem solving component, or
    #                  to use a "shallower" version (1 layer-only).
    #                  In this work, we use =True. See Bianco et al, Nutrients 2025 for a comparison with =False.
    # - use_micro_nutrients: (boolean) to use micronutrients during training. Currently untested. Defaults to False.
    class FoodInfoPredictionModel(nn.Module):
        
        def __init__(self, model_fn, ft_size, pt_name,
                     img_size=-1, out_dim=5, model_name="R50_IN1k", 
                     freeze_backbone=False, dedicated_mlp=False, deeper_MT_mlp=False, use_micro_nutrients=False):
            super().__init__()
            self.model_name = model_name
            self.freeze_backbone = freeze_backbone
            model = model_fn(weights=pt_name)
            if freeze_backbone:
                for p in model.parameters():
                    p.requires_grad = False
            
            # Pretrained backbones have a final ImageNet-classification head that we ignore here.
            # To do so, we pick a previous node as the "output" node.
            # These have different names for Vision Transformers and ResNet/Inception networks.
            if "ViT" in model_name:
                # print(get_graph_node_names(model))
                self.return_layer_name = "getitem_5"

            else:
                # print(get_graph_node_names(model))
                self.return_layer_name = "flatten"
            self.backbone = create_feature_extractor(model, return_nodes=[self.return_layer_name])

            self.dedicated_mlp = dedicated_mlp
            self.deeper_MT_mlp = deeper_MT_mlp
            self.use_micro_nutrients = use_micro_nutrients
            if not self.dedicated_mlp:
                self.linear = nn.Linear(ft_size, out_dim)
            
            else:
                # In Bianco et al, Nutrients 2025, the following is what was defined as "2+1" or "2+2" network.
                # fc1, fc2 represent the shared 2-layer network that initially processes the features extracted by the backbone.
                # if only fc3 is used, then it's the "2+1" network. If fc3_1 is also included, then fc3_1 and fc3 represent "2+2".
                self.fc1 = nn.Linear(ft_size, ft_size)
                self.relu_fc1 = nn.ReLU()
                self.fc2 = nn.Linear(ft_size, ft_size)
                self.relu_fc2 = nn.ReLU()
                if not deeper_MT_mlp:
                    self.fc3_cal = nn.Linear(ft_size, 1)
                    self.fc3_mass = nn.Linear(ft_size, 1)
                    self.fc3_macro = nn.Linear(ft_size, 3)
                    # sep_cls: whether to use a separate classifier for each ingredient... not really recommended. 
                    # By default, =False in our case.
                    if args.sep_cls:
                        self.fc3_ingr_pred = nn.ModuleList([nn.Linear(ft_size, 1)]*(len(map_ingredients2class)))
                    else:
                        self.fc3_ingr_pred = nn.Linear(ft_size, len(map_ingredients2class))
                    if self.use_micro_nutrients:
                        self.fc3_micro = nn.Linear(ft_size, 83)
                else:
                    self.fc3_1_cal = nn.Linear(ft_size, 128)
                    self.fc3_1_mass = nn.Linear(ft_size, 128)
                    self.fc3_1_macro = nn.Linear(ft_size, 128)
                    self.relu_fc3_cal = nn.ReLU()
                    self.relu_fc3_mass = nn.ReLU()
                    self.relu_fc3_macro = nn.ReLU()
                    self.fc3_cal = nn.Linear(128, 1)
                    self.fc3_mass = nn.Linear(128, 1)
                    self.fc3_macro = nn.Linear(128, 3)
                    if args.sep_cls:
                        self.fc3_1_ingr_pred = nn.Linear(ft_size, 128)
                        self.relu_fc3_ingr_pred = nn.ReLU()
                        self.fc3_ingr_pred = nn.ModuleList([nn.Linear(128, 1)]*(len(map_ingredients2class)))

                    else:
                        self.fc3_1_ingr_pred = nn.Linear(ft_size, 128)
                        self.relu_fc3_ingr_pred = nn.ReLU()
                        self.fc3_ingr_pred = nn.Linear(128, len(map_ingredients2class))
                    
                    if self.use_micro_nutrients:
                        self.fc3_1_micro = nn.Linear(ft_size, 128)
                        self.relu_fc3_micro = nn.ReLU()
                        self.fc3_1_micro = nn.Linear(128, 83)
                
        # forward defines what the network should do after receiving some inputs
        # here, the inputs will be a batch of images, shaped as (B, Ch, H, W), with:
        # B = size of the batch, i.e., number of images to be processed in parallel (e.g., 32 or 64)
        # Ch = number of color channels (3=RGB)
        # H, W = spatial sizes of the images (e.g., 224)
        def forward(self, x):
            # First, the images are processed by the backbone; 
            # then, the features are extracted at a specific layer
            x = self.backbone(x)
            x = x[self.return_layer_name]
            
            # The features are then processed by the network, according to the definition
            if not self.dedicated_mlp:
                preds = self.linear(x)
            
            else:
                x = self.relu_fc1(self.fc1(x))
                x = self.relu_fc2(self.fc2(x))
                if self.deeper_MT_mlp:
                    cal = self.relu_fc3_cal(self.fc3_1_cal(x))
                    mass = self.relu_fc3_mass(self.fc3_1_mass(x))
                    macro = self.relu_fc3_macro(self.fc3_1_macro(x))
                    ingr_pred = self.relu_fc3_ingr_pred(self.fc3_1_ingr_pred(x))
                    cal = self.fc3_cal(cal)
                    mass = self.fc3_mass(mass)
                    macro = self.fc3_macro(macro)
                    if args.sep_cls:
                        ingr_pred = torch.cat([pred(ingr_pred) for pred in self.fc3_ingr_pred], -1)
                    else:
                        ingr_pred = self.fc3_ingr_pred(ingr_pred)
                    if self.use_micro_nutrients:
                        micro = self.relu_fc3_micro(self.fc3_1_micro(x))
                        micro = self.fc3_micro(micro)
                else:
                    cal = self.fc3_cal(x)
                    mass = self.fc3_mass(x)
                    macro = self.fc3_macro(x)
                    if args.sep_cls:
                        ingr_pred = torch.cat([pred(x) for pred in self.fc3_ingr_pred], -1)
                    else:
                        ingr_pred = self.fc3_ingr_pred(x)
                    if self.use_micro_nutrients:
                        micro = self.fc3_micro(x)

                # The predictions are then concatenated
                if not self.use_micro_nutrients:
                    preds = torch.cat((cal, macro, mass, ingr_pred), -1)
                else:
                    preds = torch.cat((cal, macro, mass, ingr_pred, micro), -1)
            
            return preds

    # Here, we use the model_name, defined above based on the command line parameters, to get its function, feature size, etc
    model_fn, ft_size, model_transforms, img_size, pt_name = get_model_weights_size_transforms(model_name)
    model = FoodInfoPredictionModel(model_fn, ft_size, pt_name, img_size=img_size, model_name=model_name, freeze_backbone=freeze_backbone, dedicated_mlp=dedicated_mlp, deeper_MT_mlp=args.deeper_MT_mlp, use_micro_nutrients=args.use_micro_in_training)
    # Note that here we set the transforms of the CustomVideoDataset instances to those of the pretrained model in evaluation mode
    # -> meaning that the __getitem__ function will use the expected transforms
    train_dataset.transform = model_transforms
    val_dataset.transform = model_transforms
    test_dataset.transform = model_transforms

    # The following are just test examples to check that nothing breaks
    if args.test_ensemble_preds:
        input_batch = torch.stack([model_transforms(sf) for sf in sample_frame])
    else:
        input_tensor = model_transforms(sample_frame)
        input_batch = input_tensor.unsqueeze(0)  # create a mini-batch as expected by the model

    # move the input and model to GPU for speed if available
    if torch.cuda.is_available():
        input_batch = input_batch.to('cuda')
        model.to('cuda')

    with torch.no_grad():
        output = model(input_batch)
        print(output.shape)

    import numpy as np
    np.random.seed(420)
    from sklearn.metrics import mean_absolute_error, mean_squared_error, mean_absolute_percentage_error
    from sklearn.metrics import precision_score, recall_score, f1_score

    # calc_metrics computes the metrics (MAE, MAPE, RMSE) for energy, macronutrients, mass predictions
    # Note: MAPE is computed as defined by Nutrition5k
    # (see: https://github.com/google-research-datasets/Nutrition5k/blob/main/scripts/compute_eval_statistics.py)
    def calc_metrics(predictions, ground_truth):
        # Mean Absolute Error (MAE)
        mae = mean_absolute_error(ground_truth, predictions)

        # Mean Absolute Percentage Error (MAPE)
        #mape = mean_absolute_percentage_error(ground_truth, predictions)
        mape = 100 * mae / np.mean(ground_truth)

        # Root Mean Square Error (RMSE)
        rmse = mean_squared_error(ground_truth, predictions, squared=False)

        return rmse, mae, mape

    # calc_class_metrics instead computes the metrics for ingredient predictions
    # If MLML is used, ground_truth values will be a list of indices.
    # Before moving onto computing the metrics, we transform it to a multi-hot.
    def calc_class_metrics(predictions, ground_truth):
        # print(np.unique(predictions), np.unique(ground_truth))
        if args.use_mlml:  #ground_truth is a list of class indices -> for metrics we need a multi-hot vector
            tmp = []
            for i in range(len(ground_truth)):
                _tmp = torch.zeros(ground_truth.shape[-1])
                _ground_truth = ground_truth[i][ground_truth[i] != -1]  # valid class indices
                _tmp[_ground_truth] = 1
                tmp.append(_tmp)
                
            _ground_truth = torch.stack(tmp)

        else:
            _ground_truth = ground_truth
        
        # Following check deals with floating point instability that can lead to 1s varying slightly (e.g., 0.99994)
        if len(np.unique(_ground_truth)) != 2:
            print(np.unique(_ground_truth))
            _ground_truth = _ground_truth > 0.5

        # Then we compute the the main metrics: Precision, Recall, F1-score
        p = precision_score(_ground_truth, predictions, average="micro", zero_division=0.0)
        r = recall_score(_ground_truth, predictions, average="micro")
        f1 = f1_score(_ground_truth, predictions, average="micro")
        return p, r, f1

    # create_dict_metrics structures a dictionary to comprise metrics for all the predictions.
    # It is mostly used for logging/debugging purposes. Actual, final predictions used in the paper are obtained by analyzing
    # the test-time predictions (later computed based on the numpy files with the final predictions).
    # This function is called for training, validation, and testing purposes, so that the logging (within wandb) contains many info.
    def create_dict_metrics(phase, predictions, ground_truth, ingredient_predictions, ingredient_groundtruth):
        step = ['Calories', 'Fat', 'Carbs', 'Proteins', 'Mass']
        loss = mean_squared_error(ground_truth, predictions)
        info_dict = {f'{phase}/Loss': loss.item()}
        # For each of the 5 components, we add a small epsilon (0.0000000001) to avoid 0-divisions.
        for i in range(5):
            rounded_gt = 1e-10 + ground_truth[:, i] #.cpu().detach().numpy()
            rmse, mae, mape = calc_metrics(predictions[:, i], #.cpu().detach().numpy(), 
                                           rounded_gt)  # to avoid 0 errors
        
            info_dict[f'{phase}/{step[i]}/RMSE'] = rmse
            info_dict[f'{phase}/{step[i]}/MAE'] = mae
            info_dict[f'{phase}/{step[i]}/MAPE'] = mape

        # Ingredient predictions arrive here as raw logits (before computing sigmoid). These are transformed into
        # binary predictions (ingredient is there or not) based on a threshold, which we define as 0.5.
        p, r, f1 = calc_class_metrics(ingredient_predictions>args.acc_threshold_classif_ingr, ingredient_groundtruth)
        info_dict[f'{phase}/Ingredients/P'] = p
        info_dict[f'{phase}/Ingredients/R'] = r
        info_dict[f'{phase}/Ingredients/F1'] = f1

        return info_dict
       
    # Here some hyperparameters are fixed because we do not really optimize them.
    # Learning rate is set to 1e-3 for the optimizer. Batches have either 32 (ViT) or 64 (ResNet, IncV3) elements.
    learning_rate = 0.001
    batch_size = 64 if "ViT" not in model_name else 32
    epochs = 100
    verbose = False

    import datetime
    suffix = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")

    # WandB (weights and biases) is a popular logging tool. We initialize it here with some info on the run.
    # The tool automatically logs info on the running environment, system usage, etc.
    # More on https://wandb.ai
    if not args.no_wandb:
        try:
            import wandb
            
            # start a new wandb run to track this script
            wandb.init(
                # set the wandb project where this run will be logged
                project="Nutrition5k",

                # track hyperparameters and run metadata
                config={
                    "learning_rate": learning_rate,
                    "batch_size": batch_size,
                    "architecture": model.model_name,
                    "epochs": epochs,
                    "freeze_backbone": model.freeze_backbone,
                    "labels_normalized": False,
                    "model_transforms": model_transforms, 
                    "pt_name": pt_name,
                    "mass_in_loss": True,
                    "multitaskloss": True,
                    "dedicated_mlp": dedicated_mlp,
                    "deeper_MT_mlp": args.deeper_MT_mlp,
                    "setup": args.setup,
                    "ingredients_num": len(map_ingredients2class),
                    "ingredients_cls": map_ingredients2class,
                    "classif_threshold": args.acc_threshold_classif_ingr,
                    "is_loss_weighted": args.weighted_loss,
                    "lambda_ingr": args.lambda_ingr,
                    "use_ASL": args.use_ASL,
                    "ASL-gamma-neg": args.ASL_gamma_neg,
                    "ASL-gamma-pos": args.ASL_gamma_pos,
                    "use_FOCAL": args.use_focal,
                    "Focal-gamma": args.focal_gamma,
                    "Focal-alpha": args.focal_alpha,
                    "separate_classifiers": args.sep_cls,
                    "is_MLML": args.use_mlml,
                }
            )
            use_wandb = True

        except:
            use_wandb = False
            print("No W&B")
    else:
        use_wandb = False

    # Here we create the dataloaders. These are used to load data from the disk, while speeding it up through multithreads.
    # Important parameters:
    # - the dataset instance to be used (so that train loader only loads training data, etc)
    # - the batch size, i.e., how many elements it should load
    # - whether to shuffle the loading order: we use =True only for training,
    #   so that batches are "always different", allowing for more variability in the inputs to the model.
    #   Note that as we fixed all the seeds at the beginning, the order is reproducible.
    # - num_workers, i.e., how many data loading processes to use. In our setup we use 32, but lower numbers can be used (e.g., 4/8)
    # - collate_fn we specify to use our custom collate defined above
    # - worker_init_fn and generator need to be passed to those defined at the beginning, so we guarantee reproducibility
    train_loader = torch.utils.data.DataLoader(train_dataset, batch_size=batch_size,
                                               shuffle=True, num_workers=32, collate_fn=collate_fn,
                                               worker_init_fn=seed_worker, generator=g)
    val_loader = torch.utils.data.DataLoader(val_dataset, batch_size=batch_size,
                                               shuffle=False, num_workers=32, collate_fn=collate_fn,
                                               worker_init_fn=seed_worker, generator=g)
    
    # For the test loader, if we ensemble predictions, we set batch_size=1.
    # This does *not* mean we only observe 1 frame at a time. 
    # It means that we call the __getitem__ for one recipe and that's the batch for this iteration.
    # However, in the __getitem__ we specify that, if ensembling predictions, all frames for that recipe are to be loaded.
    # So here we have variable-length batches, since different recipes have different number of frames.
    if not args.test_ensemble_preds:
        test_loader = torch.utils.data.DataLoader(test_dataset, batch_size=batch_size,
                                                  shuffle=False, num_workers=32, collate_fn=collate_fn,
                                                  worker_init_fn=seed_worker, generator=g)

    else:
        test_loader = torch.utils.data.DataLoader(test_dataset, batch_size=1,
                                                  shuffle=False, num_workers=32, collate_fn=collate_fn,
                                                  worker_init_fn=seed_worker, generator=g)

    import torch.optim as optim

    # We are not scaling data, so the following is a placeholder.
    scaler.inverse_transform = lambda x: x

    # If a weighted loss is used, we compute weights here. We tested here a possibility derived from another multilabel scenario.
    # https://www.kaggle.com/code/harpdeci/multi-label-classification-plant-pathology
    if args.weighted_loss:
        class_counts = torch.zeros(len(map_ingredients2class))
        for ing_dict in train_dataset.ingredient_annotation:
            class_counts += encode_ingredient_names(ing_dict)
        cls_weights = (class_counts.sum() - class_counts) / class_counts.clamp(min=1)  # "negatives" / positives per each class
        # print("="*20)
        # print(class_counts)
        cls_weights = torch.reciprocal(class_counts.float())
        cls_weights[torch.isinf(cls_weights)] = 0.0  # mask ingredients not appearing in training
        # print(cls_weights)
        cls_weights /= torch.max(cls_weights)
        # print(cls_weights)
        # exit()

    ingr_loss_used_suffix = ""
    if args.use_ASL:
        ingr_loss_used_suffix = f"_ASL_gammaN{args.ASL_gamma_neg}P{args.ASL_gamma_pos}"
    elif args.use_focal:
        ingr_loss_used_suffix = "_focal"
    elif args.weighted_loss:
        ingr_loss_used_suffix = "_Wgt"
    elif args.use_mlml:
        ingr_loss_used_suffix = "_MLML"
    
    import torch.nn.functional as F
    # Here we define a function for the Focal loss. We also tested it in our scenario, but found ASL to be generally better.
    # https://discuss.pytorch.org/t/using-focal-loss-for-multilabel-classification-problem/206839/4
    def focal_loss(inputs, targets, alpha=args.focal_alpha, gamma=args.focal_gamma):
        # BCE_loss = F.binary_cross_entropy_with_logits(inputs, targets, reduction='none')
        BCE_loss = nn.BCEWithLogitsLoss(reduction='none')(inputs, targets)
        pt = torch.exp(-BCE_loss)
        F_loss = alpha * (1-pt)**gamma * BCE_loss
        return F_loss.mean()
    
    # The following class, MultiTaskLoss, defines a single module to compute and combine the loss functions used in our approach.
    # It includes a MAE/L1 loss for energy, fats, carbohydrates, proteins, mass as in Nutrition5k.
    # Then, it also includes a component for ingredient detection. 
    # This is either of ASL, Focal, Weighted Binary Cross Entropy (BCE), MLML, or BCE.
    class MultiTaskLoss(nn.Module):
        def __init__(self, mass=False, micro=False):
            super().__init__()
            self.crit_cal = nn.L1Loss()
            self.crit_fat = nn.L1Loss()
            self.crit_carb = nn.L1Loss()
            self.crit_prot = nn.L1Loss()
            if args.use_ASL:
                from ASL import AsymmetricLoss, AsymmetricLossOptimized
                self.crit_ingr = AsymmetricLossOptimized(gamma_neg=args.ASL_gamma_neg, gamma_pos=args.ASL_gamma_pos)
            elif args.use_focal:
                # from torchvision.ops import sigmoid_focal_loss
                # from functools import partial
                # self.crit_ingr = partial(sigmoid_focal_loss, alpha=args.focal_alpha, gamma=args.focal_gamma)
                self.crit_ingr = focal_loss
            elif args.weighted_loss:
                self.crit_ingr = nn.BCEWithLogitsLoss(reduction='mean', pos_weight=cls_weights.cuda())
            elif args.use_mlml:
                self.crit_ingr = nn.MultiLabelMarginLoss(reduction='mean')
            # elif args.mlsml:
            #     self.crit_ingr = nn.MultiLabelSoftMarginLoss(reduction='mean')
            else:
                self.crit_ingr = nn.BCEWithLogitsLoss(reduction='mean')
            if mass:
                self.crit_mass = nn.L1Loss(reduction='mean')
            else:
                self.crit_mass = None
            
            if micro:
                self.crit_micro = nn.L1Loss(reduction='mean')
            else:
                self.crit_micro = None
            
        # We receive here
        # - x, composed of all the predictions: 'Cal', 'Fat', 'Carb', 'Prot', 'Mass', 'Ingredients'
        # - y, ground truth values of energy, macronutrients, and mass
        # - ingr_y, ground truth for the ingredients
        def forward(self, x, y, ingr_y): 
            loss_cal = self.crit_cal(x[:, 0], y[:, 0])
            loss_fat = self.crit_fat(x[:, 1], y[:, 1])
            loss_carb = self.crit_carb(x[:, 2], y[:, 2])
            loss_prot = self.crit_prot(x[:, 3], y[:, 3])
            loss_ingr = self.crit_ingr(x[:, 5:], ingr_y)
            assert ingr_y.shape == x[:, 5:].shape
            # print("LOSS INGR SIZE:", loss_ingr.shape)
            
            # Here we sum the loss computed for energy, avg of macronutrients' losses, and ingredients loss.
            # We have lambda_ingr (default=1.) to eventually search for better integration value of the ingredients loss.
            # The divider is 4 if all components are used (general scenario for this paper).
            # It is 3 if either mass or ingredients are removed, 2 if both are removed.
            cumulative_loss = loss_cal + (loss_fat + loss_carb + loss_prot) / 3 + args.lambda_ingr * loss_ingr
            div = 3 if args.lambda_ingr > 0. else 2
            if self.crit_mass is not None:
                loss_mass = self.crit_mass(x[:, 4], y[:, 4])
                cumulative_loss += loss_mass
                div += 1
            # Micronutrients are ignored right now.
            if self.crit_micro is not None:
                assert False
                loss_micro = self.crit_micro(x[:, 5:], y[:, 5:])
                cumulative_loss += loss_micro
                div += x[:, 5:].shape[-1]
            
            return cumulative_loss / div

    criterion = MultiTaskLoss(mass=True, micro=args.use_micro_in_training)
    # We use Adam with default hyperparameters (apart from learning rate) for optimization.
    optimizer = optim.Adam(model.parameters(), lr=learning_rate)

    times__preproc = time.time() - times__preproc
    times__epoch_trn_tmp = []
    times__epoch_val_tmp = []
    
    best_val_epoch = -1
    best_val_loss = None
    for epoch in range(epochs):
        times__epoch_trn_tmp.append(time.time())
        
        model.train()
        for batch_idx, (data, labels, ingredient_classes) in enumerate(train_loader):
            # We load all data in GPU
            data = data.cuda()
            labels = labels.cuda()
            ingredient_classes = ingredient_classes.cuda()
            if not args.use_micro_in_training:  # if NO micronutrients are used, labels will contain unneeded stuff
                labels = labels[:, :5]
            
            optimizer.zero_grad()  # Zero the gradients
            outputs = model(data)  # Forward pass
            loss = criterion(outputs, labels, ingredient_classes)  # Compute the loss
            loss.backward()  # Backpropagation
            optimizer.step()  # Update the model's weights
            
            # If wandb logging is activated, here we computed the metrics for this batch and pass everything to wandb.
            if use_wandb:
                wandb.log(create_dict_metrics('Train', 
                                              scaler.inverse_transform(outputs[:, :5].cpu().detach().numpy()), 
                                              scaler.inverse_transform(labels.cpu().detach().numpy()),
                                              scaler.inverse_transform(outputs[:, 5:].cpu().detach().numpy()), 
                                              ingredient_classes.cpu().detach().numpy()
                                             ))

        times__epoch_trn_tmp[-1] = time.time() - times__epoch_trn_tmp[-1]
        times__epoch_val_tmp.append(time.time())
        
        # After all batches are processed, we switch to eval mode.
        model.eval()
        cumulated_output = None
        cumulated_labels = None
        cumulated_ingr_out = None
        cumulated_ingr_label = None
        # We don't compute gradients here.
        with torch.no_grad():
            for batch_idx, (data, labels, ingredient_classes) in enumerate(val_loader):
                data = data.cuda()
                labels = labels.cuda()
                ingredient_classes = ingredient_classes.cuda()
                if not args.use_micro_in_training:  # if NO micronutrients are used, labels will contain unneeded stuff
                    labels = labels[:, :5]

                outputs = model(data)  # Forward pass
                loss = criterion(outputs, labels, ingredient_classes)  # Compute the loss

                # Main difference compared to training: here we cumulate predictions and groundtruth across the validation batches,
                # so that we compute metrics on all the validation data together. Something similar is done later for testing.
                if cumulated_output is None:
                    cumulated_output = outputs[:, :5]
                    cumulated_labels = labels
                    cumulated_ingr_out = outputs[:, 5:]
                    cumulated_ingr_label = ingredient_classes
                else:
                    cumulated_output = torch.cat((cumulated_output, outputs[:, :5]), 0)
                    cumulated_labels = torch.cat((cumulated_labels, labels), 0)
                    cumulated_ingr_out = torch.cat((cumulated_ingr_out, outputs[:, 5:]), 0)
                    cumulated_ingr_label = torch.cat((cumulated_ingr_label, ingredient_classes), 0)

            # Wandb logging for validation, if activated
            if use_wandb:
                wandb.log(create_dict_metrics('Val', scaler.inverse_transform(cumulated_output.cpu().detach().numpy()),
                                              scaler.inverse_transform(cumulated_labels.cpu().detach().numpy()),
                                              cumulated_ingr_out.cpu().detach().numpy(),
                                              cumulated_ingr_label.cpu().detach().numpy(),
                                             ))

            # If this is the first epoch and best_val_loss is None, then we save the current network weights and loss values as the "best".
            if best_val_loss is None:
                best_val_loss = mean_squared_error(cumulated_output.cpu().detach().numpy(), cumulated_labels.cpu().detach().numpy())
                best_val_loss += mean_squared_error(cumulated_ingr_out.cpu().detach().numpy(), cumulated_ingr_label.cpu().detach().numpy())
                best_val_epoch = epoch
                torch.save(model.state_dict(), f"checkpoint_{suffix}_{args.setup}{ingr_loss_used_suffix}{args.lambda_ingr}{'_sepCls' if args.sep_cls else ''}_{args.arch}.pth")

            # Otherwise, if it is improving, we update them.
            else:
                val_loss = mean_squared_error(cumulated_output.cpu().detach().numpy(), cumulated_labels.cpu().detach().numpy())
                val_loss += mean_squared_error(cumulated_ingr_out.cpu().detach().numpy(), cumulated_ingr_label.cpu().detach().numpy())
                if val_loss < best_val_loss:
                    best_val_loss = val_loss
                    best_val_epoch = epoch
                    torch.save(model.state_dict(), f"checkpoint_{suffix}_{args.setup}{ingr_loss_used_suffix}{args.lambda_ingr}{'_sepCls' if args.sep_cls else ''}_{args.arch}.pth")
        
        times__epoch_val_tmp[-1] = time.time() - times__epoch_val_tmp[-1]

    times__epoch_val = np.mean(times__epoch_val_tmp)
    times__epoch_trn = np.mean(times__epoch_trn_tmp)
    times__val = np.sum(times__epoch_val_tmp)
    times__training = time.time() - times__training
    times__train_only = times__training - times__val
    times__test = time.time()
    
    # Once we reach this point, we trained the network for 100 epochs, and found a best model relying on the validation data.
    # Here we load the best model found throughout the procedure, and start the testing phase.
    model.load_state_dict(torch.load(f"checkpoint_{suffix}_{args.setup}{ingr_loss_used_suffix}{args.lambda_ingr}{'_sepCls' if args.sep_cls else ''}_{args.arch}.pth"))
    model.eval()
    cumulated_output, cumulated_output_median = None, None
    cumulated_labels, cumulated_labels_median = None, None
    cumulated_ingr_out, cumulated_ingr_out_median = None, None
    cumulated_ingr_label, cumulated_ingr_label_median = None, None
    with torch.no_grad():
        for batch_idx, (data, labels, ingredient_classes) in enumerate(test_loader):
            if "ViT" in model.model_name and args.test_ensemble_preds:  # may need to subsample for ViT if too many frames are there (this happens in few cases)
                new_ixs = np.linspace(0, len(data)-1, num=args.subsample_test_frames)
                data = data[new_ixs]
                labels = labels[new_ixs]
                ingredient_classes = ingredient_classes[new_ixs]
            data = data.cuda()
            labels = labels.cuda()
            ingredient_classes = ingredient_classes.cuda()
            if not args.use_micro_in_training:  # if NO micronutrients are used, labels will contain unneeded stuff
                labels = labels[:, :5]

            outputs = model(data)  # Forward pass
            loss = criterion(outputs, labels, ingredient_classes)  # Compute the loss

            # if we want to ensemble test-time predictions, here we receive a batch composed of all the frames for a single video.
            # so here we want to aggregate them (mean, and median) and cumulate them
            if args.test_ensemble_preds:
                if cumulated_output is None:
                    cumulated_output = outputs[:, :5].mean(0).unsqueeze(0)
                    cumulated_labels = labels.mean(0).unsqueeze(0)
                    cumulated_ingr_out = outputs[:, 5:].mean(0).unsqueeze(0)
                    cumulated_ingr_label = ingredient_classes.mean(0).unsqueeze(0)
                else:
                    cumulated_output = torch.cat((cumulated_output, outputs[:, :5].mean(0).unsqueeze(0)), 0)
                    cumulated_labels = torch.cat((cumulated_labels, labels.mean(0).unsqueeze(0)), 0)
                    cumulated_ingr_out = torch.cat((cumulated_ingr_out, outputs[:, 5:].mean(0).unsqueeze(0)), 0)
                    cumulated_ingr_label = torch.cat((cumulated_ingr_label, ingredient_classes.mean(0).unsqueeze(0)), 0)

                if cumulated_output_median is None:
                    cumulated_output_median = outputs[:, :5].cpu().median(0)[0].unsqueeze(0)
                    cumulated_labels_median = labels.cpu().median(0)[0].unsqueeze(0)
                    cumulated_ingr_out_median = outputs[:, 5:].cpu().median(0)[0].unsqueeze(0)
                    cumulated_ingr_label_median = ingredient_classes.cpu().median(0)[0].unsqueeze(0)
                else:
                    cumulated_output_median = torch.cat((cumulated_output_median, outputs[:, :5].cpu().median(0)[0].unsqueeze(0)), 0)
                    cumulated_labels_median = torch.cat((cumulated_labels_median, labels.cpu().median(0)[0].unsqueeze(0)), 0)
                    cumulated_ingr_out_median = torch.cat((cumulated_ingr_out_median, outputs[:, 5:].cpu().median(0)[0].unsqueeze(0)), 0)
                    cumulated_ingr_label_median = torch.cat((cumulated_ingr_label_median, ingredient_classes.cpu().median(0)[0].unsqueeze(0)), 0)

            else:
                if cumulated_output is None:
                    cumulated_output = outputs[:, :5]
                    cumulated_labels = labels
                    cumulated_ingr_out = outputs[:, 5:]
                    cumulated_ingr_label = ingredient_classes
                else:
                    cumulated_output = torch.cat((cumulated_output, outputs), 0)
                    cumulated_labels = torch.cat((cumulated_labels, labels), 0)
                    cumulated_ingr_out = torch.cat((cumulated_ingr_out, outputs[:, 5:]), 0)
                    cumulated_ingr_label = torch.cat((cumulated_ingr_label, ingredient_classes), 0)

        print(cumulated_output.shape, cumulated_labels.shape, cumulated_ingr_out.shape, cumulated_ingr_label.shape)
        if use_wandb:
            wandb.log(create_dict_metrics('Test', scaler.inverse_transform(cumulated_output.cpu().detach().numpy()),
                                          scaler.inverse_transform(cumulated_labels.cpu().detach().numpy()),
                                          cumulated_ingr_out.cpu().detach().numpy(),
                                          cumulated_ingr_label.cpu().detach().numpy(),
                                         ))

        # Here, we log the predictions for this run (both computed with mean and median) on disk.
        # These numpy files are then used to compute the .csv files for subsequent processing.
        if args.log_predictions:
            file_name_save = f"predictions_TrainWithIngredients{ingr_loss_used_suffix}{args.lambda_ingr}_micro{'TrnTst' if args.use_micro_in_training else 'TstOnly'}_{model.model_name}_run{run_ix}{'_MLPperTask' if dedicated_mlp else ''}{'_deepMLP' if args.deeper_MT_mlp else ''}{'_TestEns' if args.test_ensemble_preds else ''}{'_sepCls' if args.sep_cls else ''}_{args.setup}.npy"
            np.save(file_name_save, np.concatenate((cumulated_output.cpu().detach().numpy(), cumulated_labels.cpu().detach().numpy()), 1))
            np.save(file_name_save.replace("predictions", "predictions_MEDIAN"), np.concatenate((cumulated_output_median.cpu().detach().numpy(), cumulated_labels_median.cpu().detach().numpy()), 1))
            
            file_name_save2 = f"predictions_Ingredients{ingr_loss_used_suffix}{args.lambda_ingr}_micro{'TrnTst' if args.use_micro_in_training else 'TstOnly'}_{model.model_name}_run{run_ix}{'_MLPperTask' if dedicated_mlp else ''}{'_deepMLP' if args.deeper_MT_mlp else ''}{'_TestEns' if args.test_ensemble_preds else ''}{'_sepCls' if args.sep_cls else ''}_{args.setup}.npy"
            np.save(file_name_save2, np.concatenate((cumulated_ingr_out.cpu().detach().numpy(), cumulated_ingr_label.cpu().detach().numpy()), 1))
            np.save(file_name_save2.replace("predictions", "predictions_MEDIAN"), np.concatenate((cumulated_ingr_out_median.cpu().detach().numpy(), cumulated_ingr_label_median.cpu().detach().numpy()), 1))
            
            print(f"completed run {run_ix+1}, saved {file_name_save} and {file_name_save2}")
            print(create_dict_metrics('Test', scaler.inverse_transform(cumulated_output.cpu().detach().numpy()),
                                          scaler.inverse_transform(cumulated_labels.cpu().detach().numpy()),
                                          cumulated_ingr_out.cpu().detach().numpy(),
                                          cumulated_ingr_label.cpu().detach().numpy(),
                                         ))

    # We also compute some time-related metrics that are finally saved on disk.
    times__test = time.time() - times__test
    time_df.append({
        "model": f"{args.arch}_{'2+2' if args.deeper_MT_mlp else '2+1'}",
        "dataset": args.setup,
        "run_index": run_ix,
        "train-val_time": times__training,
        "train_time": times__train_only,
        "val_time": times__val,
        "test_time": times__test,
        "preproc_time": times__preproc,
        "epoch-train_time_avg": times__epoch_trn,
        "epoch-val_time_avg": times__epoch_val,
    })

    if use_wandb:
        wandb.finish()

time_df = pd.DataFrame(time_df)
time_df.to_csv(f"times_df_withIngredients_micro{'TrnTst' if args.use_micro_in_training else 'TstOnly'}_{model.model_name}_run{run_ix}{'_MLPperTask' if dedicated_mlp else ''}{'_deepMLP' if args.deeper_MT_mlp else ''}{'_TestEns' if args.test_ensemble_preds else ''}_{args.setup}.csv", index=False)
