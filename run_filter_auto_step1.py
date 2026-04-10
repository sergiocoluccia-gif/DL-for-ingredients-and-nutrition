import requests

import torch
from PIL import Image
# From the transformers library, we load the AutoProcessor and AutoModelForZeroShotObjectDetection classes, 
# which will allow us to process images and perform zero-shot object detection using a pre-trained model.
from transformers import AutoProcessor, AutoModelForZeroShotObjectDetection

import os
os.environ["TOKENIZERS_PARALLELISM"] = "false"

# We specify the model ID for the pre-trained model we want to use, 
# which in this case is "IDEA-Research/grounding-dino-tiny". 
# Last update May 12, 2024 (as of April 1, 2026).
# The SHA for this model is a2bb814dd30d776dcf7e30523b00659f4f141c71
# You can check that it is the same using the following three lines of code:
# from huggingface_hub import model_info
# info = model_info("IDEA-Research/grounding-dino-tiny")
# print(info.sha)
model_id = "IDEA-Research/grounding-dino-tiny"
device = "cuda"

processor = AutoProcessor.from_pretrained(model_id)
model = AutoModelForZeroShotObjectDetection.from_pretrained(model_id).to(device)

# We create a new directory to store the filtered images, if it doesn't already exist.
os.makedirs("nutrition5k_dataset/imagery/side_angles_FILTERED", exist_ok=True)

df_del_info = []
from tqdm import tqdm
for recipe_id in os.listdir("nutrition5k_dataset/imagery/side_angles/"):
    # We create a subdirectory for each recipe ID in the filtered directory.
    os.makedirs(f"nutrition5k_dataset/imagery/side_angles_FILTERED/{recipe_id}/frames_sampled5", exist_ok=True)
    if "dish_" in recipe_id:
        # We list all the images in the current recipe's directory, filtering out any subdirectories.
        all_images = os.listdir(f"nutrition5k_dataset/imagery/side_angles/{recipe_id}/frames_sampled5/")
        all_images = [a for a in all_images if not os.path.isdir(f"nutrition5k_dataset/imagery/side_angles/{recipe_id}/frames_sampled5/{a}")]
        if len(all_images) == 0:
            continue
        # We initialize a counter to keep track of how many images are dropped during the filtering process of this recipe.
        count_dropped = 0
        for image_url in tqdm(all_images):
            # We load the image
            image = Image.open(f"nutrition5k_dataset/imagery/side_angles/{recipe_id}/frames_sampled5/{image_url}")
            # Using the dot notation, we create a string of text labels that we want the model to detect in the images.
            text_labels = ".".join([
                "hand",
                "person",
                "plates",
                "chair",
                "shoes",
                "floor",
            ]) + "." # e.g. this becomes "hand.person.plates.[etc]."
            
            # We use the processor to prepare the image and text labels for input into the model, 
            # and we move the resulting tensors to the specified device (e.g., GPU).
            inputs = processor(images=image, text=text_labels, return_tensors="pt").to(device)
            with torch.no_grad():
                outputs = model(**inputs)

            # We post-process the model's outputs to extract the detected objects, 
            # their confidence scores, and their bounding boxes.            
            results = processor.post_process_grounded_object_detection(
                outputs,
                inputs.input_ids,
                box_threshold=0.4,
                text_threshold=0.3,
                target_sizes=[image.size[::-1]]
            )
            
            result = results[0]
            drop_image = False
            count_plates = 0
            # We iterate through the detected objects, checking their labels.
            for box, score, labels in zip(result["boxes"], result["scores"], result["labels"]):
                box = [round(x, 2) for x in box.tolist()]
                # print(f"Detected {labels} with confidence {round(score.item(), 3)} at location {box}")

                # If the detected label is "plates", we increment the count of plates.
                # This will allow us to ensure that there is exactly one plate in the image.
                if labels == "plates":
                    count_plates += 1
    
                # If the detected label is not "plates" and is present in the text labels we defined earlier, 
                # we mark the image for dropping.
                if labels in [l for l in text_labels.split(".") if l != "plates"]:
                    drop_image = True
    
            # If there is not exactly one plate detected in the image, we also mark the image for dropping.
            if count_plates != 1:
                drop_image = True
    
            # If the image is marked for dropping, we increment the count of dropped images for this recipe.
            if drop_image:
                count_dropped += 1

            # If the image is not marked for dropping, we save it to the filtered directory for this recipe.
            if not drop_image:
                image.save(open(f"nutrition5k_dataset/imagery/side_angles_FILTERED/{recipe_id}/frames_sampled5/{image_url}", "w"))

        # After processing all images for the current recipe, 
        # we append information about the number of original frames and dropped frames to the df_del_info list.
        df_del_info.append({
            "recipe_id": recipe_id,
            "original_frames": len(all_images),
            "dropped_frames": count_dropped
        })
        print(f"dropped {count_dropped} on {len(all_images)} ({count_dropped/len(all_images)*100:.1f})")

import pandas as pd
# Finally, we convert the df_del_info list into a DataFrame and save it as a CSV file for further analysis.
pd.DataFrame(df_del_info).to_csv("frames_filtered_info_2.csv")

