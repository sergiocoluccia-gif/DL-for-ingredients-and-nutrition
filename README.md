# DL-for-food-images

## Data

We obtain the training data from [Nutrition5k](https://github.com/google-research-datasets/Nutrition5k). We first download all the videos (see their "Download Data" section), then use the provided script "extract_frames_sampled.sh" to extract every fifth frame ("sh extract_frames_sampled.sh 5"). Precise instructions are available in the repository.

For the automatically filtered frames, we rely on [GroundingDINO](https://arxiv.org/abs/2303.05499). The script "run_filter_auto_step1.py" leverages Huggingface to load the model created by the authors and easily setup an object detection pipeline. This will create a new folder with only the kept frames.

The folder metadata/Training_Nutrition5k contains:
* The metadata files from Nutrition5k, both in their original version and the corrected version;
* The metadata files adapted using the Italian FCDB, both in the uncorrected and the corrected version.

At the end, our folder looks like the following:

```
[current folder]
nutrition5k_dataset
  ↳ dish_ids
      ↳ splits
        dish_ids_all.txt
        ...
    imagery
      ↳ side_angles
        side_angles_FILTERED  [if you have already ran the filtering script]
        ...
metadata
  ↳ Training_Nutrition5k
      ↳ usa_corr
        ita_corr
        ...
```

### Environment

The environment used for running the experiments is based on Python 3.8 and contains several packages, such as torch 2.1.2, torchvision 0.16.2, pillow 10.2.0, and opencv-python 4.9.0.80. Full list of requirements is available in requirements.txt. We created a virtual environment, and then installed everything there:

```
python3.8 -m venv env
source env/bin/activate
pip install -r requirements.txt
```

For the extraction of the frames, a different environment was created, mostly for newer versions of already installed packages (e.g., torch, torchvision) required by GroundingDINO. The packages we have in this different environment are available in requirements_thvis16.txt.

## Training

To perform the training of a network, the following command was executed:

```CUBLAS_WORKSPACE_CONFIG=:4096:8 python run_train.py  -a ARCH  -r 5 --setup SETUP --test-ensemble-preds --log-predictions --use-ASL --ASL-gamma-neg 5 --ASL-gamma-pos 0 --deeper-MT-mlp```

This will train the model, using the backbone ARCH (which should be set to either of: IncV3, R101_IN1k, ViT-B-16_IN1k) and the 2+2 head, for 5 times on the scenario SETUP (which should be set to either of: usa_corr, ita_corr, usa_corr_filt1, ita_corr_filt1). WandB is used for logging real-time statistics of the training (e.g., losses, metrics, etc on training and validation splits), but it can be deactivated by adding ```--no-wandb```. The code uses ASL for the ingredient detection loss with gamma_neg 5 and gamma_pos 0. The code also performs testing (ensembling predictions across the available frames, both using mean and median) and logs the final predictions on several numpy files. We provide all these files, that were obtained by running 5 times all the considered ARCH-SETUP combinations, in the folder PREDICTIONS. Note that these files are then used in the notebook ```compute_CSVs.ipynb``` to compute the .csv files used for subsequent analyses. The .csv files are also made readily available in the folder PRECOMPUTED_CSVs.

Inference code is also provided in ```run_inference_filtered.py```. The same command line parameters should be used. This assumes that the trained checkpoints are already available. We provide pretrained checkpoints (5 runs, across the different scenarios, all the networks) in the folder CHECKPOINTS.

Important note: to fully reproduce the results that we obtained, training/inference should be performed with the same type of GPUs that we used. Specifically, we used NVIDIA A100 for IncV3 (across all scenarios), R101 (usa_corr_filt1 and ita_corr_filt1), and ViT-B-16 (usa_corr_filt1 and ita_corr_filt1), whereas for R101 (usa_corr and ita_corr) and ViT-B-16 (usa_corr and ita_corr) we used NVIDIA L40s. When running the inference code with different GPUs (e.g., using L40s with the IncV3 that we trained on A100), we observed slight variations in the per-recipe aggregated predictions (e.g., differences measuring 1e-2 to 1e-3 in total calories). If you are using a machine with multiple GPUs, you can specify which to use by prepending
```CUDA_VISIBLE_DEVICES=XXX``` to the previous commands (XXX=identifier of the GPU as in ```nvidia-smi```). Our hardware setup also uses 2 AMD EPYC 7643 48-Core CPUs.
