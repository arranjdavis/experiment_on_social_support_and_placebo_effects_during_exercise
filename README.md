This repository contains all the scripts and data needed to reproduce the analyses presented in the manuscript.

To replicate the analyses, please follow these steps:

  1. Create a directory containing the `analysis_code` and `data` directories, and their contents (i.e., the scripts and sub-directories they contain), and an _empty_ `outputs` directory.

  2. Use RStudio to run the `combine_experimental_data.R` script from the `analysis_code` directory. This script requires the data in the `../data/matlab_results/` and `../data/qualtrics_data_processed/` sub-directories. The script combines these datasets, drops participants with unusable data, and outputs the dataset `total_combined_data.csv` to the `data` directory. The `total_combined_data.csv` dataset is used in the main experimental analyses.
  
  3. Again using RStudio, run the `main_experimental_analyses.R` script from the `analysis_code` directory. This script requires the `total_combined_data.csv` dataset created in Step 2. The script outputs all of the statistics and plots presented in the manuscript and supplementary online materials (SOM), as well as the assumption checks for each model with statistically significant findings. Plots from the manuscript and SOM are outputted to the `../outputs/plots/` directory, and assumption checks (including assumption check plots) are outputted to the `../outputs/assumption_checks/` directory (the assumption check results for each model are in separate sub-directories). 
  
The `outputs` folder in this repository contains all of the outputs produced by running the `main_experimental_analyses.R` script as described in Step 3. The `matlab_code` directory contains the MATLAB script used to both present the handgrip exercise stimuli and to measure handgrip exercise outputs and experiences during the experiment.
