# Social contact patterns in the United Kingdom following the COVID-19 pandemic: the Reconnect cross-sectional survey

Lucy Goodfellow¹ *†, Billy J. Quilty¹'²†, Kevin van Zandvoort¹‡, W. John Edmunds¹‡

† equal contribution  
‡ equal contribution  
* Corresponding author: lucy.goodfellow@lshtm.ac.uk

¹ Centre for the Mathematical Modelling of Infectious Diseases, London School of Hygiene & Tropical Medicine, London, United Kingdom  
² Charité Centre for Global Health, Charité Universitätsmedizin, Berlin, Germany

## Overview

This repository contains the code and analysis for the Reconnect study, a comprehensive study designed to understand patterns of social interactions across different age groups and settings in the United Kingdom.

This repository reproduces the analyses which can be carried out on the survey datasets, which can be found on Zenodo at [10.5281/zenodo.16845074](https://doi.org/10.5281/zenodo.16845074).

## Survey files

PDF versions of the final surveys can be found in `/survey_files/`. These differ for adults and children, and consist of two parts. In part one, participants completed demographic information about themselves, and in part two, they completed information on their social contacts.

## Data Requirements

To run the analyses, you will need to download the Reconnect survey dataset:

1. Visit the Zenodo repository at: [10.5281/zenodo.16845074](https://doi.org/10.5281/zenodo.16845074)
2. Download the data files
3. Place the downloaded file in a `data/zenodo/` directory of this repository

The repository also requires census data for population weighting (see individual analysis instructions below).

## Project Structure

The key elements of the project are organised as follows:

- `scripts/`: Contains R scripts for data processing and analysis
    - `run_analysis.R`: Run all main analysis 
    - `analyses/`: Contains key scripts for main analysis (see below)
    - `ngm_analysis/`: Contains scripts for Next Generation Matrix (NGM) analysis
- `results/`: Output directory for generated plots and tables
- `data/`: 
    - `survation_dat/`: Raw data directory (not included in the repository)
    - `age_structure_dat/`: ONS 2022 age-, gender-, and ethnicity-specific structure
    - `census_for_ngm/`: other census files used for NGM

## Key Scripts

1. `install_packages.R`: Installs relevant packages for the analyses
2. `functions.R` and `negative_binomial_functions.R`: Main functions for analyses
4. `age_structure.R`: Processes age structure data for population weighting
10. `contact_matrices_nb.R`: Creates and visualises contact matrices, using the weighted negative binomial regression model

## NGM Analysis Scripts

The `scripts/ngm_analysis/` directory contains scripts for the Next Generation Matrix analysis:

1. `run_full_ngm_analysis.R`: Master script that runs the complete NGM analysis pipeline
2. `core_analysis_functions.R`: Core functions for NGM analysis calculations
3. `stratified_analyses.R`: Performs stratified analyses by age, ethnicity, and socioeconomic status
4. `generate_all_plots.R`: Creates all NGM-related visualisations
5. `plotting_functions.r`: Plotting utility functions for NGM analysis
6. `create_appendix_tables.R`: Generates supplementary tables for the analysis
7. `create_combined_plots_with_overall.R`: Creates combined plot visualisations

## Running the Analysis

### Main Analysis

To run the main contact analysis:

1. Ensure you have R and the required packages installed,
2. Place the raw data in the `data/` directory,
3. Run `run_analysis.R`.

### NGM Analysis

To run the comprehensive Next Generation Matrix analysis:

1. Ensure you have R and the required packages installed,
2. Ensure census data is available in the `data/census_for_ngm/` directory,
3. Run `scripts/ngm_analysis/run_full_ngm_analysis.R`.

This will execute the complete NGM analysis pipeline:
- Clean census data for population weighting
- Perform stratified analyses by age, ethnicity, and socioeconomic status
- Generate all plots and visualisations
- Create appendix tables
- Render the final Quarto report

## Results

### Main Analysis Results

The main analysis produces:

- Contact matrices stratified by age, ethnicity, and NS-SEC classes
- Visualisations of contact patterns across different locations

### NGM Analysis Results

The NGM analysis produces:

- Stratified Next Generation Matrices
- Plots and visualisations for multiple demographic stratifications
- Appendix tables
