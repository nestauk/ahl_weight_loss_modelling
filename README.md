# Modelling the Scale of the Weight Loss Challenge to Halve Obesity Prevalence in England

## Requirements

This project was built with R version 4.2.1 (2022-06-23) -- "Funny-Looking Kid" and RStudio 2022.07.2+576 "Spotted Wakerobin" Release (e7373ef832b49b2a9b88162cfe7eac5f22c40b34, 2022-09-06) for macOS
Mozilla/5.0 (Macintosh; Intel Mac OS X 12_6_0) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.10 Chrome/69.0.3497.128 Safari/537.36

The list of packages needed to run the project is automatically installed by running the script `requirements.R` which automatically checks whether the packages needed for this analysis are installed and if they are not it installs them. This repo relies on the `bw` package which is not currently available on CRAN. To install this package, run the following code.

```
if (!require(devtools)){install.packages("devtools")}
devtools::install_github("INSP-RH/bw")
```
For further information about the model there are useful vignettes available by running

```
browseVignettes("bw")
```


## How to Run this Project

After following the instructions highlighted in the Requirements section, run the scripts in the folders in this order:

1. Data Cleaning
2. Modelling
3. Analysis

## Datasets

This analysis is based on Health Survey for England data made available on the [UK Data Service](https://ukdataservice.ac.uk/) portal. To run this project you need to download and save a copy of the files locally to the `inputs\data` folder. Please note that you need a UKDS account to access data. More information on how to create a UKDS account is available on the [UK DataService website](https://beta.ukdataservice.ac.uk/myaccount/credentials). You also need to create a project on the UKDS portal. After having created an account, logged in and created a project follow the instructions below:

### [Health Survey for England, 1991-92](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7025) 
1. Follow this [link](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7025)
2. Click on the Access data button
3. On the next page click on Add to account
4. Head to your account and add the dataset to a project
5. Head to your project, click on Actions, and then click on Download from the drop down menu
6. Select the TAB option and then Download selected
7. A zip folder will be downloaded: extract the files and head to the tab folder: the data file that we need for this project is called `1991_2009hse.tab`
8. Save `1991_2009hse.tab` to `inputs\data`

### [Health Survey for England, 2019](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8860)

1. Follow this [link](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8860)
2. Click on the Access data button
3. On the next page click on Add to account
4. Head to your account and add the dataset to a project
5. Head to your project, click on Actions, and then click on Download from the drop down menu
6. Select the TAB option and then Download selected
7. A zip folder will be downloaded: extract the files and head to the tab folder: the data file that we need for this project is called `hse_2019_eul_202122006.tab`
8. Save `hse_2019_eul_202122006.tab` to `inputs\data`

## Repository Overview
```
Analysis
   |-- .DS_Store
   |-- analysis.r
   |-- check_time.R
Data Cleaning
   |-- hse_1991.R
   |-- hse_2019.R
Modelling
   |-- .DS_Store
   |-- 1. bmi_distribution_1991.R
   |-- 2. equating_weight_change.R
   |-- 3a_obese.R
   |-- 3b_overweight.R
   |-- 3c_morbidly_obese.R
   |-- Archived
   |   |-- 4. wrapper_function_final_weight_input.R
   |   |-- 4b. wrapper_function_final_weight_input.R
   |   |-- 4c. wrapper_function_final_weight_input.R
   |   |-- 6. wrapper_function_final_weight_input_3_year_split.R
   |   |-- 7. Scenario 2.r
   |   |-- 8. PA analysis.R
   |   |-- find_ei.R
README.md
ahl_weight_loss_modelling.Rproj
inputs
   |-- .DS_Store
   |-- data
   |   |-- .DS_Store
   |   |-- hse_2019_eul_20211006.tab
outputs
   |-- .DS_Store
   |-- data
   |   |-- hse_1991_clean.csv
   |   |-- hse_2019_clean.csv
   |   |-- morb_3_year.csv
   |   |-- obese_all_years.csv
   |   |-- over_3_year.csv
   |-- figures
   |   |-- bmi_distribution_50reduction.png
   |   |-- bmi_female.png
   |   |-- bmi_male.png
   |   |-- intake_all.png
   |   |-- intake_change_distribution.png
   |   |-- intake_difference.png
   |   |-- intake_distribution.png
   |   |-- obese_boxplot_year.png
   |-- reports
   |   |-- ci_intake.csv
   |   |-- iqr_intake.csv
   |   |-- iqr_weight.csv
   |   |-- mean_intake.csv
   |   |-- mean_weight.csv
   |   |-- median_intake.csv
   |   |-- median_weight.csv
   |   |-- sd_intake.csv
   |   |-- sd_weight.csv
   |   |-- weight_change_table_mean.csv
   |   |-- weight_change_table_median.csv
requirements.R
```

## Contact and Contributions
This project relies on the `bw` package which was developed by researchers at the [National Institute of Public Health of Mexico](https://www.insp.mx/insp-overview.html), to which we are really grateful. The package implements the [Dynamic Weight Change model from Hall et al. (2011)](https://pubmed.ncbi.nlm.nih.gov/21872751/) for adults. 
