# Modelling

This folders hosts the function that is core to the analysis and which takes in a weight target to find a reduction in calorie intake. This function is defined in the script `find_EI.R`.

To run the whole pipeline in one script, open the file `RUN_PIPELINE.R` and execute its content. This file calls and evaluates the rest of the files in this folder, which are:

* `1. bmi_distribution_1991.R`: deep dive into the 1991 HSE wave
* `2. equating_weight_change.R`: script that performs equipercentile equating and saves various output files in `outputs\reports`
* `3a_obese.R`: script that takes in the obese subset HSE 2019 data and simulates the intake distribution under the scenario of 50% reduction in obesity prevalence
* `3b_overweight.R`: script that takes in the overweight subset HSE 2019 data and simulates the intake distribution under the scenario of 50% reduction in obesity prevalence
* `3c_morbidly_obese.R`: script that takes in the morbidly obese subset HSE 2019 data and simulates the intake distribution under the scenario of 50% reduction in obesity prevalence
