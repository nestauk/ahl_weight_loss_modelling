# Data Cleaning 

The script in this folder reads the raw data of the Health Survey of England data from `inputs\data`, performs various transformations and saves a cleaned version in `outputs\data`. The steps undertaken are:
* filter to required wave (for 1991 file only)
* remove children (for 2019 file only)
* recalculate age based on mid-point of age groups (for 2019 file only)
* remove invalid observations for body weight and height
* renames variables so that they are consistent across the two datasets
* creates a variable for physical activity at the pre-defined level of `1.5`
* creates a variable for Resting Metabolic Rate following [Miffin & St.Jeor](https://pubmed.ncbi.nlm.nih.gov/2305711/) formula
* creates a variable for BMI category (`underweight`, `normal`, `overweight`, `obese`, `morbidly obese`)
