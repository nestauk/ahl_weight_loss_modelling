# Modelling the Scale of the Weight Loss Challenge to Halve Obesity Prevalence in England

## Requirements

This repo relies on the `bw` package developed by researchers at the [National Institute of Public Health of Mexico](https://www.insp.mx/insp-overview.html), to which we are really grateful. The package implements the [Dynamic Weight Change model from Hall et al. (2011)](https://pubmed.ncbi.nlm.nih.gov/21872751/) for adults. To install the latest version please run the following code.

```
if (!require(devtools)){install.packages("devtools")}
devtools::install_github("INSP-RH/bw")
```
For further information about the model there are useful vignettes available by running

```
browseVignettes("bw")
```

After having installed the `bw` package run 

```
requirements.R
```
which automatically checks whether the packages needed for this analysis are installed and if they are not it installs them.

## How to Run this Project

After following the instructions highlighted in the Requirements section, run the scripts in the folders in this order:

1. Data Cleaning
2. Modelling
3. Analysis

## Datasets

This analysis is based on Health Survey for England data made available on the [UK Data Service](https://ukdataservice.ac.uk/) portal:

- [Health Survey for England, 1991-92](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7025): we have relied on the harmonised version of the HSE deposited on the UKDS portal. 
- [Health Survey for England, 2019](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8860)

