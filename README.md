# NY-Education
This is the updated clean data folder for NY-Education Modeling Analysis.

## [clean] folder
There are four clean data in this folder, organized by Yoyo, Rosy, and Rosa.

## Merging.Rmd
`Merging.Rmd` uses clean datasets under `clean_data` folder. 
`Merging.Rmd` combines all variables in those four dataset to produce monthly data, and then it regroups yearly data.
This file also creates a lagged variable for monthly COVID cases.

Output: `Monthly.Merged.csv` and `Yearly.Merged.csv`

## [merged] folder
This folder includes the merged dataset output from `Merging.Rmd`.

## Modeling.Rmd
`Modeling.Rmd` contains modeling approaches for monthly and yearly dataset. It also includes plots for data exploratory analysis.