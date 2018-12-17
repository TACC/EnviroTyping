### AUTHOR: Banks Osborne
### DATE: 7 November 2018
### GOAL: To show clusters' (of various similar genotypes) yields over the summer months
###       of 2016 as responses to various predictor variables
###       ASPECTS: 
### CHALLENGES:

# library(beepr)
# library(amap)
# library(cluster)
# library(dendextend)
# library(colorspace)
library(tidyverse)
# library(tictoc)
# library(dynamicTreeCut)
library(PReMiuM)
# library(viridis)
# library(gridExtra)
# library(reshape2)

setwd("/Users/banks/RProjects/EnviroTyping/sandbox/shifted_data_analysis/2016")

### TEST BY SHOWING CLUSTERS RESPONSES TO VARIABLES WITH ONLY JUNE MONTH FIRST ### 

# First, clean and arrange monthly weather data

hyb_by_mon <- read_rds("../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")

# Assign column names to a list
vars = colnames(hyb_by_mon)
print(vars)

# Select Pedi, Yield, and numeric variables ending in "6" to isolate June variables
june = select(hyb_by_mon,Pedi,Yield,ends_with("6"))

# Verify June variables by printing names
print(colnames(june))

# Take "min"/"max" variables from June data 
val <- grep("min|max",names(june))
numericVars <- names(june[val]) #[vapply(june[val], function(x) var(x) != 0, logical(1))]

# Run Profile Regression on just Min/Max June variables

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = numericVars,
                       #output = "output/",
                       data = june,
                       nSweeps = 500,
                       nProgress = 10
                       nBurn = 50)

calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

# Note: can just use clusObj = read_rds("min_vars_3000/clusObj.rds")
# TRY TO GET INFORMATION FROM CLUSTERING IN CLUS OBJ - 
# BUT WHAT ABOUT GETTING ONLY JUNE VARIABLES

# tic()
riskProfObj <- calcAvgRiskAndProfile(clusObj)
# toc()
# beep("ping")
write_rds(riskProfObj, "/riskProfObj.rds")
