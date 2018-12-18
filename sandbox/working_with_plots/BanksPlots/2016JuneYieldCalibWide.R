### AUTHOR: Banks Osborne
### DATE: 17 December 2018
### GOAL: To show clusters' (of various similar genotypes) yields over the summer months
###       of 2016 as responses to various predictor variables
###       ASPECTS: Not the shifted data
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

setwd("/home1/06019/bno5761/EnviroTyping/sandbox/shifted_data_analysis/2016")

### TEST BY SHOWING CLUSTERS RESPONSES TO VARIABLES WITH ONLY JUNE MONTH FIRST ### 

# First, clean and arrange monthly weather data

hyb_by_mon <- read_rds("../../../data/interim/2016/hyb_by_mon_calib_wide.rds")

# Assign column names to a list
# vars = colnames(hyb_by_mon)
# print(vars)

# Select Pedi, Yield, and numeric variables ending in "5","6", or "7" to isolate May-July variables
may_july = select(hyb_by_mon,Pedi,Yield,ends_with("5"),ends_with("6"),ends_with("7"))

# Verify May-July variables by printing names
# print(colnames(may_july))

# Take "mean" variables from May-July data 
val <- grep("mean",names(may_july))
numericVars <- names(may_july[val])

# Run Profile Regression on just "Mean" May-July variables

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = numericVars,
                       #output = "output/",
                       data = may_july,
                       nSweeps = 500,
                       nProgress=10,
                       nBurn = 500)

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
