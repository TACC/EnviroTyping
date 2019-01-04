### AUTHOR: Banks Osborne
### DATE: 18 December 2018
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

setwd("/work/06019/bno5761/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2016")

### TEST BY SHOWING CLUSTERS RESPONSES TO VARIABLES WITH ONLY JUNE MONTH FIRST ### 

# First, clean and arrange monthly weather data

hyb_by_mon <- read_rds("../../../data/interim/2016/hyb_by_mon_calib_wide.rds")

# Assign column names to a list
# vars = colnames(hyb_by_mon)
# print(vars)

# Select Pedi, Yield, and numeric variables ending in following numbers to isolate March-November variables
mar_nov = select(hyb_by_mon,Pedi,Yield,ends_with("3"),ends_with("4"),ends_with("5"),
                 ends_with("6"),ends_with("7"),ends_with("8"),ends_with("9"),ends_with("10"),
                 ends_with("11"))

# Verify March-November variables by printing names
# print(colnames(mar_nov))

# Take "mean" variables from May-July data 
val <- grep("mean",names(mar_nov))
numericVars <- names(mar_nov[val])

# Run Profile Regression on just "Mean" May-July variables

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = numericVars,
                       #output = "output/",
                       data = mar_nov,
                       nSweeps = 200,
                       nProgress=10,
                       nBurn = 200)

calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

# Note: can just use clusObj = read_rds("min_vars_3000/clusObj.rds")
# TRY TO GET INFORMATION FROM CLUSTERING IN CLUS OBJ - 
# BUT WHAT ABOUT GETTING ONLY JUNE VARIABLES

# tic()
riskProfObj <- calcAvgRiskAndProfile(clusObj)
# toc()
# beep("ping")
write_rds(riskProfObj, "/riskProfObj.rds", compress = "xz")
