### AUTHOR: Banks Osborne
### DATE: 25 October 2018
### GOAL: To show clusters' (of various similar genotypes) yields over the summer months
###       as responses to various predictor variables
###       ASPECTS: 
### CHALLENGES:

library(beepr)
library(amap)
library(cluster)
library(dendextend)
library(colorspace)
library(tidyverse)
library(tictoc)
library(dynamicTreeCut)
library(PReMiuM)
library(viridis)
library(gridExtra)
library(reshape2)

setwd("/Users/banks/RProjects/EnviroTyping/sandbox/shifted_data_analysis/2015")

### TEST BY SHOWING CLUSTERS RESPONSES TO VARIABLES WITH ONLY JUNE MONTH FIRST ### 

# First, clean and arrange monthly weather data
# Try with no outliers
hyb_by_mon <- read_rds("../../../data/interim/2015/hybrid_by_month_shift_all_stats.rds")

# Assign column names to a list
vars = colnames(hyb_by_mon)

print(vars)

# Assign index to loop through columns to keep June months
month_ind = 0
c = 1
month_ind[0] = 4
for (i in 1:length(vars)) {
    if (endsWith(vars[i], "6")) {
        month_ind[c] = i
        c = c+1
    }
}

# Combine Pedi, Yield, and June variables 
june = cbind(hyb_by_mon[2],hyb_by_mon[4],hyb_by_mon[month_ind])

# Verify June variables by printing names
print(colnames(june))

# Take "min"/"max" variables from June data 
val <- grep("Min|Max",names(june))
numericVars <- names(june[val])[vapply(june[val], function(x) var(x) != 0, logical(1))]

# Run Profile Regression on just Min/Max June variables

# SEE NOTE BELOW ABOUT SKIPPING STEP - ISSUE WITH ONLY TWO CLUSTERS
runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = numericVars,
                       #output = "output/",
                       data = june,
                       nSweeps = 1000,
                       nBurn = 1000)

calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

# Note: can just use clusObj = read_rds("min_vars_3000/clusObj.rds")
# TRY TO GET INFORMATION FROM CLUSTERING IN CLUS OBJ - 
# BUT WHAT ABOUT GETTING ONLY JUNE VARIABLES

# tic()
# riskProfObj <- calcAvgRiskAndProfile(clusObj)
# toc()
# beep("ping")
# saveRDS(riskProfObj, "../riskProfObj.rda")
