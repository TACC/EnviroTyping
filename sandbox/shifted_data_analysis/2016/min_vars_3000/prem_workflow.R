library(PReMiuM)
library(tidyverse)

#clear environment in case there have been other worflows run
rm(list = ls(all = TRUE))

#set directory for the output files (created by the batch script)
setwd("/work/04902/azg5169/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_3000/output1")

#read in by-month, calibrated, wide shifted data, example in docs
df <- read_rds("../../../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")

#remove 0 variance variables
variance.var <- names(which(map_dbl(df[,16:255], var, na.rm = TRUE) != 0))

#subset only the minimum measurements (mean, median, and max are other options)
min.vars <- str_subset(variance.var, "min")

#set seed for R 
set.seed(12345)

#profile regression
#don't set seed because we can get the seed used from the output.R
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 1000, nProgress = 1000)

#calculate dissimilarity matrix
calcDists <- calcDissimilarityMatrix(runInfoObj)

#calculate optimal clustering
clusObj <- calcOptimalClustering(calcDists)

#calculate risk profile
riskProfObj <- calcAvgRiskAndProfile(clusObj)

#write clusObj to take a quick look at clustering 
#clustering is how we have been assessing performance on surface level
write_rds(clusObj, "../clusObj1.rds", compress = "xz")
