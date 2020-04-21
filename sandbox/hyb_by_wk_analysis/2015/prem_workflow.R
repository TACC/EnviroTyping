library(PReMiuM)
library(tidyverse)

#clear environment in case there have been other worflows run
rm(list = ls(all = TRUE))

#set directory for the output files (created by the batch script)
#setwd("/work/04902/azg5169/stampede2/EnviroTyping/sandbox/hyb_by_day_analysis/2015/output")
setwd("sandbox/hyb_by_wk_analysis/2015/output")

#read in by-month, calibrated, wide shifted data, example in docs
df <- read_rds("../../../../data/interim/2015/hyb_by_wk_calib.rds")

#remove 0 variance variables
variance.var <- names(which(map_dbl(df[,17:56], var, na.rm = TRUE) != 0))

#subset only the minimum measurements (mean, median, and max are other options)
min.vars <- str_subset(variance.var, "min")

df = df[sample(c(1:length(t(df[,1]))), 100),]

#set seed for R 
set.seed(12345)

#profile regression
#don't set seed because we can get the seed used from the output.R
runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min.vars, data = df, nSweeps = 1000, nBurn = 1000, nProgress = 50)

#calculate dissimilarity matrix
calcDists <- calcDissimilarityMatrix(runInfoObj)

#calculate optimal clustering
clusObj <- calcOptimalClustering(calcDists)

clusObj$clusObjRunInfoObj$xMat = na_if(clusObj$clusObjRunInfoObj$xMat, -999)

#calculate risk profile
riskProfObj <- calcAvgRiskAndProfile(clusObj)

#write clusObj to take a quick look at clustering 
#clustering is how we have been assessing performance on surface level
write_rds(clusObj, "../clusObj.rds", compress = "xz")

write_rds(riskProfObj, "../riskProfObj.rds", compress = "xz")
