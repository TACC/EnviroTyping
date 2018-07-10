library(PReMiuM)
library(tidyverse)

setwd("/work/04902/azg5169/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2015/mean_vars_1000/output")

df <- read_rds("../../../../../data/interim/2015/hyb_by_mon_calib_wide_shifted.rds")

variance.var <- names(which(map_dbl(df[,16:255], var, na.rm = TRUE) != 0))
mean.vars <- str_subset(variance.var, "mean")

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = mean.vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000, seed = 2435)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds")

