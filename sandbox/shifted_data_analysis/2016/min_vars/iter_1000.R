library(PReMiuM)
library(tidyverse)

setwd("/work/04734/dhbrand/stampede2/github/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_1000/output")

df <- read_rds("../../../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")

variance.var <- names(which(map_dbl(df[,17:255], var, na.rm = TRUE) != 0))
min.vars <- str_subset(variance.var, "min")

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 1000, nBurn = 50, nProgress = 100, nClusInit = 1000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write(riskProfObj, "../riskProfObj.rds")

