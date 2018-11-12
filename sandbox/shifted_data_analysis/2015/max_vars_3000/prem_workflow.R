library(PReMiuM)
library(tidyverse)
library(tictoc)


setwd("/work/04902/azg5169/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2015/2015/max_vars_3000/output")

df <- read_rds("../../../../../data/interim/2015/hyb_by_mon_calib_wide_shifted.rds")

variance.var <- names(which(map_dbl(df[,16:length(df)], var, na.rm = TRUE) != 0))
max.vars <- str_subset(variance.var, "max")

set.seed(12345)
tic()
runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = max_vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000)
toc()
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds")
write_rds(clusObj, "../clusObj.rds", compress = "xz")

