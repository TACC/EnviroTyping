library(PReMiuM)
library(tidyverse)

rm(list = ls(all = TRUE))

setwd("/work/04902/azg5169/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_3000/output1")

df <- read_rds("../../../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")

variance.var <- names(which(map_dbl(df[,16:255], var, na.rm = TRUE) != 0))
min.vars <- str_subset(variance.var, "min")

set.seed(12345)


runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 1000, nProgress = 1000, nClusInit = 1000, seed = 3846532330)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(clusObj, "../clusObj1.rds", compress = "xz")
