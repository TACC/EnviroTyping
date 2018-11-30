library(PReMiuM)
library(tidyverse)

setwd("/work/04902/azg5169/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_3000/output")

df <- read_rds("../../../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")

variance.var <- names(which(map_dbl(df[,16:255], var, na.rm = TRUE) != 0))
min.vars <- str_subset(variance.var, "min")

set.seed(12345)

seeds = c(5015460331, 2924967319, 2560083040, 6900504555)

runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000, seed = seeds[1])
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds")
write_rds(clusObj, "../clusObj1.rds", compress = "xz")

runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000, seed = seeds[2])
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds")
write_rds(clusObj, "../clusObj2.rds", compress = "xz")

runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000, seed = seeds[3])
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds")
write_rds(clusObj, "../clusObj3.rds", compress = "xz")

runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000, seed = seeds[4])
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds")
write_rds(clusObj, "../clusObj4.rds", compress = "xz")

