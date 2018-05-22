# library(PReMiuM)
# library(tidyverse)

setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/sandbox/shifted_data_analysis/output_shift")
df.16 <- read_rds("../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")


variance.var <- names(which(map_dbl(df.16[,17:295], var, na.rm = TRUE) != 0))
min.vars <- str_subset(variance.var, "min")
max.vars <- str_subset(variance.var, "max")
mean.vars <- str_subset(variance.var, "mean")


set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df.16, nSweeps = 1000, nBurn = 50, nProgress = 100, nClusInit = 1000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write(riskProfObj, "min_vars_1000.rds")


set.seed(1234)
runInfoObj2 <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df.16, nSweeps = 2000, nBurn = 50, nProgress = 100, nClusInit = 1000)
calcDists2 <- calcDissimilarityMatrix(runInfoObj2)
clusObj2 <- calcOptimalClustering(calcDists2)
riskProfObj2 <- calcAvgRiskAndProfile(clusObj2)
write(riskProfObj2, "min_vars_2000.rds")

set.seed(1234)
runInfoObj3 <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df.16, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000)
calcDists3 <- calcDissimilarityMatrix(runInfoObj3)
clusObj3 <- calcOptimalClustering(calcDists3)
riskProfObj3 <- calcAvgRiskAndProfile(clusObj3)
write(riskProfObj3, "min_vars_3000.rds")


set.seed(1234)
runInfoObj4 <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = max.vars, data = df.16, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000)
calcDists4 <- calcDissimilarityMatrix(runInfoObj4)
clusObj4 <- calcOptimalClustering(calcDists4)
riskProfObj4 <- calcAvgRiskAndProfile(clusObj4)
write(riskProfObj4, "max_vars_3000.rds")


set.seed(1234)
runInfoObj5 <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = mean.vars, data = df.16, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000)
calcDists5 <- calcDissimilarityMatrix(runInfoObj5)
clusObj5 <- calcOptimalClustering(calcDists5)
riskProfObj5 <- calcAvgRiskAndProfile(clusObj5)
write(riskProfObj5, "mean_vars_3000.rds")

