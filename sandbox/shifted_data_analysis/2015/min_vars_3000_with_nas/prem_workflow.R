library(PReMiuM)
library(tidyverse)

setwd("/work/04902/azg5169/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2015/min_vars_3000_with_nas/output")

df <- read_rds("../../../../../data/interim/2015/hyb_by_mon_calib_wide_w_wth_nas.rds")

variance_var <- names(which(map_dbl(df[,16:length(df)], var, na.rm = TRUE) != 0))
min_vars <- str_subset(variance_var, "min")

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min_vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 500, seed = 12346)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds")

