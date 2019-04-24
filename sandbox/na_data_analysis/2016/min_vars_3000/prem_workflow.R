#hyb_by_mon_calib_w_wth_nas

library(PReMiuM)
library(tidyverse)

setwd("/work/04902/azg5169/stampede2/EnviroTyping/sandbox/na_data_analysis/2016/min_vars_3000/output")
#setwd("~/EnviroTyping/sandbox/na_data_analysis/2016/min_vars_3000/output")

df <- read_rds("../../../../../data/interim/2016/hyb_by_mon_calib_w_wth_nas.rds")

df = df[is.na(df$yield) == FALSE,]

variance.var <- names(which(map_dbl(df[,16:dim(df)[2]], var, na.rm = TRUE) != 0))
min.vars <- str_subset(variance.var, "min")

set.seed(12345)
runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 1000, nProgress = 1000, nClusInit = 500)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(clusObj, "../clusObj.rds", compress = "xz")

