### AUTHOR: Banks Osborne
### DATE: 13 January 2019

library(tidyverse)
library(PReMiuM)

setwd("/work/06019/bno5761/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2016/mean_vars_3000/output")

hyb_by_mon <- read_rds("../../../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")

variance.var <- names(which(map_dbl(hyb_by_mon[,16:255], var, na.rm = TRUE) != 0))
mean.vars <- str_subset(variance.var, "mean")

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", 
                       discreteCovs = "Pedi", continuousCovs = mean.vars, data = hyb_by_mon, nSweeps = 3000, 
                       nBurn = 50, nProgress = 10, nClusInit = 1000, seed = 2435)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds")