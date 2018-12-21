library(PReMiuM)
library(tidyverse)

rm(list = ls(all = TRUE))

setwd("~/EnviroTyping/sandbox/shifted_data_analysis/simulated/output")

df <- read_rds("../data/hyb_simulated.rds")[[1]]

variance.var <- names(which(map_dbl(df[,6:93], var, na.rm = TRUE) != 0))
min.vars <- str_subset(variance.var, "Min")

set.seed(12345)


runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 1000, nProgress = 100)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
write_rds(clusObj2, "../clusObj2.rds", compress = "xz")