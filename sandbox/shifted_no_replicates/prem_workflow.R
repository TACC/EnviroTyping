library(PReMiuM)
library(tidyverse)

setwd("~/github/EnviroTyping/sandbox/shifted_no_replicates/output")

df <- read_rds("../../../data/interim/2015/hybrid_by_month_shift_all_stats.rds") %>% filter(Repl == 1)

variance.var <- names(which(map_dbl(df[,17:189], var, na.rm = TRUE) != 0))
min.vars <- str_subset(variance.var, "Min")

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)

clus_1 <- bind_cols(clus = clusObj$clustering, df) %>% filter(clus == 1)
table(clus_1$StatID)
clus_2 <- bind_cols(clus = clusObj$clustering, df) %>% filter(clus == 2)
table(clus_2$StatID)

