library(PReMiuM)
library(tidyverse)
df <- read_rds("~/github/EnviroTyping/data/interim/2015/hyb_by_mon_calib_wide_shifted_front.rds")
cont_vars <- str_subset(names(df), "dew_mean")
runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed = 5000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)


# From my file
calcDists_Dave <- read_rds("C:/Users/Austin/Documents/GitHub/EnviroTyping/sandbox/comparison/calcDists_Dave.rds")
identical(calcDists, calcDists_Dave)

