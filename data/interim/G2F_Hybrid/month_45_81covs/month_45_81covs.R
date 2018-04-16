library(tidyverse)
library(PReMiuM)
library(tictoc)

setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/month_45_81covs/output")

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/month_45_81covs/output")

df <- read_rds("../../hybrid_by_month_calibrated_45subset_81covMedian.rds")

set.seed(1234)
tic()
runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = names(df[,16:96]),
                       data = df, nSweeps = 100, nBurn = 900,
                       nProgress = 25, seed = 1234)
toc()
write_rds(runInfoObj, path = "../runInfoObj.rds", compress = "xz")
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds", compress = "xz")


