library(tidyverse)
library(PReMiuM)
library(tictoc)

setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/month_45_162covs/iters400/output")

#setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/month_45_162covs/output")

df <- read_rds("../../../hybrid_by_month_calibrated_45subset_162covMinMax.rds")

set.seed(1234)
tic()
runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = names(df[,16:177]),
                       data = df, nSweeps = 40, nBurn = 360,
                       nProgress = 25, seed = 1234)
toc()
write_rds(runInfoObj, path = "../runInfoObj.rds", compress = "xz")
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds", compress = "xz")


