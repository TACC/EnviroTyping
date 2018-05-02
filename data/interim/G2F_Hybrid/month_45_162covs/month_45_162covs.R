library(tidyverse)
library(PReMiuM)
library(tictoc)

setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/month_45_162covs/output")

#setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/month_45_162covs/output")

df <- read_rds("../../hybrid_by_month_calibrated_45subset_162covMinMax.rds")

set.seed(1234)
tic()
runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = names(df[,16:177]),
                       data = df, nSweeps = 500, nBurn = 500,
                       nProgress = 10, seed = 1234, 
                       reportBurnIn = TRUE)
toc()
write_rds(runInfoObj, path = "../runInfoObj.rds", compress = "xz")

tic();calcDists <- calcDissimilarityMatrix(runInfoObj);toc()

tic();clusObj <- calcOptimalClustering(calcDists);toc()

tic();riskProfObj <- calcAvgRiskAndProfile(clusObj);toc()
write_rds(riskProfObj, "../riskProfObj.rds", compress = "xz")


