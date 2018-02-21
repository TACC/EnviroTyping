library(PReMiuM)

setwd("/werk/04734/dhbrand/stampede2/EnviroTyping/data/interim/G2F_Hybrid/premiumOutput_2.15.18_6-8wksSubset/output_seed_6_8/")

runInfoObj <- readRDS("runInfoObj.rda")

calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

saveRDS(clusObj, file = "clusObj.rda")
