library(PReMiuM)
library(profvis)
library(ClusterR)

inputs <- generateSampleDataFile(clusSummaryNormalDiscrete())

setwd("scripts/multiThreading/")
source("pamParallel.R")

setwd("Output")

runInfoObj<-profRegr(yModel=inputs$yModel, xModel=inputs$xModel, nSweeps=100, nClusInit=15,
                     nBurn=300, data=inputs$inputData, output="output", covNames = inputs$covNames,
                     fixedEffectsNames = inputs$fixedEffectNames, seed=12345)

dissimObj<-calcDissimilarityMatrix(runInfoObj)
#diss <- as.matrix(dissimObj$disSimMat)
# Cluster_Medoids(data = diss, clusters = 5, distance_metric = "euclidean",
#                 minkowski_p = 1, threads = 1, swap_phase = TRUE,v
#                 erbose = TRUE, seed = 1)



system.time({
    clusObj<-calcOptimalClustering(dissimObj)
})


# riskProfileObj<-calcAvgRiskAndProfile(clusObj)