library(PReMiuM)
library(profvis)
library(lineprof)

inputs <- generateSampleDataFile(clusSummaryNormalDiscrete())

source("pamParallel.R")

setwd("Output")

runInfoObj<-profRegr(yModel=inputs$yModel, xModel=inputs$xModel, nSweeps=100, nClusInit=15,
                     nBurn=300, data=inputs$inputData, output="output", covNames = inputs$covNames,
                     fixedEffectsNames = inputs$fixedEffectNames, seed=12345)

dissimObj<-calcDissimilarityMatrix(runInfoObj)

system.time({
    clusObj<-calcOptimalClustering(dissimObj)
})


# riskProfileObj<-calcAvgRiskAndProfile(clusObj)