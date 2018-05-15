require(foreach)
require(doMC)
library(tictoc)
library(PReMiuM)
inputs <- generateSampleDataFile(clusSummaryNormalNormal())

registerDoMC(cores=4)

tic()
result <- foreach(i=1:4) %dopar% {
    runInfoObj <- profRegr(yModel=inputs$yModel, xModel=inputs$xModel, nSweeps=100, nClusInit=15, nBurn=300, data=inputs$inputData, output=paste("run_", i, sep = ""), covNames = inputs$covNames, fixedEffectsNames = inputs$fixedEffectNames, seed=12345)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
}
toc()

tic()
master <- foreach(i=1:4) %dopar% {
    runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = contVars,
                       data = df, nSweeps = 3000, nBurn = 50,
                       nProgress = 50, seed = 1234,
                       reportBurnIn = TRUE)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
}
toc()