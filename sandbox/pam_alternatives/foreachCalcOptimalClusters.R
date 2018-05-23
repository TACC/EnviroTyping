library(PReMiuM)


inputs <- generateSampleDataFile(clusSummaryNormalNormal())

runInfoObj <- profRegr(yModel=inputs$yModel, xModel=inputs$xModel, nSweeps=100, nClusInit=15, nBurn=300, data=inputs$inputData, output="output", covNames = inputs$covNames,fixedEffectsNames = inputs$fixedEffectNames, seed=12345)

disSimObj <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(dissimObj)
