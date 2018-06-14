library(tidyverse)
library(PReMiuM)


setwd("~/github/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_3000/output")

inputs <- generateSampleDataFile(clusSummaryBernoulliDiscrete())

clusObj <- list()
seed <- c(54161148, 96222856, 43479820, 54824768, 47347836, 69778632, 75928328, 04363405, 45616084, 48415972)
for (i in seed) {
    runInfoObj<-profRegr(yModel=inputs$yModel,
                         xModel=inputs$xModel, nSweeps=3000,
                         nBurn=50, data=inputs$inputData, output="output",
                         covNames = inputs$covNames,
                         fixedEffectsNames = inputs$fixedEffectNames, seed=i)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj[[i]] <- calcOptimalClustering(calcDists)
}

toy_df <- read_rds("~/github/EnviroTyping/data/interim/2015/toy_data.rds")

for (i in seed) {
    runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = names(toy_df)[3:8], data = toy_df, nSweeps = 3000, nBurn = 50, nProgress = 100, seed = i)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj[[i]] <- calcOptimalClustering(calcDists)
    print(i)
}
