require(foreach)
require(doMC)
library(tictoc)
library(PReMiuM)
library(tidyverse)

setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/sandbox/var_sel_min/output")
df <- read_rds("../../../data/interim/G2F_Hybrid/hybrid_by_month_shift_all_stats.rds")
contVars <- names(df)[grep("Min", names(df))]

registerDoMC(cores=4)


inputs <- generateSampleDataFile(clusSummaryNormalNormal())
tic()
result <- foreach(i=1:4) %dopar% {
    runInfoObj <- profRegr(yModel=inputs$yModel, xModel=inputs$xModel, nSweeps=100, nClusInit=15, nBurn=300, data=inputs$inputData, output=paste("run_", i, sep = ""), covNames = inputs$covNames, fixedEffectsNames = inputs$fixedEffectNames, seed=12345)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
}
toc()

comb <- function(x, ...) {
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

premComb <- function(x) {
    
}

registerDoMC(cores = 2)
tic()
master <- foreach(i=1:2, .combine='comb', .multicombine=TRUE, .init=list(list())) %dopar% {
    list(
        profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = contVars,
                       data = df, nSweeps = 3000, nBurn = 50,
                       nProgress = 50, seed = 1234,
                       reportBurnIn = TRUE)
    )
}
toc()

tic()
master <- foreach(i=1:2, .combine='comb', .multicombine=TRUE, .init=list(list())) %dopar% {
    list(
        profRegr(covNames, outcome = 'Yield',
                 yModel = 'Normal', xModel = "Mixed",
                 discreteCovs = "Pedi",
                 continuousCovs = contVars,
                 data = df, nSweeps = 3000, nBurn = 50,
                 nProgress = 50, seed = 1234,
                 reportBurnIn = TRUE)
    )
}
toc()


oper <- foreach(i=1:10, .combine='comb', .multicombine=TRUE,
                .init=list(list(), list())) %dopar% {
                    list(i+2, i+3)
                }

oper1 <- oper[[1]]
oper2 <- oper[[2]]

str(oper)


runInfoObj <- profRegr(yModel=inputs$yModel, xModel=inputs$xModel, nSweeps=100, nClusInit=15, nBurn=300, data=inputs$inputData, output=('ouput'), covNames = inputs$covNames, fixedEffectsNames = inputs$fixedEffectNames, seed=12345)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)

multiResultClass <- function(result1=NULL,result2=NULL) {
    me <- list(
        result1 = result1,
        result2 = result2
    )
    
    ## Set the name for the class
    class(me) <- append(class(me),"multiResultClass")
    return(me)
}
prem <- foreach(i=1:2) %dopar% {
    result <- multiResultClass()
    result$result1 <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi",
continuousCovs = contVars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 50, seed = 1234, reportBurnIn = TRUE)
    result$result2 <- calcDissimilarityMatrix(result$result1)
    return(result)
}
