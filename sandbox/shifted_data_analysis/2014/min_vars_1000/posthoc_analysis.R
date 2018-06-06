library(tidyverse)
library(PReMiuM)

setwd("~/github/EnviroTyping/sandbox/shifted_data_analysis/2014/min_vars_1000/output")

min_1K_14 <- read_rds("../riskProfObj.rds")

clusObj <- min_1K_14$riskProfClusObj
outlier_list <- list()
while (clusObj$clusterSizes[2] == 1) {
    outlier <- bind_cols(clus = clusObj$clustering, yield = clusObj$clusObjRunInfoObj$yMat[,1],  clusObj$clusObjRunInfoObj$xMat) %>% filter(clus == 2) %>% select(yield, pedi)
    outlier_list <- c(outlier_list, outlier)
    update <- bind_cols(yield = clusObj$clusObjRunInfoObj$yMat, clusObj$clusObjRunInfoObj$xMat) %>% filter(!(pedi == outlier$pedi & yield == outlier$yield)) %>% modify_at(2, factor)
    
    min.vars <- names(update[,3:26])
    
    set.seed(1234)
    runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min.vars, data = update, nSweeps = 1000, nBurn = 50, nProgress = 100, nClusInit = 1000, seed = 1528146167)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
    if (length(clusObj$clusterSizes) >= 3) 
        break
    print(clusObj$clusterSizes)
}
