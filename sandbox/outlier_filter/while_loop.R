library(tidyverse)
library(PReMiuM)

setwd("~/github/EnviroTyping/sandbox/outlier_filter/output")

toy <- read_rds("../../../data/interim/2015/toy_data.rds")

cont_vars <- names(toy[3:10])
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = cont_vars, data = toy, nSweeps = 3000, nBurn = 50, nProgress = 100, seed = 9999)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)

outlier_list <- list()
while (clusObj$clusterSizes[2] == 1) {
    outlier <- bind_cols(clus = clusObj$clustering, yield = clusObj$clusObjRunInfoObj$yMat[,1],  clusObj$clusObjRunInfoObj$xMat) %>% filter(clus == 2) %>% select(yield, Pedi)
    outlier_list <- c(outlier_list, outlier)
    update <- bind_cols(yield = toy$Yield, pedi = toy$Pedi, clusObj$clusObjRunInfoObj$xMat) %>% filter(!(Pedi == outlier$Pedi & yield == outlier$yield))
    
    set.seed(1234)
    runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = update, nSweeps = 3000, nBurn = 50, nProgress = 100, seed = 9999)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
    if (length(clusObj$clusterSizes) >= 3) 
        break
    print(clusObj$clusterSizes)
}

### 2014
df <- read_rds("../../../data/interim/2014/hyb_by_mon_calib_wide_shifted.rds")
variance_var <- names(which(map_dbl(df[,16:207], var, na.rm = TRUE) != 0))
min_vars <- str_subset(variance_var, "min")
runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min_vars, data = df, nSweeps = 1000, nBurn = 50, nProgress = 100)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)

outlier_list <- list()
while (any(clusObj$clusterSizes <= 3 )) {
    outlier_clusters <- which(clusObj$clusterSizes <= 3, useNames = TRUE)
    outlier <- bind_cols(clus = clusObj$clustering, yield = clusObj$clusObjRunInfoObj$yMat[,1], pedi = clusObj$clusObjRunInfoObj$xMat[,1]) %>% filter(clus %in% outlier_clusters) %>% select(yield, pedi)
    outlier_list <- c(outlier_list, outlier)
    update <- bind_cols(yield = clusObj$clusObjRunInfoObj$yMat[,1], clusObj$clusObjRunInfoObj$xMat) %>% filter(!(pedi == outlier$pedi & yield == outlier$yield)) %>% modify_at(2, as.character) %>% modify_at(2,as_factor)
    
    set.seed(1234)
    runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min_vars, data = update, nSweeps = 1000, nBurn = 50, nProgress = 100)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
    # if (length(clusObj$clusterSizes) >= 3) 
    #     break
    print(clusObj$clusterSizes)
}

### 2016
df <- read_rds("../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")
variance_var <- names(which(map_dbl(df[,16:255], var, na.rm = TRUE) != 0))
max_vars <- str_subset(variance_var, "max")
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = max_vars, data = df, nSweeps = 1000, nBurn = 50, nProgress = 100)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)

outlier_list <- list()
while (any(clusObj$clusterSizes <= 3 )) {
    outlier_clusters <- which(clusObj$clusterSizes <= 3, useNames = TRUE)
    outlier <- bind_cols(clus = clusObj$clustering, yield = clusObj$clusObjRunInfoObj$yMat[,1], pedi = clusObj$clusObjRunInfoObj$xMat[,1]) %>% filter(clus %in% outlier_clusters) %>% select(yield, pedi)
    outlier_list <- c(outlier_list, outlier)
    update <- bind_cols(yield = clusObj$clusObjRunInfoObj$yMat[,1], clusObj$clusObjRunInfoObj$xMat) %>% filter(!(pedi == outlier$pedi & yield == outlier$yield)) %>% modify_at(2, as.character) %>% modify_at(2,as_factor)
    
    set.seed(1234)
    runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min_vars, data = update, nSweeps = 1000, nBurn = 50, nProgress = 100)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
    # if (length(clusObj$clusterSizes) >= 3) 
    #     break
    print(clusObj$clusterSizes)
}

