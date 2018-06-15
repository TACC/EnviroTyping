library(tidyverse)
library(PReMiuM)

setwd("~/github/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_3000_no_outliers/output")
df <- read_rds("../../../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")

variance_var <- names(which(map_dbl(df[,16:255], var, na.rm = TRUE) != 0))
min_vars <- str_subset(variance_var, "min")

runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min_vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, seed = 1528830361)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj3 <- calcOptimalClustering(calcDists)
1528830361/1528831845
outlier_list <- list()
while (any(clusObj$clusterSizes <= 3 )) {
    outlier_clusters <- which(clusObj$clusterSizes <= 3, useNames = TRUE)
    outlier <- bind_cols(clus = clusObj$clustering, yield = clusObj$clusObjRunInfoObj$yMat[,1], pedi = clusObj$clusObjRunInfoObj$xMat[,1]) %>% filter(clus %in% outlier_clusters) %>% select(yield, pedi)
    outlier_list <- c(outlier_list, outlier)
    update <- bind_cols(yield = clusObj$clusObjRunInfoObj$yMat[,1], clusObj$clusObjRunInfoObj$xMat) %>% filter(!(Pedi == outlier$pedi & yield == outlier$yield)) %>% modify_at(2, as.character) %>% modify_at(2,as_factor)
    
    set.seed(1234)
    runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = max_vars, data = update, nSweeps = 1000, nBurn = 50, nProgress = 100)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
    # if (length(clusObj$clusterSizes) >= 3) 
    #     break
    print(clusObj$clusterSizes)
}

write_rds(clusObj, "../clusObj.rds", compress = "xz")
write_rds(outlier_list, "../outlier_list.rds", compress = "xz")

clus_3 <- bind_cols(cluster = clusObj$clustering, df) %>% filter(cluster == 3)
set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = max_vars, data = clus_3, nSweeps = 1000, nBurn = 50, nProgress = 100)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)