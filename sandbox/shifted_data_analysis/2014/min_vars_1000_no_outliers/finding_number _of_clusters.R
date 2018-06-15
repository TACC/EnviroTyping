library(tidyverse)
library(PReMiuM)

setwd("~/github/EnviroTyping/sandbox/shifted_data_analysis/2014/min_vars_1000_no_outliers/output")

df <- read_rds("../../../../../data/interim/2014/hyb_by_mon_calib_wide_shifted.rds")
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

write_rds(clusObj, "../clusObj.rds", compress = "xz")
write_rds(outlier_list, "../outlier_list.rds", compress = "xz")
