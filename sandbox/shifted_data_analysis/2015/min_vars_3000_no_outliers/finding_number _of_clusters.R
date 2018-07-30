library(tidyverse)
library(PReMiuM)
library(magrittr)

setwd("/work/04902/azg5169/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2015/min_vars_3000_no_outliers/output")

df <- read_rds("../../../../../data/interim/2015/hyb_by_mon_calib_wide_shifted.rds")
variance_var <- names(which(map_dbl(df[,16:255], var, na.rm = TRUE) != 0))
min_vars <- str_subset(variance_var, "min")
runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min_vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, seed = 5001)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)

outlier_list <- list()
while (any(clusObj$clusterSizes <= 3 )) {
    outlier_clusters <- which(clusObj$clusterSizes <= 3, useNames = TRUE)
    outlier <- bind_cols(clus = clusObj$clustering, yield = clusObj$clusObjRunInfoObj$yMat[,1], pedi = clusObj$clusObjRunInfoObj$xMat[,1]) %>% filter(clus %in% outlier_clusters) %>% select(yield, pedi)
    outlier_list <- c(outlier_list, outlier)
    update <- bind_cols(yield = clusObj$clusObjRunInfoObj$yMat[,1], clusObj$clusObjRunInfoObj$xMat) %>% filter(!(pedi == outlier$pedi & yield == outlier$yield)) %>% modify_at(2, as.character) %>% modify_at(2,as_factor)
    
    set.seed(5000)
    runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min_vars, data = update, nSweeps = 3000, nBurn = 50, nProgress = 100, seed = 5001)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
    # if (length(clusObj$clusterSizes) >= 3) 
    #     break
    print(clusObj$clusterSizes)
}

write_rds(clusObj, "../clusObj.rds", compress = "xz")
write_rds(outlier_list, "../outlier_list.rds", compress = "xz")


#outlier_full <- (unlist(outlier_list))[11:20] 
#outlier_full <- rbind(names(outlier_full), unname(outlier_full)) %>% #as_tibble()
#outlier_yields <-  outlier_full[2,str_which(outlier_full[1,], "yield")]
# outliers_final <- df[match(outlier_yields, df$yield),]
# df_no_outliers <- df[-match(outlier_yields, df$yield),]
# 
# df_clus <- cbind(cluster = clusObj$clustering, df_no_outliers)
