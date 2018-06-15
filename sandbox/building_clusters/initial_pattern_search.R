library(PReMiuM)
library(tidyverse)
library(broom)
library(tictoc)

setwd("~/github/EnviroTyping/sandbox/building_clusters/output")

df <- read_rds("~/github/EnviroTyping/data/interim/2015/hyb_by_mon_calib_wide_shifted_front.rds")

cont_vars <- str_subset(names(df), "dew_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed = 5000)

calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)
clusObj$nClusters; clusObj$clusterSizes 
# [1] 9
# [1]  491  491 1493  499  500  297  176  280  219


## 2 variables
cont_vars <- str_subset(names(df), "humid_mean|soil_moist_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed = 5000)

calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)
clusObj$nClusters; clusObj$clusterSizes 
# [1] 6 "humid_mean|dew_mean"
# [1] 1491  991  493  499  473  499


## 3 variables
cont_vars <- str_subset(names(df), "humid_mean|dew_mean|rain_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed = 5000)

calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)
clusObj$nClusters; clusObj$clusterSizes 
# [1] 10
# [1] 2482 1092  100  200   99   95   95   95   94   94

## 4 variables
cont_vars <- str_subset(names(df), "humidMean|dewMean|rainMean|soilTempMean")

runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed = 5000)

calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)
clusObj$nClusters; clusObj$clusterSizes 
# [1] 2
# [1] 3763  683

## 1 variables
cont_vars <- str_subset(names(df), "solarMean")

runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed = 5000)

calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)
clusObj$nClusters; clusObj$clusterSizes 
# [1] 15 soilMoistMean
# [1]  500  493  100  100  100  100   99   95   95   95   94   94 1491  499  491
# [1] 14 solarMean
# [1] 1442   58  984  100  100  100  100   99   95   95  586   94   94  499

clus_df <- cbind(cluster = clusObj$clustering, select(df, Yield, Pedi, StatID, contains("solarMean")))

count_by_clust_stat_id <- clus_df %>% select(cluster, StatID) %>% group_by(cluster, StatID) %>% tally()

summary_clus_df <- clus_df %>%
    select(1,4,2,4:9) %>%
    group_by(cluster, StatID) %>%
    do(map_dfr(.[-c(1:2)],~tidy(summary(.x)),.id="var")) %>%
    ungroup %>% 
    arrange(var, cluster, StatID, mean)

summary(summary_clus_df) %>% filter(var == "Yield")

df_14 <- read_rds("~/github/EnviroTyping/data/interim/2014/hyb_by_mon_calib_wide_shifted.rds")

## 1 variables
cont_vars <- str_subset(names(df_14), "solar_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df_14, nSweeps = 500, nBurn = 50, nProgress = 50, seed = 5000)

calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)
clusObj$nClusters; clusObj$clusterSizes 
# [1] 15 soilMoistMean
# [1]  500  493  100  100  100  100   99   95   95   95   94   94 1491  499  491
# [1] 14 solarMean
# [1] 1442   58  984  100  100  100  100   99   95   95  586   94   94  499

update <- filter(df, yield %in% c(100.2706, 132.3455))
clus_df <- cbind(cluster = clusObj$clustering, select(update, yield, pedi, stat_id, contains("humid_mean|soil_moist_mean")))

count_by_clust_stat_id <- clus_df %>% select(cluster, stat_id) %>% group_by(cluster, stat_id) %>% tally()

summary_clus_df_dew <- clus_df_dew %>%
    select(1,4,2,4:9) %>%
    group_by(cluster, stat_id) %>%
    do(map_dfr(.[-c(1:2)],~tidy(summary(.x)),.id="var")) %>%
    ungroup %>% 
    arrange(var, cluster, stat_id)

summary(summary_clus_df) %>% filter(var == "Yield")


# Filter outliers

## 2 variables
cont_vars <- str_subset(names(df), "humid_mean|soil_moist_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed = 5000)

calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)
clusObj$nClusters; clusObj$clusterSizes 
# [1] 6 "humid_mean|dew_mean"
# [1] 1491  991  493  499  473  499
outlier_list <- list()
while (any(clusObj$clusterSizes <= 3 )) {
    outlier_clusters <- which(clusObj$clusterSizes <= 3, useNames = TRUE)
    outlier <- bind_cols(clus = clusObj$clustering, yield = clusObj$clusObjRunInfoObj$yMat[,1], pedi = clusObj$clusObjRunInfoObj$xMat[,1]) %>% filter(clus %in% outlier_clusters) %>% select(yield, pedi)
    outlier_list <- c(outlier_list, outlier)
    update <- bind_cols(yield = clusObj$clusObjRunInfoObj$yMat[,1], clusObj$clusObjRunInfoObj$xMat) %>% filter(!(pedi == outlier$pedi & yield == outlier$yield)) %>% modify_at(2, as.character) %>% modify_at(2,as_factor)
    
    set.seed(1234)
    runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = update, nSweeps = 500, nBurn = 50, nProgress = 100, seed = 5000)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
    # if (length(clusObj$clusterSizes) >= 3) 
    #     break
    print(clusObj$clusterSizes)
}
