library(PReMiuM)
library(tidyverse)
library(broom)

setwd("~/github/EnviroTyping/sandbox/building_clusters/output")

df <- read_rds("~/github/EnviroTyping/data/interim/2015/hyb_by_mon_calib_wide_shifted.rds")

cont_vars <- str_subset(names(df), "dew_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed = 5000)

calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)
clusObj$nClusters; clusObj$clusterSizes 
# [1] 9
# [1]  491  491 1493  499  500  297  176  280  219


## 2 variables
cont_vars <- str_subset(names(df), "humid_mean|dew_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed = 5000)

calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)
clusObj$nClusters; clusObj$clusterSizes 
# [1] 10
# [1] 2484  200  199  100   95   95   95   94   94  990

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

clus_df_dew <- cbind(cluster = clusObj$clustering, select(df, yield, pedi, stat_id, contains("dew_mean")))

count_by_clust_stat_id_dew <- clus_df_dew %>% select(cluster, stat_id) %>% group_by(cluster, stat_id) %>% tally()

summary_clus_df_dew <- clus_df_dew %>%
    select(1,4,2,4:9) %>%
    group_by(cluster, stat_id) %>%
    do(map_dfr(.[-c(1:2)],~tidy(summary(.x)),.id="var")) %>%
    ungroup %>% 
    arrange(var, cluster, stat_id)

summary(summary_clus_df) %>% filter(var == "Yield")
