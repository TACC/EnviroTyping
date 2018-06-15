library(tidyverse)
library(PReMiuM)
library(broom)

setwd("~/github/EnviroTyping/sandbox/building_clusters/output")

df <- read_rds("../../../data/interim/2015/hyb_by_mon_calib_wide_shifted.rds")


################try 1

cont_vars = str_subset(names(df), "humid_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed=5000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)

#$nClusters
#[1] 14

#$clusterSizes
#[1] 1500  493  100  195  100  100   99   95   95   94   94  499  491  491





################ try 2

cont_vars = str_subset(names(df), "humid_mean|soil_moist_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed=5000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)

#$nClusters
#[1] 13
#
#$clusterSizes
#[1]  500  984  100  100  100  100  193   95   95   95   94 1499  491

################ try 3

cont_vars = str_subset(names(df), "humid_mean|dew_mean|rain_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed=5000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)

#$nClusters
#[1] 10
#
#$clusterSizes
#[1] 1991  593  100  200   99   95   95   95   94 1084

############### try 4
cont_vars = str_subset(names(df), "dewMean")

runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = cont_vars, data = df, nSweeps = 500, nBurn = 50, nProgress = 50, seed=5000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)

clus_df <- cbind(cluster = clusObj$clustering, select(df, yield, pedi, stat_id, contains("humid_mean")))

count_by_clust_stat_id <- clus_df %>% select(cluster, stat_id) %>% group_by(cluster, stat_id) %>% tally()

summary_clus_df <- clus_df %>%
    select(1,4,2,4:9) %>%
    group_by(cluster, StatID) %>%
    do(map_dfr(.[-c(1:2)],~tidy(summary(.x)),.id="var")) %>%
    ungroup %>% 
    arrange(var, cluster, StatID, mean)


#rm all NAs

df1 = df[,colSums(is.na(df)) == 0]

cont_vars = str_subset(names(df1), "humid_mean|soil_moist_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df1, nSweeps = 500, nBurn = 50, nProgress = 50, seed=5000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)

#$nClusters
#[1] 2

#$clusterSizes
#[1] 4445    1

cont_vars = str_subset(names(df1), "humid_mean|dew_mean|rain_mean")

runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = cont_vars, data = df1, nSweeps = 500, nBurn = 50, nProgress = 50, seed=5000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)

#$nClusters
#[1] 3

#$clusterSizes
#[1] 1481 1993  972

clus_df <- cbind(cluster = clusObj$clustering, select(df1, yield, pedi, stat_id,str_subset(names(df1), "humid_mean|dew_mean|rain_mean")))

count_by_clust_stat_id <- clus_df %>% select(cluster, stat_id) %>% group_by(cluster, stat_id) %>% tally()

summary_clus_df <- clus_df %>%
    select(1,4,2,4:19) %>%
    group_by(cluster, stat_id) %>%
    do(map_dfr(.[-c(1:2)],~tidy(summary(.x)),.id="var")) %>%
    ungroup %>% 
    arrange(var, cluster, stat_id, mean)

summary(summary_clus_df) %>% filter(var == "yield")
