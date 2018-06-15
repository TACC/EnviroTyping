library(PReMiuM)
library(tidyverse)

setwd("~/github/EnviroTyping/sandbox/building_clusters/output")


big_sim <- read.table("~/github/EnviroTyping/Simulated-data-for-performance-testing/sim_large_dataset_NormalMixed.dat", header = TRUE)

small_sim <- big_sim %>% select(1:3, 32:33)

small_sim_.2_na <- bind_cols(outcome = small_sim[,1], map_df(small_sim[,-1], function(x) {x[sample(c(TRUE, NA), prob = c(0.8, 0.2), size = length(x), replace = TRUE)]}))


missing <- small_sim_.2_na %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))


discr_covs <- str_subset(names(small_sim_.2_na), "Discr")
cont_covs <- str_subset(names(small_sim_.2_na), "Cont")

runInfoObj <- profRegr(outcome = "outcome", discreteCovs = discr_covs, continuousCovs = cont_covs, data = small_sim_.2_na, nSweeps = 500, nBurn = 50, seed = 5000, yModel = "Normal", xModel = "Mixed")
calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)

small_sim_.35_na <- bind_cols(outcome = small_sim[,1], map_df(small_sim[,-1], function(x) {x[sample(c(TRUE, NA), prob = c(0.65, 0.35), size = length(x), replace = TRUE)]}))

missing_.3<- small_sim_.3_na %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

runInfoObj <- profRegr(outcome = "outcome", discreteCovs = discr_covs, continuousCovs = cont_covs, data = small_sim_.35_na, nSweeps = 500, nBurn = 50, seed = 5000, yModel = "Normal", xModel = "Mixed")
calcDists <- calcDissimilarityMatrix(runInfoObj)
set.seed(1234)
clusObj <- calcOptimalClustering(calcDists)
