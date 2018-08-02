library(tidyverse)
library(PReMiuM)

setwd("~/github/EnviroTyping/sandbox/shifted_data_analysis/2015/min_vars_3000/output")

df <- read_rds("../../../../../data/interim/2015/hybrid_by_month_shift_all_stats.rds")

min_3K_15 <- read_rds("../riskProfObj.rds")
pedi_stat <- read_rds("../../../../../data/interim/2015/hybrid_by_month_shift_all_stats.rds") %>% select(Pedi, StatID)


clus_1 <- bind_cols(clus = min_3K_15$riskProfClusObj$clustering, yield = min_3K_15$riskProfClusObj$clusObjRunInfoObj$yMat, pedi_stat, min_3K_15$riskProfClusObj$clusObjRunInfoObj$xMat[,-1]) %>% filter(clus == 1)

cont_vars <- names(clus_1[4:43])

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = cont_vars, data = clus_1, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj_1 <- calcOptimalClustering(calcDists)

clus_1_1 <- bind_cols(clus = clusObj_1$clustering, clus_1[,-1]) %>% filter(clus == 1)
clus_1_2 <- bind_cols(clus = clusObj_1$clustering, clus_1[,-1]) %>% filter(clus == 2)

table(clus_1_1$StatID)
table(clus_1_2$StatID)

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = cont_vars, data = clus_1_1, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj_1_1 <- calcOptimalClustering(calcDists)

clus_1_1_1 <- bind_cols(clus = clusObj_1_1$clustering, clus_1_1[,-1]) %>% filter(clus == 1)
clus_1_1_2 <- bind_cols(clus = clusObj_1_1$clustering, clus_1_1[,-1]) %>% filter(clus == 2)

table(clus_1_1_1$StatID); n_distinct(clus_1_1_1$Pedi)
table(clus_1_1_2$StatID); n_distinct(clus_1_1_2$Pedi)


df_clus <- bind_cols(clus = min_3K_15$riskProfClusObj$clustering, df)

clus_1 <- df_clus %>% filter(clus == 1)
clus_2 <- df_clus %>% filter(clus == 2)

yield_clus_1 <- clus_1 %>%
    select(StatID, Yield, contains("Min")) %>% 
    group_by(StatID) %>%
    do(map_dfr(.[-1],~tidy(summary(.x)),.id="var"))
yield_clus_2 <- clus_2 %>%
    select(StatID, Yield, contains("Min")) %>% 
    group_by(StatID) %>%
    do(map_dfr(.[-1],~tidy(summary(.x)),.id="var"))
yield_comparison <- bind_rows(list(yield_clus_1, yield_clus_2), .id = "clus") %>% arrange(StatID, var)
setdiff(clus_1$Pedi, clus_2$Pedi)

pedi_clus <- df_clus %>% select(clus, StatID, Pedi, Yield, Repl)
