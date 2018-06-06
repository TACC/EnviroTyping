library(PReMiuM)
library(tidyverse)
library(broom)

setwd("~/github/EnviroTyping/sandbox/shifted_no_replicates/output")

df <- read_rds("../../../data/interim/2015/hybrid_by_month_shift_all_stats.rds") %>% filter(Repl == 1)

variance_var <- names(which(map_dbl(df[,17:189], var, na.rm = TRUE) != 0))
min_vars <- str_subset(variance_var, "Min")

set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min_vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, nClusInit = 1000)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)

clus_1 <- bind_cols(clus = clusObj$clustering, df) %>% filter(clus == 1)
table(clus_1$StatID)
clus_2 <- bind_cols(clus = clusObj$clustering, df) %>% filter(clus == 2)
table(clus_2$StatID)

min_loc <- str_which(names(clus_1), "Min")
summ_clus_1 <- clus_1 %>% select(Yield, contains("Min")) %>% map(~tidy(summary(.x))) %>% do.call(rbind,.)
summ_clus_2 <- clus_2 %>% select(Yield, contains("Min")) %>% map(~tidy(summary(.x))) %>% do.call(rbind,.)

pedi_matches <- clus_1$Pedi %in% clus_2$Pedi
pedi_matched <- match(clus_1$Pedi, clus_2$Pedi)

table(pedi_matches)["TRUE"]
length(pedi_matches)
n_distinct(df$Pedi)
