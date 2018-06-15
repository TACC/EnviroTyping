library(PReMiuM)
library(tidyverse)
library(broom)
library(magrittr)

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

summ_clus_1 <- clus_1 %>% select(Yield, contains("Min")) %>% map(~tidy(summary(.x))) %>% do.call(rbind,.)
summ_clus_2 <- clus_2 %>% select(Yield, contains("Min")) %>% map(~tidy(summary(.x))) %>% do.call(rbind,.)

pedi_matches <- clus_1$Pedi %in% clus_2$Pedi
pedi_matched <- match(clus_1$Pedi, clus_2$Pedi)
clus_1$Pedi[pedi_matched[!is.na(pedi_matched)]]

"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
clus_1$Pedi %w/o% clus_2$Pedi # -> keeps duplicates
clus_2$Pedi %w/o% clus_1$Pedi # -> keeps duplicates
setdiff(clus_1$Pedi, clus_2$Pedi) # -> unique values
intersect(clus_1$Pedi, clus_1$Pedi)

# Filter by station
stat800_1 <- filter(clus_1, StatID == 800)
stat800_2 <- filter(clus_2, StatID == 800)
stat800_1 %w/o% stat800_2
intersect(stat800_1, stat800_2)

summary(stat800_1$Yield); summary(stat800_2$Yield)
filter(df, StatID == 800) %>% summary(Yield)

# reprex for stack overflow
# library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
# library(broom)
# cyl_4 <- filter(mtcars, cyl == 4) %>% select(4:8) %>% map(~tidy(summary(.x))) %>% do.call(rbind,.) %>% rownames_to_column(var = "var")
# cyl_6 <- filter(mtcars, cyl == 6) %>% select(4:8) %>% map(~tidy(summary(.x))) %>% do.call(rbind,.) %>% rownames_to_column(var = "var")
# cyl_8 <- filter(mtcars, cyl == 8) %>% select(4:8) %>% map(~tidy(summary(.x))) %>% do.call(rbind,.) %>% rownames_to_column(var = "var")
# bind_rows(list("4" = cyl_4, "6" = cyl_6, "8" = cyl_8), .id = "cyl")


clus1_station <- clus_1 %>%
    select(StatID, Yield) %>% 
    group_by(StatID) %>%
    do(map_dfr(.[-1],~tidy(summary(.x)),.id="var"))
clus2_station <- clus_2 %>%
    select(StatID, Yield) %>% 
    group_by(StatID) %>%
    do(map_dfr(.[-1],~tidy(summary(.x)),.id="var"))
yield_comparison <- bind_rows(list(clus1_station, clus2_station), .id = "clus") %>% arrange(StatID)



