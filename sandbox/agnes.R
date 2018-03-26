library(cluster)
library(tidyverse)
library(lubridate)

hybridByWeek <- read_csv("data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Exp" = col_factor(levels = NULL), "Pedi" = col_factor(levels = NULL), "Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))


val <- grep("Min|Max",names(hybridByWeek))
numericVars <- names(hybridByWeek[val])[vapply(hybridByWeek[val], function(x) var(x) != 0, logical(1))]
hybridByWeekSubset <- hybridByWeek %>% select(Hyb=Pedi, Yield, Week, numericVars)
riskProfObj <- readRDS("data/interim/G2F_Hybrid/hyb_wks3-5_by_week_1k/output/riskProfObj.rda")
weeks3_5 <- hybridByWeek %>% filter(Week >= isoweek(Planted) + 3 & Week <= isoweek(Planted) +5) %>% select(Pedi,Yield, numericVars)
weeks3_5Clust <- data.frame(optClus = as.factor(riskProfObj$riskProfClusObj$clustering), weeks3_5)


