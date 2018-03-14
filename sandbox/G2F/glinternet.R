library(glinternet)
library(readr)
library(dplyr)
library(forcats)
setwd("sandbox/G2F/hierNet")


hybridByWeek <- read_csv("../../../data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Exp" = col_factor(levels = NULL), "Pedi" = col_factor(levels = NULL), "Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))

val <- grep("Min|Max",names(hybridByWeek))
numericVars <- names(hybridByWeek[val])[vapply(hybridByWeek[val], function(x) var(x) != 0, logical(1))]
hybridByWeekSubset <- hybridByWeek %>% select(Exp, Hyb =Pedi,Yield, numericVars)
hybridByWeekSubset$Hyb <- as.integer(as_factor(hybridByWeekSubset$Hyb))
hybridByWeekSubset$Exp <- as.integer(as_factor(hybridByWeekSubset$Exp))
x.matrix <- as.matrix(select(hybridByWeekSubset, Exp, Hyb, numericVars))
y.vector <- hybridByWeekSubset$Yield
numLevels <- c(length(unique(x.matrix[,1])),length(unique(x.matrix[,2])),rep(1,20))
numLevels <- c(rep(1,22))
glinFit <- glinternet(x.matrix, y.vector, numLevels)
