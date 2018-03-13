#install.packages("hierNet")
library(hierNet)
library(tidyverse)

setwd("sandbox/G2F/hierNet")


weeks3_5 <- read_csv("../../../data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))

val <- grep("Min|Max",names(weeks3_5))

numericVars <- names(weeks3_5[val])[vapply(weeks3_5[val], function(x) var(x) != 0, logical(1))]

weeks3_5 <- weeks3_5 %>% select(Exp, Hyb =Pedi,Yield, numericVars)
weeks3_5$Hyb <- as.numeric(as_factor(weeks3_5$Hyb))
weeks3_5$Exp <- as.numeric(as_factor(weeks3_5$Exp))
set.seed(1234)
x.matrix <- as.matrix(select(weeks3_5, Exp, Hyb, numericVars))
y.vector <- weeks3_5$Yield
model <- hierNet(x.matrix, y.vector, lam = 50)
print(model)
