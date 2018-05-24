library(RRF); # install RRF package before running the script
library(tidyverse)
library(tictoc)
setwd("sandbox/G2F/")

hybridByWeek <- read_csv("../../data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Exp" = col_factor(levels = NULL),"Pedi" = col_factor(levels = NULL), "Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))

val <- grep("Min|Max",names(hybridByWeek))
numericVars <- names(hybridByWeek[val])[vapply(hybridByWeek[val], function(x) var(x) != 0, logical(1))]

hybridByWeekSubset <- hybridByWeek %>% select(Exp, Hyb=Pedi, Yield, numericVars)
hybridByWeekSubset$Hyb <- as.numeric(hybridByWeekSubset$Hyb)
hybridByWeekSubset$Exp <- as.numeric(hybridByWeekSubset$Exp)

x.matrix <- scale(as.matrix(select(hybridByWeekSubset, Exp, Hyb, numericVars)))
y.vector <- hybridByWeekSubset$Yield
lambda <- 0.8
rrf <- RRF(x=x.matrix,y=y.vector,flagReg=1,coefReg=lambda, importance = TRUE, keep.forest = TRUE)
subsetRRF <- rrf$feaSet

rf <- RRF(x=x.matrix, y=y.vector, flagReg = 0, do.trace = TRUE)
impRF <- rf$importance
impRF <- impRF[,1] # get the importance score 
imp <- impRF/(max(impRF))
gamma <- 0.5   #A larger gamma often leads to fewer features. But, the quality of the features selected is quite stable for GRRF, i.e., different gammas can have similar accuracy performance (the accuracy of an ordinary RF using the feature subsets). See the paper for details. 
coefReg <- (1-gamma) + gamma*imp   # each variable has a coefficient, which depends on the importance score from the ordinary RF and the parameter: gamma
tic()
grrf <- RRF(x=x.matrix,y=y.vector, flagReg=1, coefReg=coefReg, do.trace = TRUE, keep.forest = TRUE)
toc()
subsetGRRF <- grrf$feaSet # produce the indices of the features selected by GRRF

print(subsetRRF) #the subset includes many more noisy variables than GRRF
print(subsetGRRF)
