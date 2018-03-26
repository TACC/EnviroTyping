#install.packages("hierNet")
library(hierNet)
#library(glinternet)
library(tidyverse)
library(tictoc)

#setwd("sandbox/G2F/variableSelection/")
setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/sandbox/variableSelection")

hybridByWeek <- read_csv("../../data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Exp" = col_factor(levels = NULL),"Pedi" = col_factor(levels = NULL), "Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))

val <- grep("Min|Max",names(hybridByWeek))

numericVars <- names(hybridByWeek[val])[vapply(hybridByWeek[val], function(x) var(x) != 0, logical(1))]

hybridByWeekSubset <- hybridByWeek %>% select(Exp, Hyb=Pedi, Yield, numericVars)
hybridByWeekSubset$Hyb <- as.numeric(hybridByWeekSubset$Hyb)
hybridByWeekSubset$Exp <- as.numeric(hybridByWeekSubset$Exp)

x.matrix <- scale(as.matrix(select(hybridByWeekSubset, Exp, Hyb, numericVars)))
y.vector <- hybridByWeekSubset$Yield
# set lambda to the Frobenius norm of the X matrix
# lambda <- norm(x.matrix, type = "F")

# set.seed(1234)
# tic()
# hierFit <- hierNet(x.matrix, y.vector, lam = lambda)
# toc()
# saveRDS(hierFit, "hierFitLamNorm.rda")
# 
# varimp <- hierNet.varimp(hierFit, x.matrix, y.vector)
# saveRDS(varimp, "varimpLabNorm.rda")
# print(varimp)
# 
# varimpRank <- varimp[order(varimp[,1]),]
# print(varimpRank)
# 
# varRank <- data.frame(names(hybridByWeekSubset[-3]),varimp) %>% arrange(Predictor)

# tic()
# hierFitPath <- hierNet.path(x.matrix, y.vector, minlam = 800, maxlam = 1500, maxiter = 5000)
# toc()
# saveRDS(hierFitPath, "hierFitPath.rda")

# hierFitPath <- readRDS("hierFitPath.rda")
# fitcv=hierNet.cv(hierFitPath,x.matrix,y.vector)
# saveRDS(fitcv, "fitcv.rda")

fitcv <- readRDS("fitcv.rda")
lamhat=fitcv$lamhat.1se
fitcv$lamlist

fit2=hierNet(x = x.matrix, y = y.vector, lam=lamhat, maxiter = 5000)

yhat=predict(fit2,newx = x.matrix)
saveRDS(yhat, "yhat.rda")

varImp <- hierNet.varimp(fit2, x = x.matrix, y = y.vector)
saveRDS(varImp, "varImp.rda")


