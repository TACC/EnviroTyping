library(PReMiuM)
library(tidyverse)
library(tictoc)

setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_by_month_preds/complete45_long")

df <- read_rds("../../hybrid_by_month_calibrated_weather_long_45subset.rds")
#df45long <- df45long[sample(1:nrow(df45long),.2*dim(df45long)[1]),]

set.seed(1234)
train_index <- sample(1:nrow(df45long), 0.5 * nrow(df))
test_index <- setdiff(1:nrow(df45long), train_index)
train45 <- df45long[train_index,]
test45 <- df45long[test_index,]

contVars <- names(which(map_dbl(train45[,17:25], var, na.rm = TRUE) != 0))
discrVars <- c("Pedi", "Month")
tic()
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed",discreteCovs = discrVars, continuousCovs = contVars, data = train45, predict = test45, nSweeps = 1000, nBurn = 99000, seed = 1234)
print(toc())
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
predictions <- calcPredictions(riskProfObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
print(rsqrd <- 1-(sum((predictions$observedY - predictions$predictedY)^2)/sum((predictions$observedY - mean(predictions$observedY))^2)))
print(predictions$rmse)

nfolds=5
set.seed(1)
foldi=sample(rep(1:nfolds,length.out=length(df$Pedi)))
table(foldi)

rSqrd=NULL
predErr=NULL
for(k in 1:nfolds){
    testi=which(foldi==k)
    train=df[-testi,]
    test=df[testi,]
    runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed",discreteCovs = "Pedi", continuousCovs = numericVars, data = train, predict = test, nSweeps = 1000, nBurn = 1000, seed = 1234)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
    riskProfObj <- calcAvgRiskAndProfile(clusObj)
    predictions <- calcPredictions(riskProfObj,doRaoBlackwell = F, fullSweepPredictions = F,fullSweepLogOR = T)
    print(rsqrd <- 1-(sum((predictions$observedY - predictions$predictedY)^2)/sum((predictions$observedY - mean(predictions$observedY))^2)))
    print(predictions$rmse)
    predErr=c(predErr,predictions$rmse)
    rSqrd=c(rSqrd,rsqrd)
}
(avg_rSqrd <- mean(rSqrd))
(avg_predErr <- mean(predErr))

save(file = "complete45_long.rds")
