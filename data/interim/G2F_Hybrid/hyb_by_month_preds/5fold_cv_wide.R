library(PReMiuM)
library(tidyverse)

setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_wks3-5_by_week_1k_preds/output")

setwd("data/interim/G2F_Hybrid/hyb_by_month_preds/output")


df <- read_rds("../../hybrid_by_month_calibrated_weather_wide_months_5-7.rds")
subset <- df[sample(1:nrow(df),.2*dim(df)[1]),]

# find continous variables with variance
val <- grep("Min|Max",names(subset))
numericVars <- names(which(map_dbl(subset[val], var) != 0))

nfolds=5
set.seed(1)
foldi=sample(rep(1:nfolds,length.out=length(subset$Pedi)))
table(foldi)

rSqrd=NULL
predErr=NULL

for(k in 1:nfolds){
    testi=which(foldi == 1)
    train=subset[-testi,]
    test=subset[testi,]
    runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed",discreteCovs = "Pedi", continuousCovs = numericVars, data = train, predict = test, nSweeps = 100, nBurn = 100, seed = 1234)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists, maxNClusters = 9)
    riskProfObj <- calcAvgRiskAndProfile(clusObj)
    predictions <- calcPredictions(riskProfObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
    print(rsqrd <- 1-(sum((predictions$observedY - predictions$predictedY)^2)/sum((predictions$observedY - mean(predictions$observedY))^2)))
    print(prederr <- sqrt(1/(dim(test)[1])*sum((predictions$observedY - predictions$predictedY)^2)))
    predErr=c(predErr,prederr)
    rSqrd=c(rSqrd,rsqrd)
}
(avg_rSqrd <- mean(rSqrd))
(avg_predErr <- mean(predErr))

df45 <- read_rds("../../hybrid_by_month_calibrated_weather_wide_45subset.rds")

train_index <- sample(1:nrow(df45), 0.5 * nrow(df45))
test_index <- setdiff(1:nrow(df45), train_index)
train45 <- df45[train_index,]
test45 <- df45[test_index,]

contVars <- names(which(map_dbl(train45[,16:96], var, na.rm = TRUE) != 0))

runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed",discreteCovs = "Pedi", continuousCovs = contVars, data = train45, predict = test45, nSweeps = 100, nBurn = 100, seed = 1234)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists, maxNClusters = 9)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
predictions <- calcPredictions(riskProfObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
print(rsqrd <- 1-(sum((predictions$observedY - predictions$predictedY)^2)/sum((predictions$observedY - mean(predictions$observedY))^2)))
print(predictions$)
