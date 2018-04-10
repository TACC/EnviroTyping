library(PReMiuM)
library(tidyverse)
library(paralel)
require(doMC)
require(foreach)
setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_wks3-5_by_week_1k_preds/output")

setwd("data/interim/G2F_Hybrid/hyb_wks3-5_by_week_1k_preds/")
setwd("./output")


inputs <- generateSampleDataFile(clusSummaryNormalNormal())
preds <- data.frame(matrix(c(2, 2, 2, 2, 2, 0, 0, NA, 0, 0),ncol=5,byrow=TRUE))
colnames(preds) <- names(inputs$inputData)[2:(inputs$nCovariates+1)]
runInfoObj <- profRegr(yModel=inputs$yModel, xModel=inputs$xModel,nSweeps=10000, nBurn=10000, data=inputs$inputData, output="output", covNames=inputs$covNames, predict=preds, fixedEffectsNames = inputs$fixedEffectNames)
dissimObj <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(dissimObj)
riskProfileObj <- calcAvgRiskAndProfile(clusObj)
predictions <- calcPredictions(riskProfileObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
predictions$predictedYPerSweep[1:10,,1]
plotPredictions(outfile="predictiveDensity.pdf",runInfoObj=runInfoObj, predictions=predictions,logOR=TRUE)

nfolds=5
set.seed(1)
foldi=sample(rep(1:nfolds,length.out=length(inputs$inputData$Variable1)))
table(foldi)

rSqrd=NULL
predErr=NULL
for(k in 1:nfolds){
        testi=which(foldi==k)
        train=inputs$inputData[-testi,]
        test=inputs$inputData[testi,]
        
        runInfoObj <- profRegr(yModel=inputs$yModel, xModel=inputs$xModel,nSweeps=1000, nBurn=500, data=train, output="output", covNames=inputs$covNames, predict = test, fixedEffectsNames = inputs$fixedEffectNames, seed = 1234)
        dissimObj <- calcDissimilarityMatrix(runInfoObj)
        clusObj <- calcOptimalClustering(dissimObj)
        riskProfileObj <- calcAvgRiskAndProfile(clusObj)
        predictions <- calcPredictions(riskProfileObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
        print(rsqrd <- 1-(sum((predictions$observedY - predictions$predictedY)^2)/sum((predictions$observedY - mean(predictions$observedY))^2)))
        print(prederr <- sqrt(1/(dim(test)[1])*sum((predictions$observedY - predictions$predictedY)^2)))
        predErr=c(predErr,prederr)
        rSqrd=c(rSqrd,rsqrd)
}
(avg_rSqrd <- mean(rSqrd))
(avg_predErr <- mean(predErr))


df <- read_rds("../../hybrid_by_month_calibrated_weather.rds")
subset <- df[sample(1:nrow(df),.1*dim(df)[1]),]

# find continous variables with variance
val <- grep("Median",names(subset))
numericVars <- names(which(map_dbl(subset[val], var, na.rm = TRUE) != 0))

nfolds=5
set.seed(1)
foldi=sample(rep(1:nfolds,length.out=length(subset$Pedi)))
table(foldi)

rSqrd=NULL
predErr=NULL
for(k in 1:nfolds){
    testi=which(foldi==k)
    train=subset[-testi,]
    test=subset[testi,]
    runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed",discreteCovs = "Pedi", continuousCovs = numericVars, data = train, predict = test, nSweeps = 100, nBurn = 100, seed = 1234)
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
mean(predictions$predictedY)
mean(predictions$observedY)


val <- grep("Min|Max",names(df))
numericVars <- names(which(map_dbl(df[val], var) != 0))

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
mean(predictions$predictedY)
mean(predictions$observedY)


system('top -b -n 1 -u $USER', intern=TRUE)

registerDoMC(cores = 4)
nfolds=5
set.seed(1)
foldi=sample(rep(1:nfolds,length.out=length(inputs$inputData$Variable1)))
table(foldi)

rSqrd=NULL
predErr=NULL
foreach(k = 1:nfolds) %dopar% {
    testi=which(foldi==k)
    train=inputs$inputData[-testi,]
    test=inputs$inputData[testi,]
    runInfoObj <- profRegr(yModel=inputs$yModel, xModel=inputs$xModel,nSweeps=1000, nBurn=1000, data=train, output="output", covNames=inputs$covNames, predict = test, fixedEffectsNames = inputs$fixedEffectNames, seed = 1234)
    dissimObj <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(dissimObj)
    riskProfileObj <- calcAvgRiskAndProfile(clusObj)
    predictions <- calcPredictions(riskProfileObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
    print(rsqrd <- 1-(sum((predictions$observedY - predictions$predictedY)^2)/sum((predictions$observedY - mean(predictions$observedY))^2)))
    print(prederr <- sqrt(1/(dim(test)[1])*sum((predictions$observedY - predictions$predictedY)^2)))
    #predErr=c(predErr,prederr)
    #rSqrd=c(rSqrd,rsqrd)
}
(avg_rSqrd <- mean(rSqrd))
(avg_predErr <- mean(predErr))