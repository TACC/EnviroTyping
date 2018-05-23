library(PReMiuM)

setwd("5fold_cv_sim/")
inputs <- generateSampleDataFile(clusSummaryNormalNormal())

set.seed(1234)
train_index <- sample(1:nrow(inputs$inputData), 0.5 * nrow(inputs$inputData))
test_index <- setdiff(1:nrow(inputs$inputData), train_index)
trainSim <- inputs$inputData[train_index,]
testSim <- inputs$inputData[test_index,]

runInfoObj <- profRegr(yModel=inputs$yModel, xModel=inputs$xModel,nSweeps=1000, nBurn=1000, data= trainSim, output="output", covNames=inputs$covNames, predict= testSim, fixedEffectsNames = inputs$fixedEffectNames, seed = 1234)
dissimObj <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(dissimObj)
riskProfileObj <- calcAvgRiskAndProfile(clusObj)
predictions <- calcPredictions(riskProfileObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
print(rsqrd <- 1-(sum((predictions$observedY - predictions$predictedY)^2)/sum((predictions$observedY - mean(predictions$observedY))^2))) # 0.8964785
print(predictions$rmse) # 1.295523

nfolds=5
set.seed(1234)
foldi=sample(rep(1:nfolds,length.out=length(inputs$inputData$Variable1)))
table(foldi)

rSqrd=NULL
predErr=NULL
for(k in 1:nfolds){
    testi=which(foldi==k)
    train=inputs$inputData[-testi,]
    test=inputs$inputData[testi,]
    
    runInfoObj <- profRegr(yModel=inputs$yModel, xModel=inputs$xModel,nSweeps=1000, nBurn=1000, data=train, output="output", covNames=inputs$covNames, predict = test, fixedEffectsNames = inputs$fixedEffectNames, seed = 1234)
    dissimObj <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(dissimObj)
    riskProfileObj <- calcAvgRiskAndProfile(clusObj)
    predictions <- calcPredictions(riskProfileObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
    print(rsqrd <- 1-(sum((predictions$observedY - predictions$predictedY)^2)/sum((predictions$observedY - mean(predictions$observedY))^2)))
    print(predictions$rmse)
    predErr=c(predErr,predictions$rmse)
    rSqrd=c(rSqrd,rsqrd)
}
(avg_rSqrd <- mean(rSqrd)) # 0.8904863
(avg_predErr <- mean(predErr)) # 1.299396
