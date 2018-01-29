library(profr)
library(PReMiuM)
library(ggplot2)
set.seed(1234)
start.time = Sys.time()
##########classNum is number of known clusters data is in, default is 5
prem.bench <- function(classNum = 5){
    function.list = c("clusSummaryBernoulliDiscrete", "clusSummaryBernoulliNormal", "clusSummaryBernoulliDiscreteSmall","clusSummaryCategoricalDiscrete","clusSummaryNormalDiscrete","clusSummaryNormalNormal","clusSummaryNormalNormalSpatial","clusSummaryVarSelectBernoulliDiscrete","clusSummaryBernoulliMixed")
    final.out = NULL
    for (i in function.list){
        set.seed(0001)
        inputs = do.call("generateSampleDataFile",list(do.call(i,list())))
        popSize = dim(inputs$inputData)[1]
        clusSizes = c(sample(1:classNum,popSize,replace = TRUE,prob = c(sample(20:80, classNum,replace = TRUE)/100)))
        table(clusSizes)
        known = NULL
        for (j in 1:classNum){
            newdat = rep(paste("Known",j),table(clusSizes)[j])
            known = c(known, newdat)
        }
        function.vector = c(rep(i,length(known)))
        if (exists("fixedEffectsNames",where = inputs)){
            runInfoObj<-profRegr(yModel=inputs$yModel, xModel=inputs$xModel, nSweeps=100, nClusInit=15,nBurn=300, data=inputs$inputData, output="output",covNames = inputs$covNames,fixedEffectsNames = inputs$fixedEffectNames, seed=12345)
        } else {
            if (exists("discreteCovs",where = inputs)){
                runInfoObj<-profRegr(yModel=inputs$yModel, xModel=inputs$xModel, nSweeps=100, nClusInit=15,nBurn=300, data=inputs$inputData, output="output",covNames = inputs$covNames,discreteCovs = inputs$discreteCovs, continuousCovs = inputs$continuousCovs, seed=12345)
            } else {
                runInfoObj<-profRegr(yModel=inputs$yModel, xModel=inputs$xModel, nSweeps=100, nClusInit=15,nBurn=300, data=inputs$inputData, output="output",covNames = inputs$covNames, seed=12345)
            }
        }
        dissimObj<-calcDissimilarityMatrix(runInfoObj)
        clusObj<-calcOptimalClustering(dissimObj,maxNClusters =7)
        riskProfileObj<-calcAvgRiskAndProfile(clusObj)
        clusterOrderObj<-plotRiskProfile(riskProfileObj,"summary2.png")
        optAlloc<-clusObj$clustering
        tmp_boxplot<-data.frame(opt=as.factor(optAlloc),outcome=inputs$inputData$outcome,known=as.factor(known))
        final.out = rbind(final.out,cbind(function.vector,tmp_boxplot))
    }
    return(final.out)
}

tester = prem.bench()

end.time = Sys.time()

end.time-start.time




BernoulliDiscrete = tester[which(tester == "clusSummaryBernoulliDiscrete"),-1]
BernoulliNormal = tester[which(tester == "clusSummaryBernoulliNormal"),-1]
BernoulliDiscreteSmall = tester[which(tester == "clusSummaryBernoulliDiscreteSmall"),-1]
CategoricalDiscrete = tester[which(tester == "clusSummaryCategoricalDiscrete"),-1]
NormalDiscrete = tester[which(tester == "clusSummaryNormalDiscrete"),-1]
NormalNormal = tester[which(tester == "clusSummaryNormalNormal"),-1]
NormalNormalSpatial = tester[which(tester == "clusSummaryNormalNormalSpatial"),-1]
VarSelectBernoulliDiscrete = tester[which(tester == "clusSummaryVarSelectBernoulliDiscrete"),-1]
BernoulliMixed = tester[which(tester == "clusSummaryBernoulliMixed"),-1]



# tester = profr(prem.bench())
# ggplot(tester)

