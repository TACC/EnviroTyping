setwd("~/GitHub/EnviroTyping/md_doc_files/manual_plots/output/")
library(PReMiuM)
set.seed(1234)
inputs <- generateSampleDataFile(clusSummaryBernoulliDiscrete())
runInfoObj<-profRegr(yModel=inputs$yModel,
                     xModel=inputs$xModel, nSweeps=100, nClusInit=15,
                     nBurn=300, data=inputs$inputData, output="output",
                     covNames = inputs$covNames,
                     fixedEffectsNames = inputs$fixedEffectNames, seed=12345)
dissimObj <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(dissimObj, maxNClusters = 7)
riskProfileObj <- calcAvgRiskAndProfile(clusObj)
clusterOrderObj<-plotRiskProfile(riskProfileObj,"summary.png")


known <- c(rep("A",600),rep("B",600), rep("C",300),rep("D",500),rep("E",1000))
set.seed(12578)
inputs <- generateSampleDataFile(clusSummaryNormalDiscrete())
runInfoObj<-profRegr(yModel=inputs$yModel,
                     xModel=inputs$xModel, nSweeps=100, nClusInit=15,
                     nBurn=300, data=inputs$inputData, output="output",
                     covNames = inputs$covNames,
                     fixedEffectsNames = inputs$fixedEffectNames, seed=12345)
dissimObj <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(dissimObj, maxNClusters = 7)
riskProfileObj <- calcAvgRiskAndProfile(clusObj)
clusterOrderObj <- plotRiskProfile(riskProfileObj, "summary2.png")
optAlloc<-clusObj$clustering
library(ggplot2)
tmp_boxplot <- data.frame(opt=as.factor(optAlloc), outcome=inputs$inputData$outcome, known=as.factor(known))
p <- ggplot(tmp_boxplot, aes(x=known, y=outcome, fill=opt)) +
    geom_violin()+
    labs(title="",x="Known Truth", y = "outcome") +
    facet_grid(~known,scales='free',space='free') +
    guides(fill=guide_legend(title="Clusters")) +
    theme(strip.text.x = element_blank(), strip.background = element_blank())
# Use brewer color palettes
p + scale_fill_brewer(palette="Set1")

# Construct the number of clusters file name
nClustersFileName<-file.path(runInfoObj$directoryPath, paste(runInfoObj$fileStem,'_nClusters.txt',sep=''))
nClustersFile <- file(nClustersFileName, open="r")
# Construct the allocation file name
zFileName <- file.path(runInfoObj$directoryPath, paste(runInfoObj$fileStem,'_z.txt', sep=''))
zFile <- file(zFileName, open="r")
# Construct the theta file name
thetaFileName <- file.path(runInfoObj$directoryPath, paste(runInfoObj$fileStem,'_theta.txt', sep=''))
thetaFile <- file(thetaFileName, open="r")
firstLine <- ifelse(runInfoObj$reportBurnIn, runInfoObj$nBurn/runInfoObj$nFilter+2,1)
lastLine <- (runInfoObj$nSweeps+ifelse(runInfoObj$reportBurnIn, runInfoObj$nBurn+1,0))/runInfoObj$nFilter
nSamples <- lastLine-firstLine
thetaByObs <- matrix(NA, ncol=runInfoObj$nSubjects, nrow=runInfoObj$nSweeps)

for(sweep in firstLine:lastLine){
    if(sweep == firstLine){
        skipVal <- firstLine - 1
    }else{
        skipVal <- 0
    }
    
    currMaxNClusters <- scan(nClustersFile, what=integer(), skip=skipVal,n=1,quiet=T)
    
    # Get the current allocation data for this sweep
    currZ <- scan(zFile, what=integer(), skip=skipVal, n=runInfoObj$nSubjects+runInfoObj$nPredictSubjects,quiet=T)
    currZ <- 1 + currZ
    
    # Get the risk data corresponding to this sweep
    currThetaVector <- scan(thetaFile, what=double(), skip=skipVal, n=currMaxNClusters, quiet=T)
    currTheta <- matrix(currThetaVector,ncol=1,byrow=T)
    
    for (i in 1:runInfoObj$nSubjects){
        thetaByObs[sweep,i] <- currTheta[currZ[i]]
    }
}

tmp_boxplot2 <- data.frame(opt=vector(), outcome=vector(), known=vector())
for (c in 1:max(optAlloc)){
    for (k in names(table(known))){
        tmp_index <- which(optAlloc == c & known == k)
        tmp_length <- length(tmp_index)
        if (tmp_length > 1){
            tmp_value<-apply(thetaByObs[,tmp_index],1,mean)
            tmp_boxplot3<-data.frame(opt=rep(c,runInfoObj$nSweeps), outcome=tmp_value, known=rep(k,runInfoObj$nSweeps))
            tmp_boxplot2<-rbind(tmp_boxplot2,tmp_boxplot3)
        }
        if (tmp_length == 1){
            tmp_boxplot3<-data.frame(opt=rep(c,runInfoObj$nSweeps), outcome=thetaByObs[,tmp_index], known=rep(k,runInfoObj$nSweeps))
            tmp_boxplot2<-rbind(tmp_boxplot2,tmp_boxplot3)
        }
    }
}
tmp_boxplot2$opt <- as.factor(tmp_boxplot2$opt)
tmp_boxplot2$known <- as.factor(tmp_boxplot2$known)
p <- ggplot(tmp_boxplot2, aes(x=known, y=outcome, fill=opt)) +
    geom_violin() +
    labs(title="",x="Known Truth", y = "outcome") +
    facet_grid(~ known,scales='free',space='free') +
    guides(fill=guide_legend(title="Clusters")) +
    theme(strip.text.x = element_blank(), strip.background = element_blank())
# Use brewer color palettes
p + scale_fill_brewer(palette="Set1")
# close all files
close(zFile)
close(nClustersFile)
close(thetaFile)



inputs <- generateSampleDataFile(clusSummaryNormalNormal())
runInfoObj<-profRegr(yModel=inputs$yModel,
                     xModel=inputs$xModel, nSweeps=100, nClusInit=15,
                     nBurn=300, data=inputs$inputData, output="output",
                     covNames = inputs$covNames,
                     seed=12345)
dissimObj <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(dissimObj,maxNClusters = 7)
riskProfileObj <- calcAvgRiskAndProfile(clusObj)
clusterOrderObj <- plotRiskProfile(riskProfileObj,"summary3.png")
profile <- riskProfileObj$profile
meanSortIndex <- clusterOrderObj
muArray <- data.frame(covariateNumber=vector(),clusterNumber=vector(), colourPlot=vector())
for(j in 1:runInfoObj$nCovariates){
    # Compute the means
    muMat <- profile[,meanSortIndex,j]
    muMeans <- apply(muMat,2,mean)
    muMean <- sum(muMeans*clusObj$clusterSizes)/sum(clusObj$clusterSizes)
    muLower <- apply(muMat,2,quantile,0.05)
    muUpper <- apply(muMat,2,quantile,0.95)
    # Get the plot colors
    muColor <- ifelse(muLower>rep(muMean, length(muLower)), "high", ifelse(muUpper<rep(muMean,length(muUpper)),"low","avg"))
    muArrayNew <- data.frame(covariateNumber=rep(j,clusObj$nClusters), clusterNumber=c(1:clusObj$nClusters), colourPlot=muColor)
    muArray <- rbind(muArray, muArrayNew)
}

# THIS IS NOT WORKING YET!!
ggplot(muArray, aes(x=clusterNumber, y=covariateNumber, fill=colourPlot)) +
    geom_col(stat="identity", colour="white")





inputs <- generateSampleDataFile(clusSummaryBernoulliDiscrete())
preds<-data.frame(matrix(c(2, 2, 2, 2, 2, 0, 0, NA, 0, 0),ncol=5,byrow=TRUE))
colnames(preds)<-names(inputs$inputData)[2:(inputs$nCovariates+1)]
runInfoObj<-profRegr(yModel=inputs$yModel, xModel=inputs$xModel,
                     nSweeps=10000, nBurn=10000, data=inputs$inputData, output="output",
                     covNames=inputs$covNames,predict=preds,
                     fixedEffectsNames = inputs$fixedEffectNames)
dissimObj <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(dissimObj)
riskProfileObj <- calcAvgRiskAndProfile(clusObj)
predictions <- calcPredictions(riskProfileObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
plotPredictions(outfile="predictiveDensity.png",runInfoObj=runInfoObj, predictions=predictions,logOR=TRUE)
