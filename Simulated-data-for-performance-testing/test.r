# test of the simulated data 

dataAll<-read.table("sim_large_dataset_NormalMixed.dat",header = TRUE)

# this is a small test using only 2 continuous covariates and the first 2,000 observations

library(PReMiuM)
runInfoObj<-profRegr(yModel="Normal", 
                     xModel="Normal", nSweeps=2000, nClusInit=40,
                     nBurn=2000, data=dataAll[1:2000,], output="output", 
                     covNames = c("Cont1","Cont2"))

dissimObj<-calcDissimilarityMatrix(runInfoObj)
clusObj<-calcOptimalClustering(dissimObj,maxNClusters = nClusters+3)
riskProfileObj<-calcAvgRiskAndProfile(clusObj)
clusterOrderObj<-plotRiskProfile(riskProfileObj,"summary.png")

