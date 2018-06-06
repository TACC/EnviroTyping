library(PReMiuM)
library(tidyverse)

# test of the simulated data 
setwd("/work/04734/dhbrand/stampede2/github/EnviroTyping/Simulated-data-for-performance-testing/output")
dataAll<-read.table("../sim_large_dataset_NormalMixed.dat",header = TRUE)

cont_vars <- str_subset(names(dataAll), "Cont")
disc_vars <- str_subset(names(dataAll), "Discr")

runInfoObj<-profRegr(covNames, continuousCovs = cont_vars, discreteCovs = disc_vars, yModel="Normal", xModel="Normal", nSweeps=2000, nClusInit=40, nBurn=2000, data=dataAll, output="output")

dissimObj<-calcDissimilarityMatrix(runInfoObj)
clusObj<-calcOptimalClustering(dissimObj)
riskProfileObj<-calcAvgRiskAndProfile(clusObj)
write_rds(riskProfileObj, "riskProfObj.rds", compress = "xz")
