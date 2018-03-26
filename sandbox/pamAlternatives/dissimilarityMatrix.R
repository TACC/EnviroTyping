library(PReMiuM)
library(Matrix)
library(gdata)
library(ClusterR)

setwd("sandbox/multiThreading/Output")
generateDataList <- clusSummaryNormalNormal()

inputs <- generateSampleDataFile(generateDataList)

runInfoObj<-profRegr(yModel=inputs$yModel, xModel=inputs$xModel, 
                     nSweeps=10, nBurn=20, data=inputs$inputData, output="output", 
                     covNames=inputs$covNames,nClusInit=15)

dissimObj<-calcDissimilarityMatrix(runInfoObj)

disSimMat <- dissimObj$disSimMat

emptyMatrix <- Matrix(0, nrow = a[1], ncol = a[1])
upperTriangle(emptyMatrix) <- diss
emptyMatrix[1:10,1190:1200]
new <- forceSymmetric(emptyMatrix)
dim(new);sum(diag(new))
cores <- parallel::detectCores()
cluster <- Cluster_Medoids(dissimObj$disSimRunInfoObj$xMat, 13, distance_metric = "euclidean", threads = cores, swap_phase = TRUE, verbose = TRUE)

optClusObj <- calcOptimalClustering(dissimObj)
