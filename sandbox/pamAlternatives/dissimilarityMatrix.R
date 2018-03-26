library(PReMiuM)
library(Matrix)
library(gdata)
library(ClusterR)

setwd("sandbox/multiThreading/Output")
generateDataList <- clusSummaryNormalNormal()

inputs <- generateSampleDataFile(generateDataList)

runInfoObj<-profRegr(yModel=inputs$yModel, xModel=inputs$xModel, 
                     nSweeps=100, nBurn=20, data=inputs$inputData, output="output", 
                     covNames=inputs$covNames,nClusInit=15)

dissimObj<-calcDissimilarityMatrix(runInfoObj)

disSimMat <- dissimObj$disSimMat
disSimMat[1:20]

z <- read_delim("output_z.txt", " ", col_names = FALSE)
z[23,1:20]

d <- c(rep(1, 899), rep(0, (1200-899)))
mean(d)

emptyMatrix <- Matrix(0, nrow = a[1], ncol = a[1])
upperTriangle(emptyMatrix) <- diss
emptyMatrix[1:10,1190:1200]
new <- forceSymmetric(emptyMatrix)
dim(new);sum(diag(new))
cores <- parallel::detectCores()
cluster <- Cluster_Medoids(dissimObj$disSimRunInfoObj$xMat, 13, distance_metric = "euclidean", threads = cores, swap_phase = TRUE, verbose = TRUE)

optClusObj <- calcOptimalClustering(dissimObj)
r = 1/100
r^2
1-r^2

