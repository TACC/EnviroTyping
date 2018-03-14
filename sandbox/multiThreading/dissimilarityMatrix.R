library(PReMiuM)
library(Matrix)
library(gdata)

generateDataList <- clusSummaryNormalNormal()

inputs <- generateSampleDataFile(generateDataList)

runInfoObj<-profRegr(yModel=inputs$yModel, xModel=inputs$xModel, 
                     nSweeps=10, nBurn=20, data=inputs$inputData, output="output", 
                     covNames=inputs$covNames,nClusInit=15)

dissimObj<-calcDissimilarityMatrix(runInfoObj)

str(dissimObj)

diss <- dissimObj$disSimMat

emptyMatrix <- Matrix(0, nrow = length(diss)+1, ncol = length(diss) + 1)
upperTriangle(emptyMatrix) <- diss

