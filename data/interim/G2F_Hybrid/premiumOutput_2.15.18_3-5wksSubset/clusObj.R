library(PReMiuM)
        
runInfoObj <- readRDS("/output_seed_3_5/runInfoObj.rda")

calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

saveRDS(clusObj, file = "clusObj.rda")
