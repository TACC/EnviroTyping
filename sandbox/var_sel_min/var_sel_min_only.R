library(PReMiuM)
library(tidyverse)
library(magrittr)

setwd("~/GitHub/EnviroTyping/sandbox/var_sel_min/output")
df <- read_rds("../../../data/interim/G2F_Hybrid/hybrid_by_month_shift_all_stats.rds")

outliers <- str_which(df$Pedi,"^Z0|^WF|^GEM|^W1|^PH2|^W37|^B14|^Q38|^MP7|^PHN|^TX8")
df <- df[-outliers,]

contVars <- names(df)[grep("Min", names(df))]
set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = contVars,
                       data = df, nSweeps = 3000, nBurn = 50,
                       nProgress = 50, seed = 1234,
                       reportBurnIn = TRUE)

globalParsTrace(runInfoObj, "nClusters", plotBurnIn = TRUE)

calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
clusObj$nClusters; clusObj$clusterSizes
riskProfObj <- calcAvgRiskAndProfile(clusObj)


set.seed(1234)
runInfoObj2 <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = contVars,
                       data = df, nSweeps = 3000, nBurn = 50,
                       nProgress = 50,
                       reportBurnIn = TRUE)
globalParsTrace(runInfoObj2, "nClusters", plotBurnIn = TRUE)
calcDists2 <- calcDissimilarityMatrix(runInfoObj2)
clusObj2 <- calcOptimalClustering(calcDists2)
clusObj2$nClusters;clusObj2$clusterSizes
df2.clust <- cbind(df, clust = clusObj2$clustering)
df2.clust.2 <- df2.clust %>% filter(clust == 8)


which(clusObj2$clusterSizes == min(clusObj2$clusterSizes))
df2.clust.1 <- df2.clust %>% filter(clust == 1)
n_distinct(df2.clust.1$Pedi)
df2.clust.2 <- df2.clust %>% filter(clust == 2)
n_distinct(df2.clust.2$Pedi)


riskProfObj2 <- calcAvgRiskAndProfile(clusObj2)
plotRiskProfile(riskProfObj2, "may.png", whichCovariates = may)



set.seed(1234)
runInfoObj3 <- profRegr(covNames, outcome = 'Yield',
                        yModel = 'Normal', xModel = "Mixed",
                        discreteCovs = "Pedi",
                        continuousCovs = contVars,
                        data = df, nSweeps = 10, nBurn = 5,
                        nProgress = 1,
                        reportBurnIn = TRUE)
# globalParsTrace(runInfoObj3, "nClusters", plotBurnIn = TRUE)
calcDists3 <- calcDissimilarityMatrix(runInfoObj3)
clusObj3 <- calcOptimalClustering(calcDists3)
clusObj3$nClusters; clusObj3$clusterSizes
df3.clust <- cbind(df, clust = clusObj3$clustering)
which(clusObj3$clusterSizes == min(clusObj3$clusterSizes))
df3.clust.1 <- df3.clust %>% filter(clust == 1)
n_distinct(df3.clust.1$Pedi)
df3.clust.2 <- df3.clust %>% filter(clust == 2)
n_distinct(df3.clust.2$Pedi)


riskProfObj3 <- calcAvgRiskAndProfile(clusObj3)




set.seed(1234)
runInfoObj4 <- profRegr(covNames, outcome = 'Yield',
                        yModel = 'Normal', xModel = "Mixed",
                        discreteCovs = "Pedi",
                        continuousCovs = contVars,
                        data = df, nSweeps = 5000, nBurn = 50,
                        nProgress = 50,
                        reportBurnIn = TRUE)
# globalParsTrace(runInfoObj3, "nClusters", plotBurnIn = TRUE)
calcDists4 <- calcDissimilarityMatrix(runInfoObj4)
clusObj4<- calcOptimalClustering(calcDists3)
clusObj4$nClusters
df4.clust <- cbind(df, clust = clusObj3$clustering)
which(clusObj3$clusterSizes == min(clusObj3$clusterSizes))
df3.clust.1 <- df3.clust %>% filter(clust == 1)
n_distinct(df3.clust.1$Pedi)
df3.clust.2 <- df3.clust %>% filter(clust == 2)
n_distinct(df3.clust.2$Pedi)


riskProfObj3 <- calcAvgRiskAndProfile(clusObj3)
