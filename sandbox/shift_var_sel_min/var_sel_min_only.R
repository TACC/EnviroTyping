library(PReMiuM)

setwd("~/GitHub/EnviroTyping/sandbox/shift_var_sel_min/output")
df <- read_rds("../../../data/interim/G2F_Hybrid/hybrid_by_month_shift_all_stats.rds")

contVars <- names(df)[grep("Min", names(df))]
set.seed(1234)
runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = contVars,
                       data = df, nSweeps = 500, nBurn = 100,
                       nProgress = 10, seed = 1234,
                       reportBurnIn = TRUE)

globalParsTrace(runInfoObj, "alpha", plotBurnIn = TRUE)

calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)

may <- grep("5", contVars, value = TRUE)
june <- grep("6", contVars, value = TRUE)
july <- grep("7", contVars, value = TRUE)
aug <- grep("8", contVars, value = TRUE)
sept <- grep("9", contVars, value = TRUE)

plotRiskProfile(riskProfObj, "may.png", whichCovariates = may)
plotRiskProfile(riskProfObj, "june.png", whichCovariates = june)
plotRiskProfile(riskProfObj, "july.png", whichCovariates = july)
plotRiskProfile(riskProfObj, "aug.png", whichCovariates = aug)
plotRiskProfile(riskProfObj, "sept.png", whichCovariates = sept)

dew <- grep("dew", contVars, value = TRUE)

plotRiskProfile(riskProfObj, "dew.png", whichCovariates = dew)

df.clust <- cbind(df, clust = clusObj$clustering)

df.clust.4 <- df.clust %>% filter(clust == 4)
n_distinct(df.clust.4$Pedi)
df.clust.2 <- df.clust %>% filter(clust == 2)
n_distinct(df.clust.2$Pedi)
which(clusObj$clusterSizes == min(clusObj$clusterSizes))
df.clust.1 <- df.clust %>% filter(clust == 1)
n_distinct(df.clust.1$Pedi)


riskProfObj$profileMu[1:20,1,1:10]
dew.summary <- summary(df[contVars[1:5]])
humid.summary <- summary(df[contVars[6:10]])
soil.moist.summary <- summary(df[contVars[11:15]])



set.seed(1234)
runInfoObj2 <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = contVars,
                       data = df, nSweeps = 3000, nBurn = 50,
                       nProgress = 50,
                       reportBurnIn = TRUE)
# globalParsTrace(runInfoObj2, "nClusters", plotBurnIn = TRUE)
calcDists2 <- calcDissimilarityMatrix(runInfoObj2)
clusObj2 <- calcOptimalClustering(calcDists2)
clusObj2$nClusters
df2.clust <- cbind(df, clust = clusObj2$clustering)

df.repl.1 <- df2.clust %>% filter(Repl == 1)
df.repl.2 <- df2.clust %>% filter(Repl == 2)
table(df.repl.1$clust)
table(df.repl.2$clust)
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
                        data = df, nSweeps = 3000, nBurn = 50,
                        nProgress = 50,
                        reportBurnIn = TRUE)
# globalParsTrace(runInfoObj3, "nClusters", plotBurnIn = TRUE)
calcDists3 <- calcDissimilarityMatrix(runInfoObj3)
clusObj3 <- calcOptimalClustering(calcDists3)
clusObj3$nClusters
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
