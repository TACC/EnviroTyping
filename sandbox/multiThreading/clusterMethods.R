library(PReMiuM)
library(profvis)
library(ClusterR)
library(readr)
library(dplyr)
library(lubridate)
library(tictoc)
library(cluster)
library(Matrix)

df <- read_csv("data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))

temp <- df %>% filter(Week >= isoweek(Planted)+3 & Week <= isoweek(Planted)+5)
val <- grep("Min|Max",names(temp))

numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))]
test <- data.frame(temp$Yield, temp$Exp, temp$Pedi, temp[numericVars])
test1 <- data.frame(temp[numericVars])

inputs <- generateSampleDataFile(clusSummaryNormalDiscrete())

#setwd("/work/04734/dhbrand/stampede2/EnviroTyping/scripts/multiThreading")
setwd("sandbox/multiThreading/Output")
#source("ppam.R")
df <- data.frame(inputs$inputData)
#setwd("/work/04734/dhbrand/stampede2/EnviroTyping/scripts/multiThreading/Output")
setwd("Output/")

runInfoObj <- profRegr(yModel=inputs$yModel, xModel=inputs$xModel, nSweeps=100, nClusInit=15,nBurn=300, data=inputs$inputData, output="output", covNames = inputs$covNames,  fixedEffectsNames = inputs$fixedEffectNames, seed=12345)

dissimObj <- calcDissimilarityMatrix(runInfoObj)
class(dissimObj$disSimMat)
diss <- as.matrix(dissimObj$disSimMat, nrow = 2120, ncol = 2120)


class(diss)

profvis({
    Cluster_Medoids(data = diss, clusters = 5, threads = 1, swap_phase = FALSE, verbose = TRUE, seed = 1)
})

tic()
cl <- Cluster_Medoids(data = test1, clusters = 5, threads = 1, swap_phase = TRUE, seed = 1)
toc() # 95.308 sec elapsed

tic()
cl1 <- Cluster_Medoids(data = test1, clusters = 5, threads = 7, swap_phase = TRUE, seed = 1)
toc() # 35.025 sec elapsed

tic()
pm <- pam(test1, 5, diss = FALSE, do.swap = TRUE)
toc() # 224.434 sec elapsed
plot(pm)

tic()
opt_md <- Optimal_Clusters_Medoids(test1, 30, 'euclidean', plot_clusters = FALSE)
toc()




# riskProfileObj<-calcAvgRiskAndProfile(clusObj)
