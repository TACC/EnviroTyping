setwd("~/training")
library(plyr)
##install.packages("dplyr")
library(dplyr)
##install.packages("tidyr")
library(tidyr)
##install.packages("xlsx")
##library(xlsx)
#install.packages("PReMiuM")
library(PReMiuM)

subin=read.csv("Subset_of_Final_Input.csv")

subin$brand_hybrid<-as.character(subin$brand_hybrid)
numericVars <- which(sapply(subin, class)=='numeric' & names(subin) != 'Yield')
categoricalVars <- which(sapply(subin, class)=='character' & names(subin) != 'Yield')

system.time({
  
  mod <- profRegr(covName, outcome = 'Yield', 
                  yModel = 'Normal', xModel = "Mixed",
                  #nCovariates = 2,
                  #fixedEffectsNames = 'yield',
                  discreteCovs = c(names(subin[categoricalVars])),
                  continuousCovs = c(names(subin[numericVars])),
                  data = subin)
})


require(snow)
library(parallel)
hostnames <- rep('localhost', 100)
cluster <- makeCluster(100,type="FORK")

clusterExport(cluster, list('mod'))

ptm <- proc.time()
result <- clusterCall(cluster, eval, profRegr(covName, outcome = 'Yield', 
                                              yModel = 'Normal', xModel = "Mixed",
                                              #nCovariates = 2,
                                              #fixedEffectsNames = 'yield',
                                              discreteCovs = c(names(subin[categoricalVars])),
                                              continuousCovs = c(names(subin[numericVars])),
                                              data = subin))##clusterApply(cluster, 1:10, function(i) myProc())
proc.time() - ptm

stopCluster(cluster)


calcDists <- calcDissimilarityMatrix(mod)

clusts <- calcOptimalClustering(calcDists)

riskProfileOb <- calcAvgRiskAndProfile(clusts)

clusterOrderObj<-plotRiskProfile(riskProfileOb,"summary.png")