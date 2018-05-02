library(tidyverse)
library(PReMiuM)
library(tictoc)

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/shift/output")

df <- read_rds("../../hybrid_by_month_shift_all_stats.rds")
# for (i in unique(df$Pedi)) {
#     temp <- filter(df, Pedi = i)
#     contVars <- names(df[5:49])
#     
# }

temp <- filter(df, Pedi %in% unique(df$Pedi)[1:5])
min <- names(df)[grep("Min", names(df))]
max <- names(df)[grep("Max", names(df))]
med <- names(df)[grep("Median", names(df))]
mean <- names(df)[grep("Mean", names(df))]
tic()
set.seed(1234)
runInfoObj <- profRegr(covNames = mean, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Normal",
                       data = temp, nSweeps = 10, nBurn = 5,
                       nProgress = 1,
                       reportBurnIn = TRUE, 
                       seed = 1234, varSelectType = "Continuous")
toc()
globalParsTrace(runInfoObj, parameters = "nClusters", plotBurnIn = T)
rho <- summariseVarSelectRho(runInfoObj)
var.sel.min <- names(temp[min])[which(rho$rhoMean > .8)]
var.sel.max <- names(temp[max])[which(rho$rhoMean > .8)]
var.sel.med <- names(temp[med])[which(rho$rhoMean > .8)]
var.sel.mean <- names(temp[mean])[which(rho$rhoMean > .8)]

calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
plotRiskProfile(riskProfObj, "hyb.png")

max.out <- max[which(is.na(match(max,var.sel.max)))]



