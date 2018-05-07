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

temp <- filter(df, Pedi %in% unique(df$Pedi)[50])
min <- names(df)[grep("Min", names(df))]
max <- names(df)[grep("Max", names(df))]
med <- names(df)[grep("Median", names(df))]
mean <- names(df)[grep("Mean", names(df))]
stats <- c("Min", "Max", "Med", "Mean")

var.sel <- list()
for (i in 1:2) {
    hyb <- unique(df$Pedi)[i]
    temp <- filter(df, Pedi %in% hyb)
    var.sel[[hyb]] <- list()
    for (i in stats) {
        set.seed(1234)
        print(i)
        var <- names(df)[grep(i, names(df))]
        runInfoObj <- profRegr(covNames = var, outcome = 'Yield',
                               yModel = 'Normal', xModel = "Normal",
                               data = temp, nSweeps = 10, nBurn = 5,
                               nProgress = 1,
                               reportBurnIn = TRUE, 
                               seed = 1234, varSelectType = "Continuous")
        rho <- summariseVarSelectRho(runInfoObj)
        var.sel[[hyb]][[i]] <- var[which(rho$rhoMean > .90)]
    }
}




globalParsTrace(runInfoObj, parameters = "nClusters", plotBurnIn = T)
rho <- summariseVarSelectRho(runInfoObj)
var.sel.min <- names(temp[min])[which(rho$rhoMean > .8)]
var.sel.max <- names(temp[max])[which(rho$rhoMean > .8)]
var.sel.med <- names(temp[med])[which(rho$rhoMean > .8)]
var.sel.mean <- names(temp[mean])[which(rho$rhoMean > .95)]

calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
plotRiskProfile(riskProfObj, "hyb.png")

max.out <- max[which(is.na(match(max,var.sel.max)))]



