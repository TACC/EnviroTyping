library(tidyverse)
library(PReMiuM)
library(magrittr)
library(klaR)

setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/shift/output")
setwd("data/interim/G2F_Hybrid/shift/output/")
df <- read_rds("../../hybrid_by_month_shift_all_stats.rds")

stats <- c("Min", "Max", "Med", "Mean")

var.sel.80 <- list()
for (i in 1:n_distinct(df$Pedi)) {
    hyb <- unique(df$Pedi)[i]
    print(hyb)
    temp <- filter(df, Pedi %in% hyb)
    var.sel.80[[hyb]] <- list()
    for (i in stats) {
        set.seed(1234)
        print(i)
        var <- names(df)[grep(i, names(df))]
        runInfoObj <- profRegr(covNames = var, outcome = 'Yield',
                               yModel = 'Normal', xModel = "Normal",
                               data = temp, nSweeps = 100, nBurn = 50,
                               nProgress = 1,
                               reportBurnIn = TRUE, 
                               seed = 1234, varSelectType = "Continuous")
        rho <- summariseVarSelectRho(runInfoObj)
        var.sel.80[[hyb]][[i]] <- var[which(rho$rhoMean > .80)]
    }
}

write_rds(var.sel.80, "var.sel.80.rds")

var.sel.90 <- list()
for (i in 1:n_distinct(df$Pedi)) {
    hyb <- unique(df$Pedi)[i]
    temp <- filter(df, Pedi %in% hyb)
    var.sel.90[[hyb]] <- list()
    for (i in stats) {
        set.seed(1234)
        print(i)
        var <- names(df)[grep(i, names(df))]
        runInfoObj <- profRegr(covNames = var, outcome = 'Yield',
                               yModel = 'Normal', xModel = "Normal",
                               data = temp, nSweeps = 100, nBurn = 50,
                               nProgress = 1,
                               reportBurnIn = TRUE, 
                               seed = 1234, varSelectType = "Continuous")
        rho <- summariseVarSelectRho(runInfoObj)
        var.sel.90[[hyb]][[i]] <- var[which(rho$rhoMean > .90)]
    }
}

write_rds(var.sel.90, "var.sel.90.rds")

var.sel.95 <- list()
for (i in 1:n_distinct(df$Pedi)) {
    hyb <- unique(df$Pedi)[i]
    temp <- filter(df, Pedi %in% hyb)
    var.sel.95[[hyb]] <- list()
    for (i in stats) {
        set.seed(1234)
        print(i)
        var <- names(df)[grep(i, names(df))]
        runInfoObj <- profRegr(covNames = var, outcome = 'Yield',
                               yModel = 'Normal', xModel = "Normal",
                               data = temp, nSweeps = 100, nBurn = 50,
                               nProgress = 1,
                               reportBurnIn = TRUE, 
                               seed = 1234, varSelectType = "Continuous")
        rho <- summariseVarSelectRho(runInfoObj)
        var.sel.95[[hyb]][[i]] <- var[which(rho$rhoMean > .95)]
    }
}

write_rds(var.sel.95, "var.sel.95.rds")





