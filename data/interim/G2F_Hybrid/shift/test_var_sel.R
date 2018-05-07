var.sel.80 <- list()
for (i in 1:3) {
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
                               data = temp, nSweeps = 10, nBurn = 1,
                               nProgress = 1,
                               reportBurnIn = TRUE, 
                               seed = 1234, varSelectType = "Continuous")
        rho <- summariseVarSelectRho(runInfoObj)
        var.sel.80[[hyb]][[i]] <- var[which(rho$rhoMean > .90)]
    }
}
str(var.sel.80)
