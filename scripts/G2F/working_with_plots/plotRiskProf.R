function (riskProfObj, outFile, showRelativeRisk = F, orderBy = NULL, 
          whichClusters = NULL, whichCovariates = NULL, useProfileStar = F, 
          riskLim = NULL) 
{
    riskProfClusObj = NULL
    clusObjRunInfoObj = NULL
    includeResponse = NULL
    yModel = NULL
    profileStar = NULL
    xModel = NULL
    whicCov = NULL
    nCategoriesY = NULL
    cluster = NULL
    prob = NULL
    meanProb = NULL
    fillColor = NULL
    lowerProb = NULL
    upperProb = NULL
    meanRisk = NULL
    lowerRisk = NULL
    upperRisk = NULL
    clusterSize = NULL
    mu = NULL
    meanMu = NULL
    lowerMu = NULL
    upperMu = NULL
    sigma = NULL
    meanSigma = NULL
    lowerSigma = NULL
    upperSigma = NULL
    weibullFixedShape = NULL
    nu = NULL
    meanNu = NULL
    lowerNu = NULL
    upperNu = NULL
    for (i in 1:length(riskProfObj)) assign(names(riskProfObj)[i], 
                                            riskProfObj[[i]])
    for (i in 1:length(riskProfClusObj)) assign(names(riskProfClusObj)[i], 
                                                riskProfClusObj[[i]])
    for (i in 1:length(clusObjRunInfoObj)) assign(names(clusObjRunInfoObj)[i], 
                                                  clusObjRunInfoObj[[i]])
    if (nClusters == 1) 
        stop("Cannot produce plots because only one cluster has been found.")
    if (includeResponse) {
        if (yModel == "Normal" || yModel == "Quantile") {
            showRelativeRisk <- F
        }
    }
    if (useProfileStar) {
        profile <- profileStar
    }
    if (!is.null(whichCovariates)) {
        if (!is.numeric(whichCovariates)) {
            whichCovariatesTmp <- vector()
            for (k in 1:length(whichCovariates)) {
                whichCovariatesTmp[k] <- which(riskProfClusObj$clusObjRunInfoObj$covNames == 
                                                   whichCovariates[k])
            }
            whichCovariates <- whichCovariatesTmp
        }
        if (xModel == "Discrete") {
            profile <- profile[, , whichCovariates, ]
            nCategories <- nCategories[whichCovariates]
            covNames <- covNames[whichCovariates]
            nCovariates <- length(whichCovariates)
        }
        else if (xModel == "Normal") {
            profile <- profile[, , whichCovariates]
            profileStdDev <- profileStdDev[, , whichCovariates, 
                                           whichCovariates]
            covNames <- covNames[whichCovariates]
            nCovariates <- length(whichCovariates)
        }
        else if (xModel == "Mixed") {
            nDiscreteCovsAll <- nDiscreteCovs
            nContinuousCovsAll <- nContinuousCovs
            whichDiscreteCovs <- whichCovariates[which(whichCovariates <= 
                                                           nDiscreteCovs)]
            whichContinuousCovs <- whichCovariates[which(whichCovariates > 
                                                             nDiscreteCovs)]
            discreteCovs <- discreteCovs[whichDiscreteCovs]
            nDiscreteCovs <- length(discreteCovs)
            continuousCovs <- continuousCovs[whichContinuousCovs - 
                                                 nDiscreteCovsAll]
            nContinuousCovs <- length(continuousCovs)
            profilePhi <- profilePhi[, , whichDiscreteCovs, ]
            nCategories <- nCategories[whichDiscreteCovs]
            profileMu <- profileMu[, , whichContinuousCovs - 
                                       nDiscreteCovsAll]
            profileStdDev <- profileStdDev[, , whichContinuousCovs - 
                                               nDiscreteCovsAll, whichContinuousCovs - nDiscreteCovsAll]
            covNames <- c(discreteCovs, continuousCovs)
            nCovariates <- length(covNames)
        }
    }
    if (nCovariates > 15) 
        stop("ERROR: this plotting function is suitable only for up to 15 covariates. If your dataset includes more than 15 variables, the option 'whichCovariates' allows plotting of subsets of covariates.")
    png(outFile, width = 1200, height = 800)
    orderProvided <- F
    if (!is.null(orderBy)) {
        if (!includeResponse) {
            if (orderBy != "Empirical" && orderBy != "ClusterSize" && 
                !orderBy %in% covNames) {
                if (is.numeric(orderBy)) {
                    if (length(orderBy) == nClusters) {
                        orderProvided <- T
                        meanSortIndex <- orderBy
                    }
                    else {
                        cat("Order vector provided not of same length as number of clusters. Reverting to default ordering.\n")
                        orderBy <- NULL
                    }
                    orderBy <- NULL
                }
            }
        }
        else {
            if (orderBy != "Risk" && orderBy != "Empirical" && 
                orderBy != "ClusterSize" && !orderBy %in% covNames) {
                if (is.numeric(orderBy)) {
                    if (length(orderBy) == nClusters) {
                        orderProvided <- T
                        meanSortIndex <- orderBy
                    }
                    else {
                        cat("Order vector provided not of same length as number of clusters. Reverting to default ordering.\n")
                        orderBy <- NULL
                    }
                    orderBy <- NULL
                }
            }
        }
    }
    plotLayout <- grid.layout(ncol = nCovariates + 2, nrow = 6)
    grid.newpage()
    pushViewport(viewport(layout = plotLayout))
    if (!orderProvided) {
        if (!is.null(risk)) {
            if (is.null(orderBy)) {
                orderStat <- apply(risk, 2, median)
            }
            else {
                if (orderBy == "Risk") {
                    orderStat <- apply(risk, 2, median)
                }
                else if (orderBy == "Empirical") {
                    orderStat <- empiricals
                }
                else if (orderBy == "ClusterSize") {
                    orderStat <- clusterSizes
                }
                else {
                    whichCov <- match(orderBy, covNames)
                    if (xModel == "Normal") {
                        orderStat <- apply(profile[, , whichCov], 
                                           2, median)
                    }
                    else {
                        tmpMat <- profile[, , whichCov, 1]
                        if (nCategories[whichCov] > 1) {
                            for (k in 2:nCategories[whichCov]) {
                                tmpMat <- tmpMat + k * profile[, , whichCov, 
                                                               k]
                            }
                        }
                        orderStat <- apply(tmpMat, 2, median)
                    }
                }
            }
        }
        else {
            if (is.null(orderBy)) {
                orderStat <- empiricals
            }
            else {
                if (orderBy == "Empirical") {
                    orderStat <- empiricals
                }
                else if (orderBy == "ClusterSize") {
                    orderStat <- clusterSizes
                }
                else {
                    whichCov <- match(orderBy, covNames)
                    if (xModel == "Normal") {
                        orderStat <- apply(profile[, , whichCov], 
                                           2, median)
                    }
                    else {
                        tmpMat <- profile[, , whichCov, 1]
                        if (nCategories[whichCov] > 1) {
                            for (k in 2:(nCategories[whichCov])) {
                                tmpMat <- tmpMat + k * profile[, , whichCov, 
                                                               k]
                            }
                        }
                        orderStat <- apply(tmpMat, 2, median)
                    }
                }
            }
        }
        meanSortIndex <- order(orderStat, decreasing = F)
    }
    if (includeResponse) {
        riskDim <- dim(risk)
        risk <- array(risk[, meanSortIndex, ], dim = riskDim)
        if (showRelativeRisk) {
            for (c in nClusters:1) {
                risk[, c, ] <- risk[, c, ]/risk[, 1, ]
            }
        }
        if (yModel == "Survival" && !weibullFixedShape) {
            nuDim <- dim(nuArray)
            nuArray <- array(nuArray[, meanSortIndex], dim = nuDim)
        }
    }
    clusterSizes <- clusterSizes[meanSortIndex]
    empiricals <- empiricals[meanSortIndex]
    meanEmpirical <- sum(empiricals * clusterSizes)/sum(clusterSizes)
    if (is.null(whichClusters)) {
        whichClusters <- 1:nClusters
    }
    nClusters <- length(whichClusters)
    if (includeResponse) {
        riskMeans <- apply(risk, 2, mean, trim = 0.005)
        riskMean <- sum(riskMeans * clusterSizes)/sum(clusterSizes)
        riskLower <- apply(risk, 2, quantile, 0.05)
        riskUpper <- apply(risk, 2, quantile, 0.95)
        plotMax <- max(riskUpper)
        riskColor <- ifelse(riskLower > rep(riskMean, nClusters), 
                            "high", ifelse(riskUpper < rep(riskMean, nClusters), 
                                           "low", "avg"))
        if (yModel == "Categorical") {
            my.list <- vector("list", length(whichClusters) * 
                                  nCategoriesY)
        }
        else {
            my.list <- vector("list", length(whichClusters))
        }
        if (yModel == "Survival" && !weibullFixedShape) {
            nuMeans <- apply(nuArray, 2, mean, trim = 0.005)
            nuMean <- sum(nuMeans * clusterSizes)/sum(clusterSizes)
            nuLower <- apply(nuArray, 2, quantile, 0.05)
            nuUpper <- apply(nuArray, 2, quantile, 0.95)
        }
    }
    else {
        riskColor <- ifelse(empiricals > rep(meanEmpirical, length(empiricals)), 
                            "high", ifelse(empiricals < rep(meanEmpirical, nClusters), 
                                           "low", "avg"))
    }
    my.list.nu <- vector("list", length(whichClusters))
    my.list.empirical <- vector("list", length(whichClusters))
    my.list.size <- vector("list", length(whichClusters))
    z = 1
    z.nu = 1
    z.other = 1
    for (c in whichClusters) {
        if (includeResponse) {
            if (yModel == "Categorical") {
                plotRisk <- risk[, c, ]
                nPoints <- dim(plotRisk)[1]
                for (k in 1:nCategoriesY) {
                    my.list[[z]] <- data.frame(risk = plotRisk[, 
                                                               k], category = rep(k, nPoints), cluster = rep(c, 
                                                                                                             nPoints), meanRisk = rep(riskMean, nPoints), 
                                               lowerRisk = rep(riskLower[c], nPoints), upperRisk = rep(riskUpper[c], 
                                                                                                       nPoints), fillColor = rep(riskColor[c], 
                                                                                                                                 nPoints))
                    z = z + 1
                }
            }
            else {
                plotRisk <- risk[, c, ]
                plotRisk <- plotRisk[plotRisk < plotMax]
                nPoints <- length(plotRisk)
                my.list[[z]] <- data.frame(risk = plotRisk, cluster = rep(c, 
                                                                          nPoints), meanRisk = rep(riskMean, nPoints), 
                                           lowerRisk = rep(riskLower[c], nPoints), upperRisk = rep(riskUpper[c], 
                                                                                                   nPoints), fillColor = rep(riskColor[c], nPoints))
                z = z + 1
            }
            if (yModel == "Survival" && !weibullFixedShape) {
                plotNu <- nuArray[, c]
                nPoints <- length(plotNu)
                my.list.nu[[z.nu]] <- data.frame(nu = plotNu, 
                                                 cluster = rep(c, nPoints), meanNu = rep(nuMean, 
                                                                                         nPoints), lowerNu = rep(nuLower[c], nPoints), 
                                                 upperNu = rep(nuUpper[c], nPoints), fillColor = rep(riskColor[c], 
                                                                                                     nPoints))
                z.nu = z.nu + 1
            }
        }
        my.list.empirical[[z.other]] <- data.frame(empiricals = empiricals[c], 
                                                   meanEmpirical = meanEmpirical, cluster = c, fillColor = riskColor[c])
        my.list.size[[z.other]] <- data.frame(clusterSize = clusterSizes[c], 
                                              cluster = c, fillColor = riskColor[c])
        z.other = z.other + 1
    }
    if (includeResponse) {
        riskDF <- do.call("rbind", my.list)
        nuDF <- do.call("rbind", my.list.nu)
        empiricalDF <- do.call("rbind", my.list.empirical)
    }
    sizeDF <- do.call("rbind", my.list.size)
    if (includeResponse) {
        if (yModel == "Categorical") {
            my.list <- vector("list", length(whichClusters) * 
                                  nCategoriesY)
            z = 1
            for (k in 1:nCategoriesY) {
                probMat <- risk[, , k]
                nPoints <- nrow(probMat)
                probMeans <- apply(probMat, 2, mean)
                probMean <- sum(probMeans * clusterSizes)/sum(clusterSizes)
                probLower <- apply(probMat, 2, quantile, 0.05)
                probUpper <- apply(probMat, 2, quantile, 0.95)
                probColor <- ifelse(probLower > rep(probMean, 
                                                    length(probLower)), "high", ifelse(probUpper < 
                                                                                           rep(probMean, length(probUpper)), "low", "avg"))
                for (c in whichClusters) {
                    my.list[[z]] <- data.frame(prob = probMat[, 
                                                              c], cluster = rep(c, nPoints), category = rep(k - 
                                                                                                                1, nPoints), meanProb = rep(probMean, nPoints), 
                                               lowerProb = rep(probLower[c], nPoints), upperProb = rep(probUpper[c], 
                                                                                                       nPoints), fillColor = rep(probColor[c], 
                                                                                                                                 nPoints))
                    z = z + 1
                }
            }
            riskDF <- do.call("rbind", my.list)
            rownames(riskDF) <- seq(1, nrow(riskDF), 1)
            plotObj <- ggplot(riskDF)
            plotObj <- plotObj + facet_wrap(~category, ncol = 1, 
                                            as.table = F, scales = "free_y")
            plotObj <- plotObj + geom_hline(aes(yintercept = meanProb))
            plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), 
                                                  y = prob, fill = as.factor(fillColor)), outlier.size = 0.5)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = lowerProb, colour = as.factor(fillColor)), 
                                            size = 1.5)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = upperProb, colour = as.factor(fillColor)), 
                                            size = 1.5)
            plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", 
                                                              low = "#0066CC", avg = "#33CC66")) + scale_colour_manual(values = c(high = "#CC0033", 
                                                                                                                                  low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
                labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))
            plotObj <- plotObj + labs(y = "Probability") + theme(axis.title.y = element_text(size = 10, 
                                                                                             angle = 90))
            plotObj <- plotObj + labs(title = ifelse(showRelativeRisk, 
                                                     "Relative Risk", "Risk"), plot.title = element_text(size = 10))
            plotObj <- plotObj + theme(plot.margin = unit(c(0.5, 
                                                            0.15, 0.5, 0.15), "lines")) + theme(plot.margin = unit(c(0, 
                                                                                                                     0, 0, 0), "lines"))
            print(plotObj, vp = viewport(layout.pos.row = 1:6, 
                                         layout.pos.col = 2))
        }
        else {
            rownames(riskDF) <- seq(1, nrow(riskDF), by = 1)
            plotObj <- ggplot(riskDF)
            plotObj <- plotObj + geom_hline(aes(yintercept = meanRisk))
            plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), 
                                                  y = risk, fill = as.factor(fillColor)), outlier.size = 0.5)
            if (!is.null(riskLim)) 
                plotObj <- plotObj + coord_cartesian(ylim = riskLim)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = lowerRisk, colour = as.factor(fillColor)), 
                                            size = 1.5)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = upperRisk, colour = as.factor(fillColor)), 
                                            size = 1.5)
            plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", 
                                                              low = "#0066CC", avg = "#33CC66")) + scale_colour_manual(values = c(high = "#CC0033", 
                                                                                                                                  low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
                labs(x = "Cluster", y = ifelse(showRelativeRisk, 
                                               "RR", ifelse(yModel == "Categorical" || yModel == 
                                                                "Bernoulli" || yModel == "Binomial", "Probability", 
                                                            "E[Y]")))
            plotObj <- plotObj + theme(axis.title.y = element_text(size = 10, 
                                                                   angle = 90), axis.title.x = element_text(size = 10))
            plotObj <- plotObj + labs(title = ifelse(showRelativeRisk, 
                                                     "Relative Risk", "Risk"), plot.title = element_text(size = 10))
            plotObj <- plotObj + theme(plot.margin = unit(c(0, 
                                                            0, 0, 0), "lines")) + theme(plot.margin = unit(c(0.5, 
                                                                                                             0.15, 0.5, 0.15), "lines"))
            print(plotObj, vp = viewport(layout.pos.row = 1:6, 
                                         layout.pos.col = 2))
        }
    }
    if ((!is.null(yModel))) {
        if (yModel != "Categorical") {
            plotObj <- ggplot(empiricalDF)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = empiricals, colour = as.factor(fillColor)), 
                                            size = 3)
            plotObj <- plotObj + geom_hline(aes(yintercept = meanEmpirical))
            plotObj <- plotObj + scale_colour_manual(values = c(high = "#CC0033", 
                                                                low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none")
            plotObj <- plotObj + labs(title = "Empirical Data", 
                                      plot.title = element_text(size = 10))
            plotObj <- plotObj + theme(axis.title.x = element_text(size = 10), 
                                       axis.title.y = element_text(size = 10, angle = 90))
            plotObj <- plotObj + labs(y = ifelse(yModel == "Bernoulli", 
                                                 "Proportion of cases", ifelse(yModel == "Binomial", 
                                                                               "Avg Proportion of occurrence", ifelse(yModel == 
                                                                                                                          "Poisson", "Avg Count", ifelse(yModel == 
                                                                                                                                                             "Survival", "Avg Survival Time", ifelse(yModel == 
                                                                                                                                                                                                         "Categorical", "Avg Proportion of occurrence", 
                                                                                                                                                                                                     "Avg Y"))))), x = "Cluster")
            plotObj <- plotObj + theme(plot.margin = unit(c(0, 
                                                            0, 0, 0), "lines")) + theme(plot.margin = unit(c(0.15, 
                                                                                                             0.5, 0.5, 1), "lines"))
            print(plotObj, vp = viewport(layout.pos.row = 1:3, 
                                         layout.pos.col = 1))
        }
    }
    plotObj <- ggplot(sizeDF)
    plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                        y = clusterSize, colour = as.factor(fillColor)), size = 3)
    plotObj <- plotObj + scale_colour_manual(values = c(high = "#CC0033", 
                                                        low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none")
    plotObj <- plotObj + labs(title = "Size", plot.title = element_text(size = 10))
    plotObj <- plotObj + theme(axis.title.x = element_text(size = 10), 
                               axis.title.y = element_text(size = 10, angle = 90))
    plotObj <- plotObj + labs(y = "No. of Subjects", x = "Cluster")
    plotObj <- plotObj + theme(plot.margin = unit(c(0, 0, 0, 
                                                    0), "lines")) + theme(plot.margin = unit(c(0.15, 0.5, 
                                                                                               0.5, 1), "lines"))
    print(plotObj, vp = viewport(layout.pos.row = 4:6, layout.pos.col = 1))
    for (j in 1:nCovariates) {
        if (xModel == "Discrete") {
            my.list <- vector("list", length(whichClusters) * 
                                  nCategories[j])
            z = 1
            for (k in 1:nCategories[j]) {
                probMat <- profile[, meanSortIndex, j, k]
                nPoints <- nrow(probMat)
                probMeans <- apply(probMat, 2, mean)
                probMean <- sum(probMeans * clusterSizes)/sum(clusterSizes)
                probLower <- apply(probMat, 2, quantile, 0.05)
                probUpper <- apply(probMat, 2, quantile, 0.95)
                probColor <- ifelse(probLower > rep(probMean, 
                                                    length(probLower)), "high", ifelse(probUpper < 
                                                                                           rep(probMean, length(probUpper)), "low", "avg"))
                for (c in whichClusters) {
                    my.list[[z]] <- data.frame(prob = probMat[, 
                                                              c], cluster = rep(c, nPoints), category = rep(k - 
                                                                                                                1, nPoints), meanProb = rep(probMean, nPoints), 
                                               lowerProb = rep(probLower[c], nPoints), upperProb = rep(probUpper[c], 
                                                                                                       nPoints), fillColor = rep(probColor[c], 
                                                                                                                                 nPoints))
                    z = z + 1
                }
            }
            profileDF <- do.call("rbind", my.list)
            rownames(profileDF) <- seq(1, nrow(profileDF), 1)
            plotObj <- ggplot(profileDF)
            plotObj <- plotObj + facet_wrap(~category, ncol = 1, 
                                            as.table = F, scales = "free_y")
            plotObj <- plotObj + geom_hline(aes(yintercept = meanProb))
            plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), 
                                                  y = prob, fill = as.factor(fillColor)), outlier.size = 0.5)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = lowerProb, colour = as.factor(fillColor)), 
                                            size = 1.5)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = upperProb, colour = as.factor(fillColor)), 
                                            size = 1.5)
            plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", 
                                                              low = "#0066CC", avg = "#33CC66")) + scale_colour_manual(values = c(high = "#CC0033", 
                                                                                                                                  low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
                labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))
            if (j == 1) {
                plotObj <- plotObj + labs(y = "Probability") + 
                    theme(axis.title.y = element_text(size = 10, 
                                                      angle = 90))
            }
            else {
                plotObj <- plotObj + theme(axis.title.y = element_blank())
            }
            plotObj <- plotObj + labs(title = covNames[j], plot.title = element_text(size = 10))
            plotObj <- plotObj + theme(plot.margin = unit(c(0.5, 
                                                            ifelse(j == nCovariates, 1, 0), 0.5, ifelse(j == 
                                                                                                            1, 0.5, 0)), "lines")) + theme(plot.margin = unit(c(0, 
                                                                                                                                                                0, 0, 0), "lines"))
            print(plotObj, vp = viewport(layout.pos.row = 1:6, 
                                         layout.pos.col = j + 2))
        }
        else if (xModel == "Normal") {
            my.list <- vector("list", length(whichClusters))
            z = 1
            muMat <- profile[, meanSortIndex, j]
            muMeans <- apply(muMat, 2, mean)
            muMean <- sum(muMeans * clusterSizes)/sum(clusterSizes)
            muLower <- apply(muMat, 2, quantile, 0.05)
            muUpper <- apply(muMat, 2, quantile, 0.95)
            plotMax <- max(muUpper)
            plotMin <- min(muLower)
            muColor <- ifelse(muLower > rep(muMean, length(muLower)), 
                              "high", ifelse(muUpper < rep(muMean, length(muUpper)), 
                                             "low", "avg"))
            for (c in whichClusters) {
                plotMu <- muMat[, c]
                plotMu <- plotMu[plotMu < plotMax & plotMu > 
                                     plotMin]
                nPoints <- length(plotMu)
                my.list[[z]] <- data.frame(mu = plotMu, cluster = rep(c, 
                                                                      nPoints), meanMu = rep(muMean, nPoints), lowerMu = rep(muLower[c], 
                                                                                                                             nPoints), upperMu = rep(muUpper[c], nPoints), 
                                           fillColor = rep(muColor[c], nPoints))
                z = z + 1
            }
            profileDF <- do.call("rbind", my.list)
            rownames(profileDF) <- seq(1, nrow(profileDF), 1)
            plotObj <- ggplot(profileDF)
            plotObj <- plotObj + geom_hline(aes(yintercept = meanMu))
            plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), 
                                                  y = mu, fill = as.factor(fillColor)), outlier.size = 0.5)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = lowerMu, colour = as.factor(fillColor)), 
                                            size = 1.5)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = upperMu, colour = as.factor(fillColor)), 
                                            size = 1.5)
            plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", 
                                                              low = "#0066CC", avg = "#33CC66")) + scale_colour_manual(values = c(high = "#CC0033", 
                                                                                                                                  low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
                labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))
            if (j == 1) {
                plotObj <- plotObj + labs(y = "Mean") + theme(axis.title.y = element_text(size = 10, 
                                                                                          angle = 90))
            }
            else {
                plotObj <- plotObj + theme(axis.title.y = element_blank())
            }
            plotObj <- plotObj + labs(title = covNames[j], plot.title = element_text(size = 10))
            plotObj <- plotObj + theme(plot.margin = unit(c(0.5, 
                                                            ifelse(j == nCovariates, 1, 0), 0.5, ifelse(j == 
                                                                                                            1, 0.5, 0)), "lines")) + theme(plot.margin = unit(c(0, 
                                                                                                                                                                0, 0, 0), "lines"))
            print(plotObj, vp = viewport(layout.pos.row = 1:3, 
                                         layout.pos.col = j + 2))
            my.list <- vector("list", length(whichClusters))
            z = 1
            sigmaMat <- profileStdDev[, meanSortIndex, j, j]
            sigmaMeans <- apply(sigmaMat, 2, mean)
            sigmaMean <- sum(sigmaMeans * clusterSizes)/sum(clusterSizes)
            sigmaLower <- apply(sigmaMat, 2, quantile, 0.05)
            sigmaUpper <- apply(sigmaMat, 2, quantile, 0.95)
            plotMax <- max(sigmaUpper)
            sigmaColor <- ifelse(sigmaLower > rep(sigmaMean, 
                                                  length(sigmaLower)), "high", ifelse(sigmaUpper < 
                                                                                          rep(sigmaMean, length(sigmaUpper)), "low", "avg"))
            for (c in whichClusters) {
                plotSigma <- sigmaMat[, c]
                plotSigma <- plotSigma[plotSigma < plotMax]
                nPoints <- length(plotSigma)
                my.list[[z]] <- data.frame(sigma = plotSigma, 
                                           cluster = rep(c, nPoints), meanSigma = rep(sigmaMean, 
                                                                                      nPoints), lowerSigma = rep(sigmaLower[c], 
                                                                                                                 nPoints), upperSigma = rep(sigmaUpper[c], 
                                                                                                                                            nPoints), fillColor = rep(sigmaColor[c], 
                                                                                                                                                                      nPoints))
                z = z + 1
            }
            profileDF <- do.call("rbind", my.list)
            rownames(profileDF) <- seq(1, nrow(profileDF), 1)
            plotObj <- ggplot(profileDF)
            plotObj <- plotObj + geom_hline(aes(yintercept = meanSigma))
            plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), 
                                                  y = sigma, fill = as.factor(fillColor)), outlier.size = 0.5)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = lowerSigma, colour = as.factor(fillColor)), 
                                            size = 1.5)
            plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                y = upperSigma, colour = as.factor(fillColor)), 
                                            size = 1.5)
            plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", 
                                                              low = "#0066CC", avg = "#33CC66")) + scale_colour_manual(values = c(high = "#CC0033", 
                                                                                                                                  low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
                labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))
            if (j == 1) {
                plotObj <- plotObj + labs(y = "Std Dev") + theme(axis.title.y = element_text(size = 10, 
                                                                                             angle = 90))
            }
            else {
                plotObj <- plotObj + theme(axis.title.y = element_blank())
            }
            plotObj <- plotObj + theme(plot.margin = unit(c(0.5, 
                                                            ifelse(j == nCovariates, 1, 0), 0.5, ifelse(j == 
                                                                                                            1, 0.5, 0)), "lines")) + theme(plot.margin = unit(c(0, 
                                                                                                                                                                0, 0, 0), "lines"))
            print(plotObj, vp = viewport(layout.pos.row = 4:6, 
                                         layout.pos.col = j + 2))
        }
        else if (xModel == "Mixed") {
            if (j <= nDiscreteCovs) {
                my.list <- vector("list", length(whichClusters) * 
                                      nCategories[j])
                z = 1
                for (k in 1:nCategories[j]) {
                    if (nDiscreteCovs == 1) {
                        probMat <- profilePhi[, meanSortIndex, 1, 
                                              k]
                    }
                    else {
                        probMat <- profilePhi[, meanSortIndex, j, 
                                              k]
                    }
                    nPoints <- nrow(probMat)
                    probMeans <- apply(probMat, 2, mean)
                    probMean <- sum(probMeans * clusterSizes)/sum(clusterSizes)
                    probLower <- apply(probMat, 2, quantile, 0.05)
                    probUpper <- apply(probMat, 2, quantile, 0.95)
                    probColor <- ifelse(probLower > rep(probMean, 
                                                        length(probLower)), "high", ifelse(probUpper < 
                                                                                               rep(probMean, length(probUpper)), "low", 
                                                                                           "avg"))
                    for (c in whichClusters) {
                        my.list[[z]] <- data.frame(prob = probMat[, 
                                                                  c], cluster = rep(c, nPoints), category = rep(k - 
                                                                                                                    1, nPoints), meanProb = rep(probMean, nPoints), 
                                                   lowerProb = rep(probLower[c], nPoints), 
                                                   upperProb = rep(probUpper[c], nPoints), 
                                                   fillColor = rep(probColor[c], nPoints))
                        z = z + 1
                    }
                }
                profileDF <- do.call("rbind", my.list)
                rownames(profileDF) <- seq(1, nrow(profileDF), 
                                           1)
                plotObj <- ggplot(profileDF)
                plotObj <- plotObj + facet_wrap(~category, ncol = 1, 
                                                as.table = F, scales = "free_y")
                plotObj <- plotObj + geom_hline(aes(yintercept = meanProb))
                plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), 
                                                      y = prob, fill = as.factor(fillColor)), outlier.size = 0.5)
                plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                    y = lowerProb, colour = as.factor(fillColor)), 
                                                size = 1.5)
                plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                    y = upperProb, colour = as.factor(fillColor)), 
                                                size = 1.5)
                plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", 
                                                                  low = "#0066CC", avg = "#33CC66")) + scale_colour_manual(values = c(high = "#CC0033", 
                                                                                                                                      low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
                    labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))
                if (j == 1) {
                    plotObj <- plotObj + labs(y = "Probability") + 
                        theme(axis.title.y = element_text(size = 10, 
                                                          angle = 90))
                }
                else {
                    plotObj <- plotObj + theme(axis.title.y = element_blank())
                }
                plotObj <- plotObj + labs(title = covNames[j], 
                                          plot.title = element_text(size = 10))
                plotObj <- plotObj + theme(plot.margin = unit(c(0.5, 
                                                                ifelse(j == nCovariates, 1, 0), 0.5, ifelse(j == 
                                                                                                                1, 0.5, 0)), "lines")) + theme(plot.margin = unit(c(0, 
                                                                                                                                                                    0, 0, 0), "lines"))
                print(plotObj, vp = viewport(layout.pos.row = 1:6, 
                                             layout.pos.col = j + 2))
            }
            else {
                my.list <- vector("list", length(whichClusters))
                z = 1
                if (nContinuousCovs == 1) {
                    muMat <- profileMu[, meanSortIndex, 1]
                }
                else {
                    muMat <- profileMu[, meanSortIndex, (j - nDiscreteCovs)]
                }
                muMeans <- apply(muMat, 2, mean)
                muMean <- sum(muMeans * clusterSizes)/sum(clusterSizes)
                muLower <- apply(muMat, 2, quantile, 0.05)
                muUpper <- apply(muMat, 2, quantile, 0.95)
                plotMax <- max(muUpper)
                plotMin <- min(muLower)
                muColor <- ifelse(muLower > rep(muMean, length(muLower)), 
                                  "high", ifelse(muUpper < rep(muMean, length(muUpper)), 
                                                 "low", "avg"))
                for (c in whichClusters) {
                    plotMu <- muMat[, c]
                    plotMu <- plotMu[plotMu < plotMax & plotMu > 
                                         plotMin]
                    nPoints <- length(plotMu)
                    my.list[[z]] <- data.frame(mu = plotMu, cluster = rep(c, 
                                                                          nPoints), meanMu = rep(muMean, nPoints), 
                                               lowerMu = rep(muLower[c], nPoints), upperMu = rep(muUpper[c], 
                                                                                                 nPoints), fillColor = rep(muColor[c], nPoints))
                    z = z + 1
                }
                profileDF <- do.call("rbind", my.list)
                rownames(profileDF) <- seq(1, nrow(profileDF), 
                                           1)
                plotObj <- ggplot(profileDF)
                plotObj <- plotObj + geom_hline(aes(yintercept = meanMu))
                plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), 
                                                      y = mu, fill = as.factor(fillColor)), outlier.size = 0.5)
                plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                    y = lowerMu, colour = as.factor(fillColor)), 
                                                size = 1.5)
                plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                    y = upperMu, colour = as.factor(fillColor)), 
                                                size = 1.5)
                plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", 
                                                                  low = "#0066CC", avg = "#33CC66")) + scale_colour_manual(values = c(high = "#CC0033", 
                                                                                                                                      low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
                    labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))
                if (j == 1) {
                    plotObj <- plotObj + labs(y = "Mean") + theme(axis.title.y = element_text(size = 10, 
                                                                                              angle = 90))
                }
                else {
                    plotObj <- plotObj + theme(axis.title.y = element_blank())
                }
                plotObj <- plotObj + labs(title = covNames[j], 
                                          plot.title = element_text(size = 10))
                plotObj <- plotObj + theme(plot.margin = unit(c(0.5, 
                                                                ifelse(j == nCovariates, 1, 0), 0.5, ifelse(j == 
                                                                                                                1, 0.5, 0)), "lines")) + theme(plot.margin = unit(c(0, 
                                                                                                                                                                    0, 0, 0), "lines"))
                print(plotObj, vp = viewport(layout.pos.row = 1:3, 
                                             layout.pos.col = j + 2))
                my.list <- vector("list", length(whichClusters))
                z = 1
                if (nContinuousCovs == 1) {
                    sigmaMat <- profileStdDev[, meanSortIndex, 
                                              1, 1]
                }
                else {
                    sigmaMat <- profileStdDev[, meanSortIndex, 
                                              (j - nDiscreteCovs), (j - nDiscreteCovs)]
                }
                sigmaMeans <- apply(sigmaMat, 2, mean)
                sigmaMean <- sum(sigmaMeans * clusterSizes)/sum(clusterSizes)
                sigmaLower <- apply(sigmaMat, 2, quantile, 0.05)
                sigmaUpper <- apply(sigmaMat, 2, quantile, 0.95)
                plotMax <- max(sigmaUpper)
                sigmaColor <- ifelse(sigmaLower > rep(sigmaMean, 
                                                      length(sigmaLower)), "high", ifelse(sigmaUpper < 
                                                                                              rep(sigmaMean, length(sigmaUpper)), "low", 
                                                                                          "avg"))
                for (c in whichClusters) {
                    plotSigma <- sigmaMat[, c]
                    plotSigma <- plotSigma[plotSigma < plotMax]
                    nPoints <- length(plotSigma)
                    my.list[[z]] <- data.frame(sigma = plotSigma, 
                                               cluster = rep(c, nPoints), meanSigma = rep(sigmaMean, 
                                                                                          nPoints), lowerSigma = rep(sigmaLower[c], 
                                                                                                                     nPoints), upperSigma = rep(sigmaUpper[c], 
                                                                                                                                                nPoints), fillColor = rep(sigmaColor[c], 
                                                                                                                                                                          nPoints))
                    z = z + 1
                }
                profileDF <- do.call("rbind", my.list)
                rownames(profileDF) <- seq(1, nrow(profileDF), 
                                           1)
                plotObj <- ggplot(profileDF)
                plotObj <- plotObj + geom_hline(aes(yintercept = meanSigma))
                plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), 
                                                      y = sigma, fill = as.factor(fillColor)), outlier.size = 0.5)
                plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                    y = lowerSigma, colour = as.factor(fillColor)), 
                                                size = 1.5)
                plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), 
                                                    y = upperSigma, colour = as.factor(fillColor)), 
                                                size = 1.5)
                plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", 
                                                                  low = "#0066CC", avg = "#33CC66")) + scale_colour_manual(values = c(high = "#CC0033", 
                                                                                                                                      low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
                    labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))
                if (j == 1) {
                    plotObj <- plotObj + labs(y = "Std Dev") + 
                        theme(axis.title.y = element_text(size = 10, 
                                                          angle = 90))
                }
                else {
                    plotObj <- plotObj + theme(axis.title.y = element_blank())
                }
                plotObj <- plotObj + theme(plot.margin = unit(c(0.5, 
                                                                ifelse(j == nCovariates, 1, 0), 0.5, ifelse(j == 
                                                                                                                1, 0.5, 0)), "lines")) + theme(plot.margin = unit(c(0, 
                                                                                                                                                                    0, 0, 0), "lines"))
                print(plotObj, vp = viewport(layout.pos.row = 4:6, 
                                             layout.pos.col = j + 2))
            }
        }
    }
    dev.off()
    return(meanSortIndex)
}
<bytecode: 0x10db662a0>
    <environment: namespace:PReMiuM>