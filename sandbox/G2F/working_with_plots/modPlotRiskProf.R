
modplotRiskProfile <- function (riskProfObj, outFile, showRelativeRisk = F, orderBy = NULL, 
          whichClusters = NULL, whichCovariates = NULL, useProfileStar = F, 
          riskLim = NULL) 
{
    ##### make some NULL objects  #####
   
    showRelativeRisk = F; orderBy = NULL; whichClusters = NULL; whichCovariates = NULL;
        useProfileStar = F
    riskLim = NULL
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
    
    ##### Create Variables and DF's #####
    
    for (i in 1:length(riskProfObj)) assign(names(riskProfObj)[i],riskProfObj[[i]])
    for (i in 1:length(riskProfClusObj)) assign(names(riskProfClusObj)[i],riskProfClusObj[[i]])
    for (i in 1:length(clusObjRunInfoObj)) assign(names(clusObjRunInfoObj)[i],clusObjRunInfoObj[[i]])
    
    png(outFile, width = 1200, height = 800)
    orderProvided <- F
    
    if(!orderProvided){
        if(!is.null(risk)){
            if(is.null(orderBy)){
                # Default is to order by posterior theta risk
                # Compute the means
                orderStat<-apply(risk,2,median)
             }
        # Sort into ascending mean size
        meanSortIndex<-order(orderStat,decreasing=F)
        }
    }
    
    
    if (includeResponse) {
        riskDim <- dim(risk)
        risk <- array(risk[, meanSortIndex, ], dim = riskDim)
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
      
    }
    my.list <- vector("list", length(whichClusters))
    
    my.list.nu <- vector("list", length(whichClusters))
    my.list.empirical <- vector("list", length(whichClusters))
    my.list.size <- vector("list", length(whichClusters))
    z = 1
    z.other = 1
    
    for (c in whichClusters) {
        if (includeResponse) {
                plotRisk <- risk[, c, ]
                plotRisk <- plotRisk[plotRisk < plotMax]
                nPoints <- length(plotRisk)
                my.list[[z]] <- data.frame(risk = plotRisk, cluster = rep(c, 
                                                                          nPoints), meanRisk = rep(riskMean, nPoints), 
                                           lowerRisk = rep(riskLower[c], nPoints), upperRisk = rep(riskUpper[c], 
                                                                                                   nPoints), fillColor = rep(riskColor[c], nPoints))
                z = z + 1
            
            
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
    
    ##### Create Risk Plot #####
    
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
    
    ##### First 2 plots #####
    
    
    plotObj2 <- ggplot(sizeDF)
    plotObj2 <- plotObj2 + geom_point(aes(x = as.factor(cluster), 
                                        y = clusterSize, colour = as.factor(fillColor)), size = 3)
    plotObj2 <- plotObj2 + scale_colour_manual(values = c(high = "#CC0033", 
                                                        low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none")
    plotObj2 <- plotObj2 + labs(title = "Size", plot.title = element_text(size = 10))
    plotObj2 <- plotObj2 + theme(axis.title.x = element_text(size = 10), 
                               axis.title.y = element_text(size = 10, angle = 90))
    plotObj2 <- plotObj2 + labs(y = "No. of Subjects", x = "Cluster")
    plotObj2 <- plotObj2 + theme(plot.margin = unit(c(0, 0, 0, 
                                                    0), "lines")) + theme(plot.margin = unit(c(0.15, 0.5, 
                                                                                               0.5, 1), "lines"))
    print(plotObj2, vp = viewport(layout.pos.row = 4:6, layout.pos.col = 1))
    
    ##### Mean and SD Plots #####
    
    for (j in 1:nCovariates) {
        # first loop is for probability plots of discrete covariates
            if (j <= nDiscreteCovs) {
                my.list <- vector("list", length(whichClusters) * 
                                      nCategories[j])
                z = 1
                for (k in 1:nCategories[j]) {
                    if (nDiscreteCovs == 1) {
                        probMat <- profilePhi[, meanSortIndex, 1, 
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
                
                ##### Mean plots ######
                
                my.list <- vector("list", length(whichClusters))
                z = 1
            
                muMat <- profileMu[, meanSortIndex, (j - nDiscreteCovs)]
                
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
                profileDF <<- do.call("rbind", my.list)
                rownames(profileDF) <- seq(1, nrow(profileDF), 1)
                
                plotObj <- ggplot(profileDF)
                plotObj <- plotObj + geom_hline() #aes(yintercept = meanMu)
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
                
                ##### SD plots ######
                
                my.list <- vector("list", length(whichClusters))
                z = 1
             
                sigmaMat <- profileStdDev[, meanSortIndex, 
                                              (j - nDiscreteCovs), (j - nDiscreteCovs)]
                
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
                plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033",low = "#0066CC", avg = "#33CC66")) + scale_colour_manual(values = c(high = "#CC0033", low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
                    labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))
            
                plotObj <- plotObj + theme(axis.title.y = element_blank())
                
                plotObj <- plotObj + theme(plot.margin = unit(c(0.5, 
                                                                ifelse(j == nCovariates, 1, 0), 0.5, ifelse(j == 
                                                                                                                1, 0.5, 0)), "lines")) + theme(plot.margin = unit(c(0, 
                                                                                                                                                                    0, 0, 0), "lines"))
                print(plotObj, vp = viewport(layout.pos.row = 4:6, 
                                             layout.pos.col = j + 2))
            }
        
    }
    dev.off()

}