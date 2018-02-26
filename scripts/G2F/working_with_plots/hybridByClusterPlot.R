library(PReMiuM)
library(readr)
library(dplyr)
library(ggplot2)
library(ggplus)
library(optCluster)

df <- read_csv("data/interim/G2F_Hybrid/hybrid_bymonth_cleaned_weather.csv",
               col_types = cols( humidMin = col_double(),
                                 solarMin = col_double(),
                                 solarMax = col_double(),
                                 rainMin = col_double(),
                                 rainMedian = col_double(),
                                 windDirMin = col_double(),
                                 windDirMax = col_double()))

setwd("scripts/G2F/working_with_plots")

temp <- df %>% filter(Month==7)

val <- grep("Mean",names(temp))

numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))]

setwd("output")

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = numericVars,
                       data = temp,
                       nSweeps = 200,
                       nBurn = 100,
                       seed = 1234)


calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

riskProfObj <- calcAvgRiskAndProfile(clusObj)

#png("outFile", width = 1200, height = 800)
for (i in 1:length(riskProfObj)) assign(names(riskProfObj)[i],riskProfObj[[i]])
for (i in 1:length(riskProfClusObj)) assign(names(riskProfClusObj)[i],riskProfClusObj[[i]])
for (i in 1:length(clusObjRunInfoObj)) assign(names(clusObjRunInfoObj)[i],clusObjRunInfoObj[[i]])

orderStat <- apply(risk, 2, median)
meanSortIndex <- order(orderStat, decreasing = F)

whichClusters <- 1:nClusters

my.list <- vector("list", length(whichClusters) * nCategories)
z = 1
j = 1
for (k in 1:nCategories) {
    if (nDiscreteCovs == 1) {
        probMat <- profilePhi[, meanSortIndex, 1, 
                              k]
    }
    
    nPoints <- nrow(probMat)
    probMeans <- apply(probMat, 2, mean)
    probMean <- sum(probMeans * clusterSizes)/sum(clusterSizes)
    probLower <- apply(probMat, 2, quantile, 0.05)
    probUpper <- apply(probMat, 2, quantile, 0.95)
    probColor <- ifelse(probLower > rep(probMean, length(probLower)), 
                        "high", ifelse(probUpper < rep(probMean, length(probUpper)), "low", "avg"))
    for (c in whichClusters) {
        my.list[[z]] <- data.frame(prob = probMat[,c], cluster = rep(c, nPoints), 
                                   category = rep(k - 1, nPoints), meanProb = rep(probMean, nPoints), 
                                   lowerProb = rep(probLower[c], nPoints), 
                                   upperProb = rep(probUpper[c], nPoints), 
                                   fillColor = rep(probColor[c], nPoints))
        z = z + 1
    }
}
profileDF <- do.call("rbind", my.list)
rownames(profileDF) <- seq(1, nrow(profileDF), 1)

subset <- profileDF %>% 
    filter(between(category, 1, 10)) %>% 
    group_by(category, cluster) %>% 
    summarise(n = n())

plotObj <- ggplot(subset)
plotObj <- plotObj + facet_wrap(~category, as.table = F, scales = "free_y")
plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), y = n))
plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), y = lowerProb, colour = as.factor(fillColor)), size = 1.5)
plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), y = upperProb, colour = as.factor(fillColor)), size = 1.5)
plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", low = "#0066CC", avg = "#33CC66")) + 
    scale_colour_manual(values = c(high = "#CC0033", low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
    labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))

plotObj <- plotObj + labs(y = "Probability") + theme(axis.title.y = element_text(size = 10, angle = 90))


plotObj <- plotObj + labs(title = covNames[j], plot.title = element_text(size = 10))
plotObj <- plotObj + theme(plot.margin = unit(c(0.5, ifelse(j == nCovariates, 1, 0), 0.5,
                                                ifelse(j == 1, 0.5, 0)), "lines")) + theme(plot.margin = unit(c(0,0, 0, 0), "lines"))
print(plotObj)
dev.off()




####from modPlotRiskProf
plotObj <- ggplot(subset)
plotObj <- plotObj + facet_wrap(~category, as.table = F, scales = "free_y")
plotObj <- plotObj + geom_hline(aes(yintercept = meanProb))
plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), y = prob, fill = as.factor(fillColor)), outlier.size = 0.5)
plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), y = lowerProb, colour = as.factor(fillColor)), size = 1.5)
plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), y = upperProb, colour = as.factor(fillColor)), size = 1.5)
plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", low = "#0066CC", avg = "#33CC66")) + 
    scale_colour_manual(values = c(high = "#CC0033", low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
    labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))

plotObj <- plotObj + labs(y = "Probability") + theme(axis.title.y = element_text(size = 10, angle = 90))


plotObj <- plotObj + labs(title = covNames[j], plot.title = element_text(size = 10))
plotObj <- plotObj + theme(plot.margin = unit(c(0.5, ifelse(j == nCovariates, 1, 0), 0.5,
                                                ifelse(j == 1, 0.5, 0)), "lines")) + theme(plot.margin = unit(c(0,0, 0, 0), "lines"))
print(plotObj)
dev.off()



png("test.png")
plotObj <- ggplot(subset)
plotObj <- plotObj + geom_hline(aes(yintercept = meanProb))
plotObj <- plotObj + geom_boxplot(aes(x = as.factor(cluster), y = prob, fill = as.factor(fillColor)), outlier.size = 0.5)
plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), y = lowerProb, colour = as.factor(fillColor)), size = 1.5)
plotObj <- plotObj + geom_point(aes(x = as.factor(cluster), y = upperProb, colour = as.factor(fillColor)), size = 1.5)
plotObj <- plotObj + scale_fill_manual(values = c(high = "#CC0033", low = "#0066CC", avg = "#33CC66")) + 
    scale_colour_manual(values = c(high = "#CC0033", low = "#0066CC", avg = "#33CC66")) + theme(legend.position = "none") + 
    labs(x = "Cluster") + theme(axis.title.x = element_text(size = 10))

plotObj <- plotObj + labs(y = "Probability") + theme(axis.title.y = element_text(size = 10, angle = 90))


plotObj <- plotObj + labs(title = covNames[j], plot.title = element_text(size = 10))
plotObj <- plotObj + theme(plot.margin = unit(c(0.5, ifelse(j == nCovariates, 1, 0), 0.5,
                                                ifelse(j == 1, 0.5, 0)), "lines")) + theme(plot.margin = unit(c(0,0, 0, 0), "lines"))


facet_multiple(plot = plotObj, 
               facets = 'category', 
               ncol = 3, 
               nrow = 3)
dev.off()

optAlloc <- clusObj$clustering
tmp_vp<-data.frame(opt=as.integer(as.factor(optAlloc)), outc=clusObj$clusObjRunInfoObj$yMat, known=as.integer(as.factor(temp$Pedi)))
optC <- optCluster(tmp_vp, 2:10, clMethods = "pam")
summary(optC)
assg <- optAssign(optC)
table(assg$cluster)