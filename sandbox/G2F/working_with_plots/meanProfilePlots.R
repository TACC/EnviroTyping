# Goal of this script to find which hybrids are the canaries or biggest contributors to the profile for the clusters with the best yield

library(readr)

clusObj <- readRDS("data/interim/G2F_Hybrid/premiumOutput_2.15.18_3-5wksSubset/output_seed_3_5/clusObj.rda")

df1 <- read_csv("data/interim/G2F_Hybrid/hybrid_by_weeksSincePlanted_cleaned_weather.csv", col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(),"rainMean" = col_number(), "rainMedian" = col_number(),"humidMin" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number()))

temp <- df1 %>% filter(seed_3_5 == TRUE)

val <- grep("Min|Max",names(temp))
numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))]

setwd("scripts/G2F/working_with_plots/output")

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = numericVars[1:14],
                       data = temp,
                       nSweeps = 100,
                       nBurn = 100)

calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

riskProfObj <- calcAvgRiskAndProfile(clusObj)

plotRiskProfile(riskProfObj, "premium1.png", whichCovariates = c('Exp', 'tempMin', 'tempMax'))

# Clusters 3, 4, and 5 have highest yields while clusters 3 and 5 have similar profiles
ct <- data.frame(Pedi = clusObj$clusObjRunInfoObj$xMat$Pedi,
                 tempMin = clusObj$clusObjRunInfoObj$xMat$tempMin,
                 yield = clusObj$clusObjRunInfoObj$yMat)


table(ct$Exp)
c1 <- ct %>% filter(clus == 1)
table(c1$Exp)
c2 <- ct %>% filter(clus == 2)
table(c2$Exp)
c3 <- ct %>% filter(clus == 3)
table(c3$Exp)
c4 <- ct %>% filter(clus == 4)
table(c4$Exp)
c5 <- ct %>% filter(clus == 5)
table(c5$Exp)

nch1 <- c5 %>% filter(Exp == "NCH1")
ohh1 <- c5 %>% filter(Exp == "OHH1")

var1 <- unique(nch1$Pedi)
var2 <- unique(ohh1$Pedi)
comnPedi <- intersect(var1,var2)

# 181 hybrids are found in both locations of cluster 5

# find mean of each covariate for cluster 5
for (i in 1:10){
    mu <- mean(profileMu[,5,i])
    mu5[i] <- mu
}
test <- ct %>% filter(clus == 3) %>% select(Pedi, yield)



##### Mean plots ######
for (i in 1:length(riskProfObj)) assign(names(riskProfObj)[i],riskProfObj[[i]])
for (i in 1:length(riskProfClusObj)) assign(names(riskProfClusObj)[i],riskProfClusObj[[i]])
for (i in 1:length(clusObjRunInfoObj)) assign(names(clusObjRunInfoObj)[i],clusObjRunInfoObj[[i]])
orderProvided <- F
orderBy = NULL
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
whichClusters <- 1:nClusters
my.list <- vector("list", length(whichClusters))
z = 1

muMat <- profileMu[, 5, ]

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
rownames(profileDF) <- seq(1, nrow(profileDF), 1)
cl5 <- profileDF %>% filter(cluster==5)

png("summary.png")
plotObj <- ggplot(cl5)
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
print(plotObj)
