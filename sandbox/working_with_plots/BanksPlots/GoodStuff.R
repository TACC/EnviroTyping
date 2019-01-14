library(amap)
library(dynamicTreeCut)
library(cluster)
library(dendextend)
library(lubridate)
library(tictoc)
library(tidyverse)
library(PReMiuM)

setwd("/Users/banks/RProjects/EnviroTyping/sandbox/shifted_data_analysis/2016")

# Read in a few months' data for min variables
hyb_by_mon = read_rds("../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")
hyb_by_mon_5_7 = hyb_by_mon %>% select(Exp,Pedi,Yield,ends_with("5"),ends_with("5"),ends_with("7")) %>% select(contains("min"))

# riskProfObj is based on min_vars_3000 without NA's and min vars
riskProfObj = read_rds("../../working_with_plots/BanksPlots/riskProfObj.rds")
# plotRiskProfile(riskProfObj,whichCovariates=riskProfObj$riskProfClusObj$clusObjRunInfoObj$covNames[11:20],
#                outFile="../../working_with_plots/BanksPlots/plot.png")

# Bring in cluster number to monthly hybrid data
mon5_7Clust = data.frame(optClus = as.factor(riskProfObj$riskProfClusObj$clustering),hyb_by_mon_5_7)

cores <- parallel::detectCores() - 1

tic()
posthocGroup <- hcluster(mon5_7Clust[,-2], method = "euclidean", link = "ward", nbproc = cores)
toc()

typeof(posthocGroup)

tic()
dynamicCutree <- cutreeDynamic(posthocGroup, distM = as.matrix(dist), deepSplit = 4, verbose = 4) 
toc()

n_distinct(dynamicCutree) 
mon5_7Groups <- data.frame(group = dynamicCutree, mon5_7Clust)

l = list()
for (i in 1:11){
    #print({
    tmp <- weeks3_5DynamicTree %>% filter(dynamicCutree == i)
    p <-  ggplot(tmp,aes(optClus, Yield)) + geom_bar(aes(fill = optClus), stat = "summary", fun.y = "mean", width = .5) +
        scale_fill_viridis(discrete = TRUE) + labs(title = paste("Cluster by Yield for Group",i,sep = " "), x = "Envirotyping Cluster", y = "Yield [bu/acre]" ) +
        theme_bw()+ guides(fill=FALSE)
    l[[i]] <- p
    #})
}

grid.arrange(l[[1]],l[[2]],l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]],l[[10]],l[[11]])

png("yieldByClusterPlot.png", width = 1280, height = 1080)
ggplot(weeks3_5DynamicTree, aes(x = optClus, y = Yield)) + geom_bar(aes(fill = optClus), stat = "summary", fun.y = "mean")+
    facet_wrap(~ dynamicCutree, scales = "free_y", ncol = 4) +
    scale_fill_viridis(discrete = TRUE) + labs(title = "Cluster by Yield for Post hoc Grouping", x = "Envirotyping Cluster", y = "Yield [bu/acre]" ) +
    theme_bw()+ guides(fill=FALSE)
dev.off()

pdf("yieldByClusterPlot.pdf", paper = "a4r")
for (i in 1:14){
    print({
        tmp <- weeks3_5Groups %>% filter(group == i)
        ggplot(gather(tmp,key,value, -c(group,optClus,Pedi)),aes(optClus,value)) + geom_boxplot(aes(fill=optClus)) +
            facet_wrap(~ key, scales="free_y", ncol = 5)+
            scale_fill_viridis(discrete = TRUE)+ ggtitle(paste("Cluster by Yield for Post hoc Group",i,sep = " ")) +
            theme_bw()+
            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    })
}
dev.off()