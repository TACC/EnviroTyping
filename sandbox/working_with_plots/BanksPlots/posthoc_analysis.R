library(amap)
library(dynamicTreeCut)
library(cluster)
library(dendextend)
library(lubridate)
library(tictoc)
library(tidyverse)
library(PReMiuM)

setwd("/Users/banks/RProjects/EnviroTyping/sandbox/shifted_data_analysis/2016")

# Bring in hybrid identifier (col 1), experiment identifier (2), and yield (15) observations from 2016 monthly, calibrated
# shifted data 
pedi <- read_rds("../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")[,c(1,2,15)]

# Load riskProfObj which is based on the 2016 min_vars_3000 without NAs analysis
riskProfObj <- read_rds("../../sandbox/posthoc_group_analysis/2016/riskProfObj.rds")
# plotRiskProfile(riskProfObj,whichCovariates=riskProfObj$riskProfClusObj$clusObjRunInfoObj$covNames[11:20],
#                outFile="../../working_with_plots/BanksPlots/profile_plot.png")


# Create a new variable named "clus" to add cluster identifiers to hybrids and merge with pedi dataframe
# and weather observations for hybrids
df_clus <- data.frame(pedi, clus = as.factor(riskProfObj$riskProfClusObj$clustering),riskProfObj$riskProfClusObj$clusObjRunInfoObj$xMat[,-1])

colnames(df_clus) # to verify we have only indentifier and minimum variables with non-zero variance

cores <- parallel::detectCores()
posthocGroup <- hcluster(df_clus[,-1], method = "euclidean", link = "ward", nbproc = cores)

dist <- daisy(df_clus[,-1])
dynamicCutree <- cutreeDynamic(posthocGroup, distM = as.matrix(dist), deepSplit = 4, verbose = 4) 
length(unique(dynamicCutree)) # 14
df_groups <- data.frame(group = dynamicCutree, df_clus)







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
dev.off()login1(1011)