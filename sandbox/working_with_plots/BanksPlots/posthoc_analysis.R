library(amap)
library(dynamicTreeCut)
library(cluster)
library(dendextend)
library(lubridate)
library(gridExtra)
library(viridis)
library(tidyverse)
library(PReMiuM)

setwd("/Users/banks/RProjects/EnviroTyping/sandbox/shifted_data_analysis/2016")

# Bring in hybrid identifier (col 1), experiment identifier (2), and yield (15) observations from 2016 
# monthly, calibrated, shifted data 
pedi <- read_rds("../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")[,c(1,2,15)]

# Load riskProfObj which is based on the 2016 min_vars_3000 without NAs analysis
riskProfObj <- read_rds("min_vars_3000/riskProfObj.rds")

# If you would like to see the risk profile of various variables, run the below code with different indices:
# plotRiskProfile(riskProfObj,whichCovariates=riskProfObj$riskProfClusObj$clusObjRunInfoObj$covNames[11:20],
#                outFile="../../working_with_plots/BanksPlots/profile_plot.png")

# Create a new variable named "clus" to add cluster identifiers to hybrids and merge with pedi dataframe
# and weather observations for hybrids
hyb_by_mon_clus <- data.frame(clus = as.factor(riskProfObj$riskProfClusObj$clustering),pedi,riskProfObj$riskProfClusObj$clusObjRunInfoObj$xMat[,-1])

colnames(hyb_by_mon_clus) # to verify we have only indentifier and minimum variables with non-zero variance

cores <- parallel::detectCores()
posthocGroup <- hcluster(hyb_by_mon_clus[,-1], method = "euclidean", link = "ward", nbproc = cores)

# The two categorical variables needed to be converted to type "Factor" to be used with daisy
hyb_by_mon_clus$Exp = as.factor(hyb_by_mon_clus$Exp)
hyb_by_mon_clus$Pedi = as.factor(hyb_by_mon_clus$Pedi)
str(hyb_by_mon_clus) # confirm the first three variables have type "Factor" and the rest "num"

# Establish post-hoc groups for further analysis
dist = daisy(hyb_by_mon_clus)
dynamicCutree = cutreeDynamic(posthocGroup, distM = as.matrix(dist), deepSplit = 4, verbose = 4) 
length(unique(dynamicCutree)) # the number of post-hoc groups should be 4
hyb_by_mon_posthoc = data.frame(group = dynamicCutree, hyb_by_mon_clus)

# Unnecessary step: rearrange columns to make reading dataframe easier
hyb_by_mon_posthoc = hyb_by_mon_posthoc[c(3,4,5,1,2,7,8,6,9,10,11,12,13,14,15,16,17,19,20,18,21,22,24,25,23)]
# colnames(hyb_by_mon_posthoc) # can be used to see new order of variables

clus.colors = c("1"="#7570B3","2"="#1C9E77","3"="#D95F02","4"="#E72A8A","5"="#E6AB02","6"="#666666") # assign colors to groups

# To create a set of boxplots that illustrate Yield by the clusters in each post-hoc group
png("../../working_with_plots/BanksPlots/Figures/ClusterYieldsbyGroup.png", width = 1280, height = 1080)
l = list()
for (i in 1:4){
    tmp = hyb_by_mon_posthoc %>% filter(group == i)
    p = ggplot(tmp,aes(clus, Yield)) + geom_bar(aes(fill = clus), width = .5, stat = "summary", fun.y = "mean") + scale_fill_manual(values=clus.colors) +
         labs(title = paste("Average Yield by Cluster for Group",i,sep = " "), x = "Envirotyping Cluster", y = "Yield [bu/acre]") +
         theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + guides(fill=FALSE)
    l[[i]] = p
}
grid.arrange(l[[1]],l[[2]],l[[3]],l[[4]])
dev.off()

ggplot(hyb_by_mon_posthoc, aes(x = clus, y = Yield)) + geom_bar(aes(fill = clus), stat = "summary", fun.y = "mean")+
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

