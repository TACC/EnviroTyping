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
pedi = read_rds("../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")[,c(1,2,15)]

# Load riskProfObj which is based on the 2016 min_vars_3000 without NAs analysis
riskProfObj = read_rds("min_vars_3000/riskProfObj.rds")

# If you would like to see the risk profile of various variables, run the below code with different indices:
# plotRiskProfile(riskProfObj,whichCovariates=riskProfObj$riskProfClusObj$clusObjRunInfoObj$covNames[11:20],
#                outFile="../../working_with_plots/BanksPlots/profile_plot.png")

# Create a new variable named "clus" to add cluster identifiers to hybrids and merge with pedi dataframe
# and weather observations for hybrids
hyb_by_mon_clus = data.frame(clus = as.factor(riskProfObj$riskProfClusObj$clustering),pedi,riskProfObj$riskProfClusObj$clusObjRunInfoObj$xMat[,-1])

colnames(hyb_by_mon_clus) # to verify we have only indentifier and minimum variables with non-zero variance

cores = parallel::detectCores()
posthocGroup = hcluster(hyb_by_mon_clus[,-1], method = "euclidean", link = "ward", nbproc = cores)

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

hyb_by_mon_posthoc$group = as.factor(hyb_by_mon_posthoc$group) # convert group to factor for graphing ease

clus.colors = c("1"="#7570B3","2"="#1C9E77","3"="#D95F02","4"="#E72A8A","5"="#E6AB02","6"="#666666") # assign colors to groups
group.colors = c("1"="#E69F00","2"="#57B4E9","3"="#019E73","4"="#F0E442")

# To create a set of boxplots that illustrate Yield by the clusters in each post-hoc group
png("../../working_with_plots/BanksPlots/Figures/ClusterYieldsbyGroup.png", width = 1280, height = 1080)
l = list()
for (i in 1:4){
    tmp = hyb_by_mon_posthoc %>% filter(group == i)
    p = ggplot(tmp,aes(clus, Yield)) + 
        geom_bar(aes(fill = clus), width = .5, stat = "summary", fun.y = "mean") + 
        scale_fill_manual(values=clus.colors) +
        labs(title = paste("Average Yield by Cluster for Group",i,sep = " "), x = "Envirotyping Cluster", y = "Yield [bu/acre]") +
        theme_bw() + 
        theme(plot.title = element_text(hjust = 0.5)) + 
        guides(fill=FALSE)
    l[[i]] = p
}
grid.arrange(l[[1]],l[[2]],l[[3]],l[[4]])
dev.off()

# Note: May need to use Shiny or other package to have interactive graphs that show number of hybrids per cluster in previous figure

png("../../working_with_plots/BanksPlots/Figures/AverageGroupYields.png", width = 1280, height = 1080)
ggplot(hyb_by_mon_posthoc, aes(x = group, y = Yield)) + 
    geom_bar(aes(fill = group), stat = "summary", fun.y = "mean") +
    scale_fill_manual(values=group.colors) + 
    labs(title = "Average Yield by Post Hoc Group", x = "Envirotyping Cluster", y = "Yield [bu/acre]" ) +
    theme_bw()+ 
    theme(plot.title = element_text(hjust = 0.5)) + 
    guides(fill=FALSE)
dev.off()

# We want to see how the post-hoc groups differ from one another, so we find hybrids
# are found in multiple groups and compare their Yield distributions with violin plots

# Next, we assign the identifiers of our hybrids of interest to their own vector (any can be chosen)

hyb_of_interest = c("2369/3IIH6","2FACC/3IIH6","B14A/MO17","B73/MO17","BGEM-0107-N/LH195","PHHB9/LH123HT","PHW52/LH82")

# There exists a glitch(?) where an unwanted violin plot is created unless we subset only our data of interest, so we subset to avoid
# the unwanted figure

hyb_by_mon_interest = hyb_by_mon_posthoc %>% filter(Pedi %in% hyb_of_interest)

png("../../working_with_plots/BanksPlots/Figures/ViolinbyPedi.png", width = 1280, height = 1080)
ggplot(hyb_by_mon_interest,aes(x = factor(Pedi,level = c("2369/3IIH6","2FACC/3IIH6","B14A/MO17","B73/MO17","BGEM-0107-N/LH195","PHHB9/LH123HT","PHW52/LH82")),y = Yield)) + 
    labs(title = "Yield by Pedigree and Group",x = "Pedi",y = "Yield") +
    geom_violin(fill = "#ADD8E6") + 
    geom_boxplot(width=0.1) + 
    scale_fill_manual(values=group.colors) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5))
dev.off()

png("../../working_with_plots/BanksPlots/Figures/ViolinbyGroup.png", width = 1280, height = 1080)
ggplot(hyb_by_mon_interest,aes(x = factor(Pedi,level = c("2369/3IIH6","2FACC/3IIH6","B14A/MO17","B73/MO17","BGEM-0107-N/LH195","PHHB9/LH123HT","PHW52/LH82")),y = Yield,fill = group)) + 
    geom_violin() + 
    scale_fill_manual(values=group.colors,name="Group") + 
    labs(title = "Yield by Pedigree and Group",x = "Pedi",y = "Yield") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5))
dev.off()

# Recreate "dots" image 
# GENERATE LISTS OF HYBRIDS IN ALL GROUPS

table(hyb_by_mon_posthoc$group,hyb_by_mon_posthoc$Exp) # To show distribution of experiments in each group

group1 = hyb_by_mon_posthoc %>% filter(group==1) %>% select(Exp,Pedi,Yield,clus)
group1_hybrids = as.data.frame(unique(group1$Pedi)); colnames(group1_hybrids) = "Hybrid" # To generate list of hybrids in Group 1
table(group1$clus,group1$Exp) # To see experiments in each cluster

group1clus2 = hyb_by_mon_posthoc %>% filter(group==1, clus==2) %>% select(Exp,Pedi,Yield)
group1clus3 = hyb_by_mon_posthoc %>% filter(group==1, clus==3) %>% select(Exp,Pedi,Yield)
group1clus4 = hyb_by_mon_posthoc %>% filter(group==1, clus==4) %>% select(Exp,Pedi,Yield)
group1clus5 = hyb_by_mon_posthoc %>% filter(group==1, clus==5) %>% select(Exp,Pedi,Yield)

group2 = hyb_by_mon_posthoc %>% filter(group==2) %>% select(Exp,Pedi,Yield,clus)
group2_hybrids = as.data.frame(unique(group2$Pedi)); colnames(group2_hybrids) = "Hybrid" # To generate list of hybrids in Group 2
table(group2$clus,group2$Exp) # To see experiments in each cluster

group2clus2 = hyb_by_mon_posthoc %>% filter(group==2, clus==2) %>% select(Exp,Pedi,Yield)
group2clus3 = hyb_by_mon_posthoc %>% filter(group==2, clus==3) %>% select(Exp,Pedi,Yield)
group2clus4 = hyb_by_mon_posthoc %>% filter(group==2, clus==4) %>% select(Exp,Pedi,Yield)

group3 = hyb_by_mon_posthoc %>% filter(group==3) %>% select(Exp,Pedi,Yield,clus)
group3_hybrids = as.data.frame(unique(group3$Pedi)); colnames(group3_hybrids) = "Hybrid" # To generate list of hybrids in Group 3
table(group3$clus,group3$Exp) # To see experiments in each cluster

group3clus3 = hyb_by_mon_posthoc %>% filter(group==3, clus==3) %>% select(Exp,Pedi,Yield)
group3clus4 = hyb_by_mon_posthoc %>% filter(group==3, clus==4) %>% select(Exp,Pedi,Yield)

group4 = hyb_by_mon_posthoc %>% filter(group==4) %>% select(Exp,Pedi,Yield,clus)
group4_hybrids = as.data.frame(unique(group4$Pedi)); colnames(group4_hybrids) = "Hybrid" # To generate list of hybrids in Group 4
table(group4$clus,group4$Exp) # To see experiments in each cluster

group4clus1 = hyb_by_mon_posthoc %>% filter(group==4, clus==1) %>% select(Exp,Pedi,Yield)
group4clus3 = hyb_by_mon_posthoc %>% filter(group==4, clus==3) %>% select(Exp,Pedi,Yield)
group4clus6 = hyb_by_mon_posthoc %>% filter(group==4, clus==6) %>% select(Exp,Pedi,Yield)

match(group2clus9$Pedi,group5clus9$Pedi)
match(group2clus13$Pedi,group5clus13$Pedi)