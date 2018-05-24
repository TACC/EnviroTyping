library(amap)
library(cluster)
library(dendextend)
library(colorspace)
library(tidyverse)
library(tictoc)
library(dynamicTreeCut)
# library(viridis)
# library(gridExtra)
# library(reshape2)
setwd("~/github//EnviroTyping/sandbox/posthoc_group_analysis/")

hyb_mon <- read_rds("../../data/interim/2015/hybrid_by_month_shift_all_stats.rds")

riskProfObj <- read_rds("../shifted_data_analysis/2015/min_vars_3000/riskProfObj.rds")

hyb_clus <- bind_cols(clus = riskProfObj$riskProfClusObj$clustering, hyb_mon)

cores <- parallel::detectCores()-2
hclus <- hcluster(hyb_clus, method = "euclidean", link = "ward", nbproc = cores)

# plot(posthocGroup)
# rect.hclust(posthocGroup, k=14, border="red")
# pruned <- maptree::clip.clust(posthocGroup,weeks3_5Clust, k=14)
# maptree::draw.clust(pruned)

k <- 6
cols <- rainbow_hcl(k)
dend <- as.dendrogram(hclus)
dend <- color_branches(dend, k = k)
plot(dend, leaflab='none')
labels_dend <- labels(dend)
group <- cutree(dend, k, use_labels_not_values = TRUE, order_clusters_as_data = FALSE)
labels_to_keep <- labels_dend[k != group]
tic()
dends <- prune(dend, labels_to_keep)
toc()

hyb_clus %<>% modify_at(c(1:4), as.character) %>% 
    modify_at(c(1:4), as_factor)
dist <- daisy(hyb_clus)
tic()
cut_hclus <- cutreeDynamic(hclus, distM = as.matrix(dist), deepSplit = 4, verbose = 4)
toc()
length(unique(dynamicCutree)) # 14
weeks3_5Groups <- data.frame(group = dynamicCutree, weeks3_5Clust)


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


l <- list()
t <- list()
for (i in 1:14){
temp <- weeks3_5Groups%>% filter(group==i)
temp <- apply(temp[5:24],2,summary)
t[[i]] <- temp
data <- rowid_to_column(as.tibble(t(scale(temp))), "ID")
data <- data %>% select(ID, Min = Min., Median, Max = Max.)
df <- melt(data ,  id.vars = 'ID', variable.name = 'series')

p <- ggplot(df, aes(ID,value)) + geom_point(aes(color=series, size = 10), show.legend = FALSE) + labs(title = paste("Scaled Weather Profile for Group",i,sep = " ")) +
    scale_x_continuous(breaks = c(1:19), labels = names(weeks3_5Groups[,c(6:13,15:25)])) + theme_bw() +
    theme(axis.text.x = element_text(face="bold", size=6, angle=45,margin = margin(t = 10)), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank())
l[[i]] <- p
}

#pdf("profileByGroup.pdf", paper = "a4r")
grid.arrange(l[[1]],l[[2]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]], ncol = 2)
#dev.off()

groupsWithHybRanks <- left_join(weeks3_5Groups,rank, by=c("Exp","Pedi"))

group2clus9 <- groupsWithHybRanks %>% filter(group==2, optClus==9) %>% select(Pedi, Rank, Yield)
group2clus13 <- groupsWithHybRanks %>% filter(group==2, optClus==13) %>% select(Pedi, Rank, Yield)
group5clus9 <- groupsWithHybRanks %>% filter(group==5, optClus==9) %>% select(Pedi, Rank, Yield)
group5clus13 <- groupsWithHybRanks %>% filter(group==5, optClus==13) %>% select(Pedi, Rank, Yield)
match(group2clus9$Pedi,group5clus9$Pedi)
match(group2clus13$Pedi,group5clus13$Pedi)

group2 <- groupsWithHybRanks %>% filter(group==2) %>% select(Exp,optClus, Pedi, Rank, Yield)
table(group2$optClus,group2$Exp)
group5 <- groupsWithHybRanks %>% filter(group==5) %>% select(Exp, optClus, Pedi, Rank, Yield)
table(group5$optClus,group5$Exp)

group2clus5 <- groupsWithHybRanks %>% filter(group==2, optClus==5) %>% select(Exp,Pedi, Rank, Yield)
group2clus11 <- groupsWithHybRanks %>% filter(group==2, optClus==11) %>% select(Exp,Pedi, Rank, Yield)
group5clus5 <- groupsWithHybRanks %>% filter(group==5, optClus==5) %>% select(Exp,Pedi, Rank, Yield)
group5clus11 <- groupsWithHybRanks %>% filter(group==5, optClus==11) %>% select(Exp,Pedi, Rank, Yield)
match(group5clus5$Pedi,group2clus5$Pedi)
match(group2clus11$Pedi,group5clus11$Pedi)

group1 <- groupsWithHybRanks %>% filter(group==1) %>% select(Exp, optClus, Pedi, Rank, Yield)
table(group1$optClus, group1$Exp)
group6 <- groupsWithHybRanks %>% filter(group==6) %>% select(Exp, optClus, Pedi, Rank, Yield)
table(group6$optClus,group6$Exp)
group1clus1 <- groupsWithHybRanks %>% filter(group==1, optClus==1) %>% select(Exp, Pedi, Rank, Yield)
group6clus1 <- groupsWithHybRanks %>% filter(group==6, optClus==1) %>% select(Exp,Pedi, Rank, Yield)
match(group1clus1$Pedi,group6clus1$Pedi)

temp <- weeks3_5Groups%>% filter(group==5)
temp <- apply(temp[,c(6:13,15:25)],2,summary)
temp <- temp[c(1,3,6),]
write_csv(as.tibble(temp), "temp.csv")
