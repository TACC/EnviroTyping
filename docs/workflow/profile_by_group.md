Post hoc Analysis
-----------------

After running the G2F hybrid/weather data through the PReMiuM package
there is a large amount of output. There are also many hybrids so a
further analysis is composed to try and answer a few high level
questions:

1.  Which hybrids have profile-clusters that are most different from
    each other (i.e. have the most g x e)?
2.  Which weather variables are most important for making
    profile-clusters that differ across hybrids, i.e. which covariates
    contribute the most to gxe?
3.  Which hybrid-profile clusters define the most similar fields (i.e.
    which field sites are the most redundant)?
4.  Which hybrids diagnose specific envirotype groups?

``` {.r}
library(amap)
library(cluster)
library(dendextend)
library(colorspace)
library(tidyverse)
library(lubridate)
library(tictoc)
library(dynamicTreeCut)
```

#### Loading data

Here load the original dataset run through Premium. Get the output with
the clustering numbers from Premium. Make a new dataframe with the
cluster associations.

``` {.r}
setwd("~/GitHub/EnviroTyping/")

hybridByWeek <- read_rds("data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.rds")


val <- grep("Min|Max",names(hybridByWeek))
numericVars <- names(hybridByWeek[val])[vapply(hybridByWeek[val], function(x) var(x) != 0, logical(1))]
hybridByWeekSubset <- hybridByWeek %>% select(Hyb=Pedi, Yield, Week, numericVars)
riskProfObj <- read_rds("/data/interim/G2F_Hybrid/hyb_wks3-5_by_week_1k/output/riskProfObj.rda")
weeks3_5 <- hybridByWeek %>% filter(Week >= isoweek(Planted) + 3 & Week <= isoweek(Planted) +5) %>% select(Exp,Pedi,Yield, numericVars)
weeks3_5Clust <- data.frame(optClus = as.factor(riskProfObj$riskProfClusObj$clustering), weeks3_5)
```

#### Agglomerative Hierarchical Clustering (AGNES)

As stated before there are too many hybrid labels within each cluster to
make strong qualitative conclusions. An additional step of clustering is
performed using aggolmerative hierarchical clustering to further break
down the data. The *amap* package function *hcluster* is used instead of
the standard *hclust* as it allows for parallel processing. Several
methods were also looked into when it came time to prune the dendrogram
produced by AGNES. A group at UCLA created an R package call
*dynamicTreeCut* which uses a novel way of pruning trees by accounting
for nested clusters via several parameters that give the more control
than the typical fixed height prune. With a new group variable from the
dynamic tree cutting and the cluster variable from the premium output,
filtering the hybrid weather profiles becomes possible.

``` {.r}
cores <- parallel::detectCores() - 1  # Save a core for RStudio
tic()
posthocGroup <- hcluster(weeks3_5Clust[,-2], method = "euclidean", link = "ward", nbproc = cores)
toc()  # 26.264 sec elapsed

tic()
dynamicCutree <- cutreeDynamic(posthocGroup, distM = as.matrix(dist), deepSplit = 4, verbose = 4) 
toc  # 55.688 sec elapsed
n_distinct(dynamicCutree)  # 14
weeks3_5Groups <- data.frame(group = dynamicCutree, weeks3_5Clust)
```

#### Plotting the Results

Developing the workflow allows for visual indicators to drive the
decisions. The y-value, yield, was first plotted across the cluster for
each group to look for any initial patterns. In Figure 5 the same
clusters can be seen in groups 1 & 6 or 2 & 5 but the yields are nearly
halved in each comparison. Second the weather profiles for each group
were plotted to look for areas of separation. Again, the goal is to find
a hybrid that is experiencing a crossover interaction so hybrids that
occur in different locations (i.e. different weather profiles) with
varying yields. There are some clear differences showcased between the
group comparisons above in Figure 6.

``` {.r}
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
```

#### Summary of Important Findings

A table of minimum, median, and maximum values by group in Table 4
allows closer inspection to see which weather variables are driving the
substantial differences between yields, Table 5.

``` {.r}
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
```
