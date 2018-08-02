library(amap)
library(cluster)
library(dendextend)
library(colorspace)
library(tidyverse)
library(magrittr)
library(lubridate)
library(tictoc)
library(dynamicTreeCut)
library(viridis)
library(gridExtra)
library(reshape2)
library(rlang)
setwd("~/github/EnviroTyping/sandbox/posthoc_group_analysis/2014/")


hyb_mon <- read_rds("../../../data/interim/2014/hyb_by_mon_calib_wide_shifted.rds")

variance_var <- names(which(map_dbl(hyb_mon[,16:207], var, na.rm = TRUE) != 0))
min_vars <- str_subset(variance_var, "min")
hyb_mon_min <- hyb_mon %>% select(pedi, min_vars ) %>% modify_at(1, as_factor) %>% modify_at(1, as.integer) %>% mutate (pedi = pedi - 1)

reduced <- hyb_mon  %>% select(yield) %>% round(digits = 3) %>% as.vector()
post_analysis <- cbind(yield = clusObj$clusObjRunInfoObj$yMat) %>% round(digits = 3) %>% as.vector()
no_outliers <- which(equals(reduced, post_analysis)==TRUE)

clusObj <- read_rds("../../shifted_data_analysis/2014/min_vars_3000_no_outliers/clusObj.rds")
xmat <- clusObj$clusObjRunInfoObj$xMat

hyb_mon_clus <- cbind(clus = clusObj$clustering, yield = clusObj$clusObjRunInfoObj$yMat, clusObj$clusObjRunInfoObj$xMat)
cores <- parallel::detectCores()
tic()
posthocGroup <- hcluster(hyb_mon_clus[,-2], method = "euclidean", link = "ward", nbproc = cores)
toc() # 26.264 sec elapsed
# plot(posthocGroup)
# rect.hclust(posthocGroup, k=14, border="red")
# pruned <- maptree::clip.clust(posthocGroup,weeks3_5Clust, k=14)
# maptree::draw.clust(pruned)

k <- 14
cols <- rainbow_hcl(k)
dend <- as.dendrogram(posthocGroup)
dend <- color_branches(dend, k = k)
plot(dend, leaflab='none')
labels_dend <- labels(dend)
groups <- cutree(dend, k, use_labels_not_values = TRUE, order_clusters_as_data = FALSE)
labels_to_keep <- labels_dend[k != groups]
dends <- prune(dend, labels_to_keep)
saveRDS(dends, "dends.rda")
dends <- read_rds("dends.rda")
plot(dends)
dist <- daisy(hyb_mon_clus[,-2])
tic()
dynamicCutree <- cutreeDynamic(posthocGroup, distM = as.matrix(dist), deepSplit = 4, verbose = 4)
toc()# 55.688 sec elapsed
length(unique(dynamicCutree)) # 14

hyb_mon_groups <- data.frame(group = dynamicCutree, hyb_mon_clus)

# original method to scale data using grid package for multiple plots on same page
l <- list()
t <- list()
for (i in 1:6){
temp <- hyb_mon_groups %>% filter(group == 1)
temp <- apply(temp[5:28],2,summary)
t[[i]] <- temp
data <- rowid_to_column(as.tibble(t(scale(temp))), "ID")
data <- data %>% select(ID, Min = Min., Median, Max = Max.)
df <- melt(data ,  id.vars = 'ID', variable.name = 'series')

p <- ggplot(df, aes(ID,value)) + geom_point(aes(color=series, size = 10), show.legend = FALSE) + labs(title = paste("Scaled Weather Profile for Group",i,sep = " ")) +
    scale_x_continuous(breaks = c(1:24), labels = names(hyb_mon_groups[,c(5:28)])) + theme_bw() +
    theme(axis.text.x = element_text(face="bold", size=6, angle=45,margin = margin(t = 10)), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank())
l[[i]] <- p
}

#pdf("profileByGroup.pdf", paper = "a4r")
grid.arrange(l[[1]],l[[2]], l[[3]],l[[4]],l[[5]],l[[6]], ncol = 2)
#dev.off()

# trying to improve for readability

scale <- hyb_mon_groups %>% 
    group_by(group) %>% 
    summarise_at(5:27, funs(min, max, median), na.rm = TRUE) %>% 
    select(-1) %>% 
    scales::rescale(., to = c(0,10)) %>% 
    as.tibble() %>% 
    cbind(group = seq(1:6), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

scale_this <- function(x){
    (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}
scale_2 <- hyb_mon_groups %>% 
    group_by(group) %>% 
    summarise_at(5:27, funs(min, max, median), na.rm = TRUE) %>% 
    select(-1) %>% 
    map_df(rescale, to = c(0,1)) %>% 
    cbind(group = seq(1:6), .) %>% 
    gather(key, value, -group) %>% 
    separate(key, into = c("var", "stat"), sep = "_(?!.*_)")

scale_2 %>% ggplot(aes(var, value)) +
    geom_point(aes(color = stat, size = 10), show.legend = FALSE) +
    facet_wrap(~ group) + 
    labs(title = "Scaled Weather Profile by Group", x = "Weather Variables with Month Number as Suffix") +
    theme(axis.text.x = element_text(face="bold", size = 10, angle = 45, margin = margin(t = 30))) +
    scale_color_manual(values = c(min = "red", median = "green", max = "blue"))


