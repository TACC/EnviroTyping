library(PReMiuM)
library(dplyr)
library(readr)
library(ggplus)
library(optCluster)


df <- read_csv("data/interim/G2F_Hybrid/hybrid_by_weeksSincePlanted_cleaned_weather.csv",
               col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(),
                                "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(),
                                "solarMax" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number()))

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/premiumOutput_2.15.18_3-5wksSubset")

temp <- df %>% filter(seed_3_5 == TRUE)

clusObj <- readRDS("output_seed_3_5/clusObj.rda")

optAlloc <- clusObj$clustering

tmp_vp<-data.frame(opt=as.integer(as.factor(optAlloc)), outc=clusObj$clusObjRunInfoObj$yMat, known=as.integer(as.factor(temp$Pedi)))

clus <- optCluster(tmp_vp, 2:10, clMethods = "hierarchical")

d <- dist(tmp_vp)
clusters <- hclust(d)
plot(clusters)

# library("NbClust")
# nb <- NbClust(data = tmp_vp, min.nc = 2, max.nc = 10, method = "kmeans")

library("factoextra")
fviz_nbclust(nb)
# pdf("./violinPlots.pdf", paper="a4")
# p2 <- ggplot(tmp_vp, aes(x=opt, y=outc)) +
#     geom_violin(aes(fill = opt)) + 
#     geom_point(size = 1, stroke = 1, shape = 16) +
#     labs(x="clusters", y = "outcome") +
#     scale_y_continuous(breaks=seq(0, 300, 75))
# 
# facet_multiple(plot = p2, 
#                facets = 'known', 
#                ncol = 3, 
#                nrow = 3)
# dev.off()