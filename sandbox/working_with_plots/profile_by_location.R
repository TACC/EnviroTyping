library(tidyverse)
library(lubridate)
library(ggforce)
library(ggstance)
library(viridis)

hybridByWeek <- read_csv("data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Exp" = col_factor(levels = NULL), "Pedi" = col_factor(levels = NULL), "Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))


val <- grep("Min|Max",names(hybridByWeek))
numericVars <- names(hybridByWeek[val])[vapply(hybridByWeek[val], function(x) var(x) != 0, logical(1))]
hybridByWeekSubset <- hybridByWeek %>% select(Exp, Hyb=Pedi, Week, numericVars)

# pdf("profile_by_location.pdf", paper = "a4")
ggplot(gather(hybridByWeekSubset,key,value, -c(Exp,Hyb,Week)),aes(Exp,value)) + 
    geom_violin(aes(fill=Exp)) +
    facet_wrap(~ key, scales="free_y", ncol = 5)+ 
    scale_fill_viridis(discrete = TRUE)+ 
    theme_bw()+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# dev.off()
riskProfObj <- readRDS("data/interim/G2F_Hybrid/hyb_wks3-5_by_week_1k/output/riskProfObj.rda")
weeks3_5 <- hybridByWeek %>% filter(Week >= isoweek(Planted) + 3 & Week <= isoweek(Planted) +5) %>% select(Exp, Hyb=Pedi,Yield, numericVars)
weeks3_5Clust <- data.frame(optClus = as.factor(riskProfObj$riskProfClusObj$clustering), weeks3_5)

ggplot(gather(weeks3_5Clust,key,value, -c(optClus,Exp,Hyb)),aes(optClus,value)) + geom_violin(aes(fill=optClus)) +
    facet_wrap(~ key, scales="free_y", ncol = 5)+ 
    scale_fill_viridis(discrete = TRUE)+ 
    theme_bw()+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
p <- ggplot(gather(weeks3_5Clust,key,value, -c(optClus,Exp,Hyb)),aes(optClus,value)) + geom_violin(aes(fill=optClus)) +
    facet_wrap_paginate(~ key, scales="free_y",nrow = 2, ncol = 4, page = 2)+ 
    scale_fill_viridis(discrete = TRUE)+ 
    theme_bw()+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
pages <- n_pages(p)
pdf("profile_by_cluster.pdf", paper = "a4r", width = 10)
for (i in 1:pages){
    # png(paste("profile_by_cluster_pg",i,".png", sep = ""), width = 1920, height = 1080)
    print({
        ggplot(gather(weeks3_5Clust,key,value, -c(optClus,Exp,Hyb)),aes(optClus,value)) + geom_violin(aes(fill=optClus)) +
        facet_wrap_paginate(~ key, scales="free_y",nrow = 2, ncol = 4, page = i)+ 
        scale_fill_viridis(discrete = TRUE)+
        ggtitle("Weather Profile by Cluster for Hybrid Weeks 3-5 Since Planted Subset") +
        theme_bw()+
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title = element_blank())
    })
    # dev.off()
}
dev.off()
# ggplot(gather(df1,key,value, -c(Exp,Hyb,Week)),aes(Exp,value)) + 
#     geom_violinh(aes(fill=Exp))+
#     facet_wrap(~ key, scales="free_x", ncol = 5)+
#     theme_bw()+
#     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
# 
# ggplot(gather(df1,key,value, -c(Exp,Hyb,Week)),aes(Exp,value)) + 
#     geom_sina(aes(fill=Exp))+
#     facet_wrap(~ key, scales="free_y", ncol = 5)+
#     theme_bw()+
#     theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# p <- ggplot(gather(df1,key,value, -c(Exp,Hyb,Week)),aes(Exp,value)) + 
#     geom_violin(aes(fill=Exp))+
#     facet_grid_paginate(~ key, scales="free_y", ncol = 4, nrow = 2, page = 1)+
#     theme_bw()+
#     theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# n_pages(p)


