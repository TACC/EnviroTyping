library(tidyverse)
library(ggforce)
library(ggstance)
library(viridis)

df <- read_csv("../../../interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))
df %>% as_factor(Pedi)
class(df$Pedi)
val <- grep("Min|Max",names(df))
numericVars <- names(df[val])[vapply(df[val], function(x) var(x) != 0, logical(1))]
df1 <- df %>% select(Exp, Hyb =Pedi, Week, numericVars)
df1$Hyb <- as_factor(df1$Hyb)

pdf("profile_by_location.pdf", paper = "a4")
ggplot(gather(df1, key, value, -c(Exp,Hyb,Week)), aes(Week, value)) + 
    geom_smooth(colour = "green", fill = "purple") + geom_point(aes(color = Exp),size = .5, stroke = .1, shape = 16, colour = "red") +
    facet_wrap(~ key, scales="free_y", ncol=5) + ggtitle("Profile by Location")
dev.off()

ggplot(gather(df1,key,value, -c(Exp,Hyb,Week)),aes(Exp,value)) + 
    geom_violin(aes(fill=Exp)) +
    facet_wrap(~ key, scales="free_y", ncol = 5)+ 
    scale_fill_viridis(discrete = TRUE)+ 
    theme_bw()+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# ggplot(gather(df1,key,value, -c(Exp,Hyb,Week)),aes(Exp,value)) + 
#     geom_violinh(aes(fill=Exp))+
#     facet_wrap(~ key, scales="free_x", ncol = 5)+
#     theme_bw()+
#     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggplot(gather(df1,key,value, -c(Exp,Hyb,Week)),aes(Exp,value)) + 
    geom_sina(aes(fill=Exp))+
    facet_wrap(~ key, scales="free_y", ncol = 5)+
    theme_bw()+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# p <- ggplot(gather(df1,key,value, -c(Exp,Hyb,Week)),aes(Exp,value)) + 
#     geom_violin(aes(fill=Exp))+
#     facet_grid_paginate(~ key, scales="free_y", ncol = 4, nrow = 2, page = 1)+
#     theme_bw()+
#     theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# n_pages(p)



