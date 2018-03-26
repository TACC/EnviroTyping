library(tidyverse)
library(ClusterR)
library(mclust)
df <- read_csv("../../../data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))

val <- grep("Min|Max",names(df))
numericVars <- names(df[val])[vapply(df[val], function(x) var(x) != 0, logical(1))]

df1 <- df %>% select(Hyb = Pedi, numericVars)
df1$Hyb <- parse_factor(df1$Hyb,levels=NULL, include_na = FALSE)
df1$Hyb <- as.numeric(df1$Hyb)

optClus <- Optimal_Clusters_Medoids(as.data.frame(df1),24,'euclidean',threads=95)

saveRDS(optClus, "optClus.rda")

BIC <- mclustBIC(df1)
summary(BIC)

mod1 <- Mclust(df1, G = 1:25,verbose = TRUE)
summary(mod1, parameters = TRUE)
plot(mod1, what = "classification")
mod1dr <- MclustDR(mod1)
summary(mod1dr)
plot(mod1dr, what = "pairs")
plot(mod1dr, what = "boundaries", ngrid = 200)
