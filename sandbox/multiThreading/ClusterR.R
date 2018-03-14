library(ClusterR)
library(lubridate)
library(tidyverse)
library(Matrix)
library(gdata)
data(mushroom)

X = mushroom[, -1]

y = as.numeric(mushroom[, 1]) # convert the labels to numeric

gwd = FD::gowdis(X) # calculate the 'gower' distance for the factor variables

gwd_mat = as.matrix(gwd) # convert the distances to a matrix

cm = Cluster_Medoids(gwd_mat, clusters = 2, swap_phase = TRUE, verbose = F)

hybridByDay <- read_csv("../../../data/interim/G2F_Hybrid/hybrid_by_day_cleaned_weather.csv")

hybridByWeek <- read_csv("../../../data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))

week3a <- hybridByDay %>% filter(Date >= Planted + weeks(3) & Date < Planted + weeks(4))

week3b <- hybridByWeek %>% filter(Week >= isoweek(Planted) + 3 & Week < isoweek(Planted) + 4)

val <- grep("Min|Max",names(week3b))
numericVars <- names(week3b[val])[vapply(week3[val], function(x) var(x) != 0, logical(1))]
numericVars <- numericVars[-c(9:10)]
week3bSubset <- week3b %>% select(Exp, Hyb = Pedi, Yield, numericVars)
week3bSubset$Hyb <- as.numeric(as_factor(week3bSubset$Hyb))
week3bSubset$Exp <- as.numeric(as_factor(week3bSubset$Exp))
week3bMatrix <- as.matrix(week3bSubset[-3])

optcm <- Optimal_Clusters_Medoids(week3bMatrix, 25, swap_phase = TRUE, verbose = T)

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_wks3-5_by_week_1k/output")
riskProfObj <- readRDS("riskProfObj.rda")
diss <- riskProfObj$riskProfClusObj$clusObjDisSimMat
length(diss)
mat <- Matrix(diss)

hollow <- Diagonal(n = length(diss),diss)
sum(diag(hollow))

x <- matrix(LETTERS[1:25], nrow=5, ncol=5, byrow=TRUE)
x
lowerTriangle(x) = upperTriangle(x, byrow=TRUE)
diag(x) <- 0
x
empty <- Matrix(0, nrow = length(diss)+1, ncol = length(diss)+1, sparse = TRUE)
object.size(empty)
