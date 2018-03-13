library(FW)
library(coda)
library(readr)
library(dplyr)
library(lubridate)
library(tictoc)
setwd("~/GitHub/EnviroTyping/sandbox/G2F/fw")

data(wheat)
attach(wheat.Y)

lm1 = FW(y = y, VAR = VAR, ENV = ENV, method = "OLS")
lm2 = FW(y = y,VAR = VAR,ENV = ENV)


df <- read_csv("../../../data/interim/G2F_Hybrid/hybrid_by_week_cleaned_weather.csv",col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(), "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(), "solarMax" = col_number(), "solarMedian" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number(), "windDirMedian" = col_number()))

df <- df %>% filter(Week >= isoweek(Planted)+3 & Week <= isoweek(Planted)+5)
ck <- df %>% filter(Pedi == "B73_PHG39-6/LH82")
ck1 <- df %>% filter(Pedi == "DKB64-69")
val <- grep("Min|Max",names(df))
numericVars <- names(df[val])[vapply(df[val], function(x) var(x) != 0, logical(1))]

tic()
Gibbs <- FW(y = df$Yield, VAR = df$Pedi, ENV = df$Exp, seed = 12345, saveAt = "Gibbs", nIter = 50000, burnIn = 5000, saveENV = as.integer(1))
toc() # 2551.945 sec elapsed 

tic()
OLS <- FW(y = df$Yield, VAR = df$Pedi, ENV = df$Exp, method = "OLS")
toc()

load("Gibbssamps.rda")
HPDinterval(samps[,c('var_e','var_g','var_b','var_h')])

cor(df$Yield,Gibbs$yhat)

plot(Gibbs,main="Gibbs", cex = 0.2,lwd = 0.2)
plot(Gibbs, plotVAR = c("F42/MO17","LH195/LH185","M0325/LH185", "P9855HR", "GEMS-0162/LH82"), main="Gibbs")
plot(Gibbs, plotVAR = top1$Pedi, main="Gibbs")
plot(Gibbs, plotVAR = c("B73/MO17", "LH132/LH51"), main="Gibbs")
rank3_5 <- df %>% group_by(Exp,Pedi) %>% summarise(max = max(Yield))  %>% mutate(Rank = dense_rank(desc(max))) %>% arrange(Exp, Rank) %>% drop_na()
top1 <- filter(rank3_5, Rank==1)
top1$Pedi

cvm <- cov(as.numeric(as.factor(df$Pedi)),as.matrix(df[numericVars]))
length(df$Pedi)

load("Gibbssamps.rda")
plot(samps,ask = T)

autocorr(samps[[1]][,"var_e"])
summary(samps)

