library(tidyverse)
library(PReMiuM)
# library(caret)
# library(sampling)
library(splitstackshape)
setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_by_month_preds/stratifiedCV_45")

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_by_month_preds/stratifiedCV_45")
# dfl <- read_rds("../../hybrid_by_month_calibrated_weather_long_45subset.rds")
# table(dfl$Exp, dfl$Month)

df <- read_rds("../../hybrid_by_month_calibrated_weather_wide_45subset.rds")
# location_subset <- c("INH1", "KSH1", "MNH1", "MOH1", "OHH1")
# df1 <-  df %>% filter(Repl == 1)
# for (i in location_subset){
#     df2 <- df1 %>% filter(Exp == i)
#     print(i)
#     print(length(df2$Pedi))
# }
# hybrids <- (unique(df1$Pedi))
# nfolds=5
# sample(df1,45) %in% hybrids 
# 
# 
# #set.seed(1234)
# foldi=sample(rep(1:nfolds,length.out=length(df1$Pedi)))
# table(foldi)
# for (i in 1:5){
#     print(i)
#     testi=which(foldi==i)
#     test <- df1[testi,]
#     print(length(unique(test$Pedi)))
# }
# 
# folds <- 5
# hybSamp <- unique(df1$Pedi)
# # use caret::createFolds() to split the unique states into folds, returnTrain gives the index of states to train on.
# hybCvFoldsIN <- createFolds(1:length(hybSamp), k = folds, returnTrain=TRUE)
# for (i in hybCvFoldsIN){
# test <- df1[i,]
# print(length(unique(test$Pedi)))
# }
# obsIndexIn <- vector("list", folds) 
# for(i in 1:length(hybCvFoldsIN)){
#     x <- which(df1$Pedi %in%  hybSamp[hybCvFoldsIN[[i]]])
#     obsIndexIn[[i]] <- x
# }
# 
# sstrata <- strata(df1, stratanames = hybrids)
# df1s <- stratified(df, "Pedi", .2)
# sum(table(df1s$Exp))
# length(table(df1s$Pedi))
# 174/927

sub <- stratified(df, "Pedi", .25)

test <- stratified(sub, "Pedi", .3)
train <- anti_join(sub, test)
sum(table(test$Pedi))
sum(table(train$Pedi))
n_distinct(train$Pedi)
n_distinct(test$Pedi)
rSqrd=NULL
predErr=NULL
nfolds <- 5
for(k in 1:nfolds){
    test <- stratified(sub, "Pedi", .2)
    train <- anti_join(sub, test)
    contVars <- names(which(map_dbl(train[,16:96], var, na.rm = TRUE) != 0))
    runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed",discreteCovs = "Pedi", continuousCovs = contVars, data = train, predict = test, nSweeps = 100, nBurn = 900, seed = 1234)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists, maxNClusters = 9)
    riskProfObj <- calcAvgRiskAndProfile(clusObj)
    predictions <- calcPredictions(riskProfObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
    print(rsqrd <- 1-(sum((predictions$observedY - predictions$predictedY)^2)/sum((predictions$observedY - mean(predictions$observedY))^2)))
    print(predictions$rmse)
    predErr=c(predErr,predictions$rmse)
    rSqrd=c(rSqrd,rsqrd)
}
print(mean(rSqrd))
print(mean(predErr))

