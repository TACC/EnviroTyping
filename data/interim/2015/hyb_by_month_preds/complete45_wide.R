library(PReMiuM)
library(tidyverse)
library(tictoc)

setwd("/work/04734/dhbrand/stampede2/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_by_month_preds/complete45_wide")

df45wide <- read_rds("../../hybrid_by_month_calibrated_weather_wide_45subset.rds")
#df45wide <- df45wide[sample(1:nrow(df45wide),.2*dim(df45wide)[1]),]
set.seed(1234)
train_index <- sample(1:nrow(df45wide), 0.5 * nrow(df45wide))
test_index <- setdiff(1:nrow(df45wide), train_index)
train45 <- df45wide[train_index,]
test45 <- df45wide[test_index,]

contVars <- names(which(map_dbl(train45[,16:96], var, na.rm = TRUE) != 0))
tic()
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed",discreteCovs = "Pedi", continuousCovs = contVars, data = train45, predict = test45, nSweeps = 1000, nBurn = 1000, seed = 1234)
print(toc())
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
predictions <- calcPredictions(riskProfObj,fullSweepPredictions=TRUE,fullSweepLogOR=TRUE)
print(rsqrd <- 1-(sum((predictions$observedY - predictions$predictedY)^2)/sum((predictions$observedY - mean(predictions$observedY))^2)))
print(predictions$rmse)
save(file = "complete45_wide.rds")
