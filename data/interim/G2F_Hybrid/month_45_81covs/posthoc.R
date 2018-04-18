library(PReMiuM)
library(tidyverse)

setwd("data/interim/G2F_Hybrid/month_45_81covs")
riskProfObj <- read_rds("riskProfObj.rds")


contVars <- riskProfObj$riskProfClusObj$clusObjRunInfoObj$continuousCovs
temp <- contVars[grep("Temp", contVars)]
dew <- contVars[grep("dew", contVars)]



plotRiskProfile(riskProfObj, "tempVars.png", whichCovariates = temp)
plotRiskProfile(riskProfObj, "dewVars.png", whichCovariates = dew)
