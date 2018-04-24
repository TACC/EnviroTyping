library(PReMiuM)
library(tidyverse)

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/month_45_81covs")
riskProfObj <- read_rds("riskProfObj.rds")


contVars <- riskProfObj$riskProfClusObj$clusObjRunInfoObj$continuousCovs
temp <- contVars[grep("Temp", contVars)]
dew <- contVars[grep("dew", contVars)]



plotRiskProfile(riskProfObj, "tempVars.png", whichCovariates = temp)
plotRiskProfile(riskProfObj, "dewVars.png", whichCovariates = dew)

members <- as.numeric(strsplit(readLines("output/output_nMembers.txt",1)," ")[[1]])

ind <- which(members != 0)
clusters <- members[ind]
str(clusters)
max(clusters[-876])
table(clusters[-876])
