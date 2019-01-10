library(tidyverse)

setwd("/work/04902/azg5169/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_3000")

clusObj = read_rds('clusObj1.rds')
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "riskProfObj.rds")

