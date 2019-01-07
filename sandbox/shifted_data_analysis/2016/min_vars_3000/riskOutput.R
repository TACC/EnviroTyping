library(PReMiuM)
library(tidyverse)

# Use 2016 wide, shifted, all months, min_vars_3000 clusObj to create riskProfObj

setwd("/work/06019/bno5761/stampede2/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_3000")

clusObj = read_rds("clusObj.rds")

riskProfObj = calcAvgRiskAndProfile(clusObj)

write_rds(riskProfObj, "/riskProfObj.rds", compress = "xz")