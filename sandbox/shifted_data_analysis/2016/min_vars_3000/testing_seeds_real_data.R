library(tidyverse)
library(PReMiuM)

setwd("/work/04734/dhbrand/stampede2/github/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_3000/output")
df <- read_rds("../../../../../data/interim/2016/hyb_by_mon_calib_wide_shifted.rds")

variance_var <- names(which(map_dbl(df[,16:255], var, na.rm = TRUE) != 0))
min_vars <- str_subset(variance_var, "min")

clusObj <- list()
seed <- c(54161148, 96222856, 43479820, 54824768, 47347836, 69778632, 75928328, 04363405, 45616084, 48415972)
for (i in seed) {
    runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min_vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, seed = i)
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj[[i]] <- calcOptimalClustering(calcDists)
}
write_rds(clusObj, "clusObj.rds", compress = "xz")


runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min_vars, data = df, nSweeps = 3000, nBurn = 50, nProgress = 100, seed = 04363405)
