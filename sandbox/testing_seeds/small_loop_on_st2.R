library(PReMiuM)
library(tidyverse)
library(tictoc)

setwd("/work/04734/dhbrand/stampede2/github/EnviroTyping/sandbox/testing_seeds/")

df <- read_rds("~/github/EnviroTyping/data/interim/2015/hyb_by_mon_calib_wide_w_wth_nas.rds")

variance_var <- names(which(map_dbl(df[,16:length(df)], var, na.rm = TRUE) != 0))
min_vars <- str_subset(variance_var, "min")

set.seed(1234)
seed <- sample(1:10000,10)
tic.clearlog()
for (i in seed) {
    tic()
    dir_name <- paste0("/work/04734/dhbrand/stampede2/github/EnviroTyping/sandbox/testing_seeds/", i, "_output")
    dir.create(dir_name)
    setwd(dir_name)
    runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min_vars, data = df, nSweeps = 20, nBurn = 1, nProgress = 1, seed = i)
    toc(log = TRUE, quiet = TRUE)
    clus_file <- paste0(dir_name,"/output_nMembers.txt")
    readLines(clus_file)
    }

seed_v_time <- bind_cols(seed = seed, time = unlist(tic.log(format = TRUE)))

write_csv(seed_v_time, "seed_v_time.csv")
