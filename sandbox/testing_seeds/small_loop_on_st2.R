library(PReMiuM)
library(tidyverse)
library(tictoc)

setwd("work/04902/azg5169/stampede2/EnviroTyping/sandbox/testing_seeds")

df <- read_rds("work/04902/azg5169/EnviroTyping/data/interim/2015/hyb_by_mon_calib_wide_w_wth_nas.rds")

variance_var <- names(which(map_dbl(df[,16:length(df)], var, na.rm = TRUE) != 0))
min_vars <- str_subset(variance_var, "min")

set.seed(1234)
seed <- sample(1:100000,10)
tic.clearlog()
for (i in seed) {
    tic()
    dir_name <- paste0("work/04902/azg5169/stampede2/EnviroTyping/sandbox/testing_seeds", i, "_output")
    dir.create(dir_name)
    setwd(dir_name)
    runInfoObj <- profRegr(covNames, outcome = 'yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "pedi", continuousCovs = min_vars, data = df, nSweeps = 500, nBurn = 10, nProgress = 25, seed = i)
    toc(log = TRUE, quiet = TRUE)
    clus_file <- paste0(dir_name,"/output_nMembers.txt")
    nmembers <- str_split(read_lines(clus_file), pattern = " ", simplify = TRUE)
    empty_clusters <- nmembers %>% 
        as.tibble() %>% 
        mutate_all(funs(.==0)) %>% 
        reduce(`+`) %>% 
        as.tibble()
    write_delim(empty_clusters, "empty_clusters.txt")
}

seed_v_time <- bind_cols(seed = seed, time = unlist(tic.log(format = TRUE)))

write_csv(seed_v_time, "seed_v_time.csv")