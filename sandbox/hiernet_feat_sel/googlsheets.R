devtools::install_github("jennybc/googlesheets")
library(googlesheets)

varImp_ss <- gs_new("hierNetvarImp", ws_title = "hierNet Variable Importance", input = varRankFit2, trim = TRUE, verbose = TRUE)
varImp_ss %>% 
    gs_read()
