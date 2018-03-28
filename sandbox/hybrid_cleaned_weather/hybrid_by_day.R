# imports all the packages i'll need
library(tidyverse)

# read the raw data as a tibble/data.frame
hyb <-  read_csv("data/external/G2F from Cyverse DataStore/g2f_2015_hybrid_data_no_outliers.csv", col_types = cols("Pedigree" = col_factor(levels = NULL),  "Date Planted" = col_date("%m/%d/%Y"), "Date Harvested" = col_date("%m/%d/%Y")))


# tidy the data
hyb1 <- hyb %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Field-Location", Pedi = "Pedigree", Repl = Replicate, Planted = "Date Planted",
           Harvest = "Date Harvested",plantHt = "Plant height [cm]", earHt = "Ear height [cm]",
           testWt = "Test weight [lbs]",plotWt = "Plot Weight [lbs]", Yield = "Grain yield [bu/acre]") %>% 
    
    # changing the sort 
    arrange(Exp, Pedi, Repl)

# joining the tidy weather data with min/max variables
# right join to preserve weather data and fill matching hybrid data to each expermient
hybrid <- right_join(hyb1, wth4, by = "Exp") %>% 
    mutate(Exp = as_factor(Exp)) %>% 
    drop_na(Yield)

# check for NA's
hybrid %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

# hybrid <- hybrid %>% 
#     mutate(wks3_4 = (Date >= Planted + weeks(3) & Date <= Planted + weeks(4)), wks5_6 = (Date >= Planted + weeks(5) & Date <= Planted + weeks(6)), wks7_8 = (Date >= Planted + weeks(7) & Date <= Planted + weeks(8)))

hybrid[,c(6:10,13:52)] <- as.numeric(unlist(hybrid[,c(6:10,13:52)]))


# filter(hybrid, wks3_4 == TRUE) 38,080 x 55
# filter(hybrid, wks5_6 == TRUE) 39,712 x 55
# filter(hybrid, wks7_8 == TRUE) 41,028 x 55

# write the data to csv
write_csv(hybrid, "data/interim/G2F_Hybrid/hybrid_by_weeksSincePlanted_cleaned_weather.csv")

write_rds(hybrid, "data/interim/G2F_Hybrid/hybrid_by_day_cleaned_weather.Rds", compress = "xz", compression = 1L)
 
check <- read_rds("data/interim/G2F_Hybrid/hybrid_by_day_cleaned_weather.Rds")
identical(check,hybrid)
