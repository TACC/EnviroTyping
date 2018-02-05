# imports all the packages i'll need
library(tidyverse)

setwd("~/Stapleton_Lab/Projects/Premium/inbredAnalysis/")

# read the raw data as a tibble/data.frame
ib <- read_csv("~/Stapleton_Lab/Downloads/g2f_2015_inbred_raw_data.csv")

# tidy the data
ib1 <- ib %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Field-Location", Pedi = "Pedigree", Repl = Replicate, Harvest = "Date Harvested",
           plantHt = "Plant Height [cm]", earHt = "Ear Height [cm]") %>% 
    
    # changing the sort 
    arrange(Exp, Pedi, Repl)


# joining the tidy weather data with min/max variables
# right join to preserve weather data and fill matching hybrid data to each expermient
inbred <- right_join(ib1, wth2, by = "Exp") %>% 
    
    drop_na(-Repl)

# check for NA's
inbred %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

# write the data to csv
write_csv(inbred, "./inbredXmonth.csv")
