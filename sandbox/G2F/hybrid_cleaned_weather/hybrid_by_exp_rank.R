# imports all the packages i'll need
library(tidyverse)

# read the raw data as a tibble/data.frame
hyb <-  read_csv("data/external/G2F from Cyverse DataStore/g2f_2015_hybrid_data_no_outliers.csv",col_types = cols("Date Planted" = col_date("%m/%d/%Y"), "Date Harvested" = col_date("%m/%d/%Y")))


# tidy the data
hyb1 <- hyb %>% 
    
    # choosing variables to keep and renaming
    select(Exp = "Field-Location", Pedi = "Pedigree", Repl = Replicate, Planted = "Date Planted",
           Harvest = "Date Harvested",plantHt = "Plant height [cm]", earHt = "Ear height [cm]",
           testWt = "Test weight [lbs]",plotWt = "Plot Weight [lbs]", Yield = "Grain yield [bu/acre]") %>% 
    
    # changing the sort 
    arrange(Exp, Pedi, Repl)

rank <- hyb1 %>% group_by(Exp,Pedi) %>% summarise(max = max(Yield))  %>% mutate(Rank = dense_rank(desc(max))) %>% arrange(Exp, Rank) %>% drop_na()

# check for NA's
rank %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

top1 <- filter(rank, Rank==1)
top1$Exp

