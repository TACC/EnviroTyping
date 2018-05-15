
wth16.na <- wth %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))


wth15.na <- wth2 %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

wth.na <- bind_rows(wth15.na, wth16.na)
wth.na <- bind_cols(Year = c("2015", "2016"), wth.na) 

cols <- names(wth.na)[2:31]
wth.na %<>% mutate_at(cols,funs(as.numeric(.)))

na.diff <- wth.na[1,2:31] - wth.na[2,2:31]
wth.na <- bind_rows(wth.na, na.diff)
write_csv(wth.na, "sandbox/interim_datasets/2016/wth_na_diff.csv")

wth16mon.na <- ungroup(wthmon) %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))


wth15mon.na <- ungroup(wthmon) %>% 
    select_if(function(x) any(is.na(x))) %>% 
    summarise_all(funs(sum(is.na(.))))

wthmon.na <- bind_rows(wth15mon.na, wth16mon.na)


