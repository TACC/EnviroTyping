med.wth <- names(wthmon)[grep("Median",names(wthmon))]
wthmon.med <- wthmon  %>% select(1:4, med.wth)

ggplot(gather(wthmon.med,key,value, -c(1:4)),aes(value)) + 
    geom_histogram(binwidth = 1, color = "black", fill = "blue") +
    facet_wrap(~ key, scales = "free") +
    theme_bw()

read_rds("data/interim/G2F_Hybrid/hybrid_by_month_calibrated_weather_wide.rds")


dew.temp <- names(df)[grep("dew|temp",names(df))]
df.dew.temp <- df  %>% select(1:4, dew.temp)
ggplot(gather(df.dew.temp,key,value, -c(1:4)),aes(value)) + 
    geom_histogram(binwidth = 1, color = "black", fill = "blue") +
    facet_wrap(~ key, scales = "free") +
    theme_bw()

solar.humid <- names(df)[grep("solar|humid",names(df))]
df.solar.humid <- df  %>% select(1:4, solar.humid)
ggplot(gather(df.solar.humid,key,value, -c(1:4)),aes(value)) + 
    geom_histogram(binwidth = 5, color = "black", fill = "blue") +
    facet_wrap(~ key, scales = "free") +
    theme_bw()


shift.all.stats <- read_rds("data/interim/G2F_Hybrid/hybrid_by_month_shift_all_stats.rds")
mean.shift <- shift.all.stats %>% select(1:4,contains("mean"))
median.shift <- shift.all.stats %>% select(1:4,contains("median"))

solar.humid <- names(mean.shift)[grep("solar|humid",names(mean.shift))]
df.solar.humid <- mean.shift  %>% select(1:4, solar.humid)
ggplot(gather(df.solar.humid,key,value, -c(1:4)),aes(value)) + 
    geom_histogram(binwidth = 5, color = "black", fill = "blue") +
    facet_wrap(~ key, scales = "free") +
    theme_bw()

median.rain <- names(median.shift)[grep("rain",names(median.shift))]
df2.rain <- median.shift  %>% select(1:4, median.rain)
ggplot(gather(df2.rain,key,value, -c(1:4)),aes(value)) + 
    geom_histogram(binwidth = .025, color = "black", fill = "blue") +
    facet_wrap(~ key, scales = "free") +
    theme_bw()

mean.rain <- names(mean.shift)[grep("rain",names(mean.shift))]
df.rain <- mean.shift  %>% select(1:4, mean.rain)
ggplot(gather(df.rain,key,value, -c(1:4)),aes(value)) + 
    geom_histogram(binwidth = .025, color = "black", fill = "blue") +
    facet_wrap(~ key, scales = "free") +
    theme_bw()
