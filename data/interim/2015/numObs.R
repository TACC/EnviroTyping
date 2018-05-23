library(readr)
library(dplyr)
library(lubridate)

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid")
df <- read_csv("hybrid_by_day_cleaned_weather.csv")
df1 <- read_csv("hybrid_by_week_cleaned_weather.csv")

ss <- df %>% filter(Date >= Planted + weeks(3) & Date <= Planted + weeks(5))
ss <- df %>% filter(Date >= Planted + weeks(7) & Date <= Planted + weeks(8))
ss <- df %>% filter(Date >= Planted + weeks(7) & Date <= Planted + weeks(8))

ss4 <- df %>% mutate(Mon = month(Date)) %>% filter(Mon == 4)
ss5 <- df %>% mutate(Mon = month(Date)) %>% filter(Mon == 5)
ss6 <- df %>% mutate(Mon = month(Date)) %>% filter(Mon == 6) 
ss7 <- df %>% mutate(Mon = month(Date)) %>% filter(Mon == 7)
ss8 <- df %>% mutate(Mon = month(Date)) %>% filter(Mon == 8)
ss9 <- df %>% mutate(Mon = month(Date)) %>% filter(Mon == 9)
ss10 <- df %>% mutate(Mon = month(Date)) %>% filter(Mon == 10)

sss4 <- df1 %>% filter(Month == 4)
sss5 <- df1 %>% filter(Month == 5)
sss6 <- df1 %>% filter(Month == 6) 
sss7 <- df1 %>% filter(Month == 7)
sss8 <- df1 %>% filter(Month == 8)
sss9 <- df1 %>% filter(Month == 9)
sss10 <- df1 %>% filter(Month == 10)
unique(df1$Harvest)

season <- df %>% filter(Date >= Planted & Date <=Harvest)

wks3_5 <- df1 %>% filter(Week >= isoweek(Planted)+3 & Week <= isoweek(Planted)+5)
