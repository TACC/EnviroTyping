asd <- read_rds("../../hybrid_by_month_calibrated_weather.rds")
table(asd$Exp,asd$Month)
library(magrittr)
names(df)

var_keep <- names(df)[grep("dew|temp", names(df))]
mon_keep <- var_keep[grep("[5-8]", var_keep)]
exp_keep <- c("GAH1", "ILH1", "INH1", "KSH1", "MNH1", "MOH1", "MOH2", "OHH1", "ONH1")
pedi_keep <- sample(df$Pedi, 3)
df1 <- df %>% filter(Exp %in% exp_keep & Pedi %in% pedi_keep ) %>%  select(Pedi, Yield, mon_keep)

write_rds(df1, "../../toy_data.rds")
