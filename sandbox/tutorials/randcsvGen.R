install.packages(c("dplyr","readr"), repos="https://cran.revolutionanalytics.com/")

library(readr)
library(dplyr)

setwd("./randcsv")


for (i in 1:100) {
    df <- mtcars %>% select(sample(names(mtcars),6))
    write_csv(df,paste("df",i,".csv",sep = ""))
}
