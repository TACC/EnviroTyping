library(readr)

args <- commandArgs(trailingOnly = TRUE)

dat <- args[1]

df <- read_csv(dat)

head(df)
