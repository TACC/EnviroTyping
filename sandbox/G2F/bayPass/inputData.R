library(PReMiuM)
library(readr)
library(dplyr)
library(tibble)

df <- read_csv("data/interim/G2F_Hybrid/hybrid_bymonth_cleaned_weather.csv",
               col_types = cols(plantHt = col_double(),
                                earHt = col_double(),
                                StatID = col_double(),
                                Year = col_double(),
                                Month = col_double(),
                                humidMin = col_double(),
                                solarMin = col_double(),
                                solarMax = col_double(),
                                rainMin = col_double(),
                                rainMedian = col_double(),
                                windDirMin = col_double(),
                                windDirMax = col_double()
               ))

setwd("scripts/G2F/bayPass")
may <- df %>% filter(Month == 5)

minMax <- grep("Min|Max",names(may))

noVar <- names(may[minMax])[vapply(may[minMax], function(x) var(x) != 0, logical(1))]

efile <- may %>% select(Pedi, Yield, noVar) %>% group_by(Pedi) %>% summarise_all(mean)

efile <- as_tibble(cbind(nms = names(efile), t(efile)))

efile <- efile[,2:867]

colnames(gfile) <- efile[1,]

efile <- efile[-1,]

write_delim(efile, "efile.txt", col_names = FALSE)

efile <- data.matrix(efile)

omega <- cov(efile)

NCols=866*2
NRows=1000

gfile <- matrix(sample(1:100,NCols*NRows, replace = TRUE), ncol=NCols) 

gfile <- as.tibble(gfile)

write_delim(gfile, "gfile.txt", col_names = FALSE)

gfile1 <- matrix(rep(seq(1,19),NCols), ncol=NCols) 

gfile1 <- as.tibble(gfile1)

write_delim(gfile1, "gfile1.txt", col_names = FALSE)

#source the baypass R functions (check PATH)
source("utils/baypass_utils.R")
#load the bovine covariance matrix
om.bta <- as.matrix(read.table("examples/omega.bta"))

#simulate allele count data for 1000 SNPs
simu.res <- simulate.baypass(omega.mat=omega)
#simulate allele count data for 1000 neutral SNPs and
#100 associated SNPs with varying regression coefficients
simu.res1 <- simulate.baypass(omega.mat=om.bta,beta.coef=runif(100,-0.2,0.2),
                             pop.trait=rnorm(18))
#simulate read count data for 1000 SNPs
simu.res2 <- simulate.baypass(omega.mat=om.bta,coverage=50)
