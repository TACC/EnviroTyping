library(PReMiuM)
library(tidyverse)


dat = read_rds('~/EnviroTyping/sandbox/shifted_data_analysis/simulated/data/hyb_simulated.rds')

dat = dat[[1]]

desiNA = .40

##Isolate variables

min = colnames(dat)[grep('Min', colnames(dat))]
#min.ind = min
min = substring(min, 1, nchar(min)-2)
min = unique(min)

max = colnames(dat)[grep('Max', colnames(dat))]
#max.ind = max
max = substring(max, 1, nchar(max)-2)
max = unique(max)

mean = colnames(dat)[grep('Mean', colnames(dat))]
#mean.ind = mean
mean = substring(mean, 1, nchar(mean)-2)
mean = unique(mean)


##String of all variables

all.vars = c(min,mean,max)


##Choose variable

sel.var = all.vars[round(runif(1,1,length(all.vars)))]


##Set corresponding variable to NA and check total % of NA

dat[,grep(sel.var,colnames(dat))] = NA
propNA = mean(is.na(dat[,1:length(dat)]))

isTRUE(all.equal(0, desiNA - propNA, tolerance = .05))

newdat1 = naBlockFunc(dat, .10, seed = 12345)
newdat2 = naBlockFunc(dat, .10, seed = 12345)
identical(newdat1, newdat2)

all.loc = c(as.character(unique(dat$StYRe)))


newdat1 = naBlockFunc(dat, .20, "intervals", seed = 12345)
mean(is.na(newdat1[,1:length(newdat1)]))

dat = dat[[2]]

newdat2 = naBlockFunc(dat, .4, seed = 12345)







