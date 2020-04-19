
library(PReMiuM)
library(tidyverse)
library(plyr)
#install.packages('gtools')
library(gtools)
#install.packages('tictoc')
library(tictoc)
library(ggplot2)

source("D:/GitHub/EnviroTyping/sandbox/Simulation/naBlockFunc.R")
dat = read_rds('D:/GitHub/EnviroTyping/sandbox/shifted_data_analysis/simulated/data/hyb_simulated.rds')

dat = dat[[1]]

crossv = NULL
seeds = c(10388733, 35149852, 93062232, 33227212, 98830152, 15923455, 60741424, 40189648, 35747108, 25172592)
for (j in 1:length(seeds)){
    set.seed(seeds[j])
    results = NULL
    res.time = NULL
for (i in seq(from = 1, to = 5, length.out = 9)){

    tic()
    
    df = naBlockFunc(dat, i/10, "intervals")

    variance.var <- names(which(map_dbl(df[,6:93], var, na.rm = TRUE) != 0))
    min.vars <- str_subset(variance.var, "Min")


    runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 1000, nProgress = 500)
    
    calcDists <- calcDissimilarityMatrix(runInfoObj)
    clusObj <- calcOptimalClustering(calcDists)
    
    temp = data.frame(c(i/10, clusObj$nClusters, clusObj$clusterSizes))
    results = data.frame(rbind.fill(results,data.frame(t(c(i/10, clusObj$nClusters, clusObj$clusterSizes)))))
    
    tmptim = toc(quiet = TRUE)
    res.time = rbind(res.time, as.numeric(tmptim[[2]]-tmptim[[1]]))
}
    #colnames(crossv) = c(1:length(crossv))
    #colnames(results) = c(1:length(results))
    results = cbind(results[,1:2],res.time, results[,3:length(results)])
    crossv = rbind.fill(crossv, results)
}

crossv[,1] = as.factor(crossv[,1])
inter.res = t(rbind(tapply(crossv[,2],crossv[,1],mean),tapply(crossv[,3],crossv[,1],mean)))
colnames(inter.res) = c('nCLusters','Time')

full.res = NULL
for (i in 1:5){
    tmp.c = tapply(crossv[,2], crossv[,1], rbind)[[i]]
    full.res = rbind(full.res, tmp.c)
}
for (i in 1:5){
    tmp.c = tapply(crossv[,3], crossv[,1], rbind)[[i]]
    full.res = rbind(full.res, tmp.c)
}
full.res = cbind(crossv[1:10,1],full.res)
full.res[1:10,1] = full.res[1:10,1]/10
write_rds(inter.res, "interres.rds", compress = "xz")

write_rds(full.res, "fullintres.rds", compress = "xz")







#####################################################################
#####################################################################
#####################################################################
#####################################################################



source("D:/GitHub/EnviroTyping/sandbox/Simulation/naBlockFunc.R")
dat = read_rds('D:/GitHub/EnviroTyping/sandbox/shifted_data_analysis/simulated/data/hyb_simulated.rds')

dat = dat[[1]]
#started at 5:55pm on 6/27
crossv = NULL
seeds = c(10388733, 35149853, 93062233, 33227213, 98830153, 15923453, 60741429, 40189643, 35747103, 25172593)
for (j in 1:length(seeds)){
    set.seed(seeds[j])
    results = NULL
    res.time = NULL
    for (i in seq(from = 1, to = 7, length.out = 7)){
        
        tic()
        
        df = naBlockFunc(dat, i/10, "variable")
        
        variance.var <- names(which(map_dbl(df[,6:93], var, na.rm = TRUE) != 0))
        min.vars <- str_subset(variance.var, "Min")
        
        
        runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 1000, nProgress = 500, seed = seeds[j])
        
        calcDists <- calcDissimilarityMatrix(runInfoObj)
        clusObj <- calcOptimalClustering(calcDists)
        
        temp = data.frame(c(i/10, clusObj$nClusters, clusObj$clusterSizes))
        results = data.frame(rbind.fill(results,data.frame(t(c(i/10, clusObj$nClusters, clusObj$clusterSizes)))))
        
        tmptim = toc(quiet = TRUE)
        res.time = rbind(res.time, as.numeric(tmptim[[2]]-tmptim[[1]]))
    }
    #colnames(crossv) = c(1:length(crossv))
    #colnames(results) = c(1:length(results))
    results = cbind(results[,1:2],res.time, results[,3:length(results)])
    crossv = rbind.fill(crossv, results)
}

crossv[,1] = as.factor(crossv[,1])
var.res = t(rbind(tapply(crossv[,2],crossv[,1],mean),tapply(crossv[,3],crossv[,1],mean)))
colnames(var.res) = c('nCLusters','Time')

full.res = NULL
for (i in 1:7){
    tmp.c = tapply(crossv[,2], crossv[,1], rbind)[[i]]
    full.res = rbind(full.res, tmp.c)
}
for (i in 1:7){
    tmp.c = tapply(crossv[,3], crossv[,1], rbind)[[i]]
    full.res = rbind(full.res, tmp.c)
}
full.res = cbind(crossv[1:14,1],full.res)
full.res[1:14,1] = full.res[1:14,1]/10
write_rds(var.res, "varres.rds", compress = "xz")

write_rds(full.res, "fullvarres.rds", compress = "xz")



#####################################################################
#####################################################################
#####################################################################
#####################################################################



pl.dat = read_rds("fullvarres.rds")
pl.dat[11:14,1] = pl.dat[11:14,1]/10
avs = read_rds("varres.rds")
boxplot(pl.dat[9,2:6])

nClus = data.frame(pl.dat[1:7,2:6], row.names = pl.dat[1:7,1])
f = ggplot(nClus, aes(pl.dat[1:7,1], nClus)) +
    geom_dotplot(nClus)





prop = c("10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70")
run = c("run_1","run_1","run_1","run_1","run_1","run_1","run_1","run_2","run_2","run_2","run_2","run_2","run_2","run_2","run_3","run_3","run_3","run_3","run_3","run_3","run_3","run_4","run_4","run_4","run_4","run_4","run_4","run_4","run_5","run_5","run_5","run_5","run_5","run_5","run_5")
clusters = c(9,6,11,5,6,3,2,12,10,6,4,3,5,2,8,8,6,5,16,2,2,13,9,7,4,7,8,4,8,10,9,13,15,24,3)
run_data = data.frame(prop,run)
tmp = data.frame(clusters)
str(run_data)
run.data = data.frame(run_data,tmp)
str(run.data)


avs = data.frame(c(10.0,8.6,7.8,6.2,9.4,8.4,2.6))
colnames(avs) = "nClus"
avsprop = as.numeric(prop[1:7])

ggplot(data = run.data,aes(x = as.numeric(prop),y = clusters, color = factor(run), group = factor(run))) +
    geom_point(size = 3) + geom_line()

ggplot(data = avs, aes(x = avsprop, y = nClus)) + geom_line()




# colnames(results) = c('NAprop','nClusters')
# results = cbind(results[,1:2],res.time, results[,3:length(results)])
# crossv = rbind(crossv, results)


#sim data 2, 10 to 70 percent by 10perc intervals, NA  by variable
#vari.7results = results
write_rds(vari.7results, "vari7results.rds", compress = "xz")


# riskProfObj <- calcAvgRiskAndProfile(clusObj)
# 
# write_rds(clusObj, "../clusObj2.rds", compress = "xz")
# write_rds(riskProfObj, "../riskProfObj2.rds", compress = "xz")
