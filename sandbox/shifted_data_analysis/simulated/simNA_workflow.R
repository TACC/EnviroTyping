
library(PReMiuM)
library(tidyverse)
library(plyr)
library(gtools)
library(tictoc)
library(ggplot2)

source("~/EnviroTyping/sandbox/Simulation/naBlockFunc.R")
dat = read_rds('~/EnviroTyping/sandbox/shifted_data_analysis/simulated/data/hyb_simulated.rds')

dat = dat[[1]]

crossv = NULL

seeds = c(89548009,85000401,54912628,60149388,99302548,22948358,38327743,34766776,59998145,13945098)
for (j in 1:length(seeds)){
    set.seed(seeds[j])
    results = NULL
    res.time = NULL
for (i in seq(from = 1, to = 5, length.out = 5)){

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
full.res = cbind(crossv[1:dim(full.res)[1],1],full.res)
full.res[1:dim(full.res)[1],1] = full.res[1:dim(full.res)[1],1]/10
write_rds(inter.res, "interres.rds", compress = "xz")

write_rds(full.res, "fullintres.rds", compress = "xz")



int.dat = read_rds("fullintres.rds")
int.avs = read_rds("interres.rds")
seeds = c(89548009,85000401,54912628,60149388,99302548,22948358,38327743,34766776,59998145,13945098)


nClus = data.frame(int.dat[1:dim(int.avs)[1],2:(length(seeds)+1)], row.names = int.dat[1:dim(int.avs)[1],1])

clusters = as.vector(unlist(nClus))


#prop = c("10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70")
prop = c(rep(paste0(c((1:dim(int.avs)[1])*10)), length(seeds)))
#just make this character vector of seeds
#run = c("run_1","run_1","run_1","run_1","run_1","run_1","run_1","run_2","run_2","run_2","run_2","run_2","run_2","run_2","run_3","run_3","run_3","run_3","run_3","run_3","run_3","run_4","run_4","run_4","run_4","run_4","run_4","run_4","run_5","run_5","run_5","run_5","run_5","run_5","run_5","run_6","run_6","run_6","run_6","run_6","run_6","run_6","run_7","run_7","run_7","run_7","run_7","run_7","run_7")
#clusters = c(9,6,11,5,6,3,2,12,10,6,4,3,5,2,8,8,6,5,16,2,2,13,9,7,4,7,8,4,8,10,9,13,15,24,3)
run.label =c(paste0(unlist(lapply(seeds, rep,5))))
run_data = data.frame(prop,run.label)
tmp = data.frame(clusters)
str(run_data)
run.data = data.frame(run_data,tmp)
str(run.data)


#avs = data.frame(c(10.0,8.6,7.8,6.2,9.4,8.4,2.6))
avs = data.frame(c(int.avs[,1]))
colnames(avs) = "nClus"
avsprop = as.numeric(prop[1:dim(int.avs)[1]])

avsdf = data.frame(as.character(avsprop),avs)
colnames(avsdf) = c("avsprop", "nClus")
newclus = as.vector(rep(unlist(avs), 10))
avsdf = data.frame(prop,run.label,newclus)



p <- ggplot(data = run.data,aes(x = as.numeric(prop),y = clusters, group = factor(run.label), color = factor(run.label))) +
    geom_point(size = 1, alpha = 1) + geom_line(alpha = .3, size = 1) +  
    geom_point(data = avsdf, aes(x = as.numeric(prop),y = newclus), size = 2, color = '#DC143C') + geom_line(data = avsdf, aes(x = as.numeric(prop),y = newclus), size = 2, color = '#DC143C')

p + labs(title = 'NAs Introduced by Interval', x = "Total NAs (percentage)", y = "Number of Clusters Found", subtitle = "From 10 to 70 percent across 10 random seeds, based on simulated data") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")


write_rds(a, 'intervalsdataset1.rds', compress = "xz")





#####################################################################
#####################################################################
#####################################################################
#####################################################################



source("~/EnviroTyping/sandbox/Simulation/naBlockFunc.R")
dat = read_rds('~/EnviroTyping/sandbox/shifted_data_analysis/simulated/data/hyb_simulated.rds')

dat = dat[[1]]

crossv = NULL

seeds = c(89548009,85000401,54912628,60149388,99302548,22948358,38327743,34766776,59998145,13945098)
for (j in 1:length(seeds)){
    set.seed(seeds[j])
    results = NULL
    res.time = NULL
    for (i in seq(from = 1, to = 7, length.out = 7)){
        
        tic()
        
        df = naBlockFunc(dat, i/10, "variable")
        
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



var.dat = read_rds("fullvarres.rds")
var.avs = read_rds("varres.rds")
seeds = c(89548009,85000401,54912628,60149388,99302548,22948358,38327743,34766776,59998145,13945098)

nClus = data.frame(var.dat[1:dim(var.avs)[1],2:(length(seeds)+1)], row.names = var.dat[1:dim(var.avs)[1],1])

clusters = as.vector(unlist(nClus))


#prop = c("10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70","10","20","30","40","50","60","70")
prop = c(rep(paste0(c((1:dim(var.avs)[1])*10)), length(seeds)))
#just make this character vector of seeds
#run = c("run_1","run_1","run_1","run_1","run_1","run_1","run_1","run_2","run_2","run_2","run_2","run_2","run_2","run_2","run_3","run_3","run_3","run_3","run_3","run_3","run_3","run_4","run_4","run_4","run_4","run_4","run_4","run_4","run_5","run_5","run_5","run_5","run_5","run_5","run_5","run_6","run_6","run_6","run_6","run_6","run_6","run_6","run_7","run_7","run_7","run_7","run_7","run_7","run_7")
#clusters = c(9,6,11,5,6,3,2,12,10,6,4,3,5,2,8,8,6,5,16,2,2,13,9,7,4,7,8,4,8,10,9,13,15,24,3)
run.label =c(paste0(unlist(lapply(seeds, rep,7))))
run_data = data.frame(prop,run.label)
tmp = data.frame(clusters)
str(run_data)
run.data = data.frame(run_data,tmp)
str(run.data)


#avs = data.frame(c(10.0,8.6,7.8,6.2,9.4,8.4,2.6))
avs = data.frame(c(var.avs[,1]))
colnames(avs) = "nClus"
avsprop = as.numeric(prop[1:dim(var.avs)[1]])

avsdf = data.frame(as.character(avsprop),avs)
colnames(avsdf) = c("avsprop", "nClus")
newclus = as.vector(rep(unlist(avs), 10))
avsdf = data.frame(prop,run.label,newclus)


p <- ggplot(data = run.data,aes(x = as.numeric(prop),y = clusters, group = factor(run.label), color = factor(run.label))) +
    geom_point(size = 1, alpha = 1) + geom_line(alpha = .3, size = 1) +  
    geom_point(data = avsdf, aes(x = as.numeric(prop),y = newclus), size = 2, color = '#DC143C') + geom_line(data = avsdf, aes(x = as.numeric(prop),y = newclus), size = 2, color = '#DC143C')

p + labs(title = 'NAs Introduced by Variable', x = "Total NAs (percentage)", y = "Number of Clusters Found", subtitle = "From 10 to 70 percent across 10 random seeds, based on simulated data") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")


tapply(run.data[,3],run.data[,1], range)


# run.data2 = run.data[-c(which(run.data[,2] == 60149388)),]
# run.data2 = run.data2[-c(which(run.data[,2] == 85000401)),]
# avsdf2 = avsdf[-c(which(run.data[,2] == 60149388)),]
# avsdf2 = avsdf2[-c(which(run.data[,2] == 85000401)),]
# prop2 = prop[-c(which(run.data[,2] == 60149388))]
# prop2 = prop2[-c(which(run.data[,2] == 85000401))]
# run.label2 = run.label[-c(which(run.data[,2] == 60149388))]
# run.label2 = run.label2[-c(which(run.data[,2] == 85000401))]
# clusters2 = clusters[-c(which(run.data[,2] == 60149388))]
# clusters2 = clusters2[-c(which(run.data[,2] == 85000401))]
# newclus2 = newclus[-c(which(run.data[,2] == 60149388))]
# newclus2 = newclus2[-c(which(run.data[,2] == 85000401))]
# 
# tapply(run.data2[,3], run.data2[,1], mean)
# avs2 = data.frame(c(tapply(run.data2[,3], run.data2[,1], mean)))
# colnames(avs2) = "nClus"
# avsprop2 = as.numeric(prop[1:dim(var.avs)[1]])
# 
# avsdf2 = data.frame(as.character(avsprop2),avs2)
# colnames(avsdf2) = c("avsprop", "nClus")
# newclus2 = as.vector(rep(unlist(avs2), 8))
# avsdf2 = data.frame(prop2,run.label2,newclus2)
# 
# str(avsdf2)
# 
# ggplot(data = run.data2,aes(x = as.numeric(prop2),y = clusters2, color = factor(run.label2), group = factor(run.label2))) +
#     geom_point(size = 3, alpha = .2) + geom_line(alpha = .2) +
#     geom_point(data = avsdf2, aes(x = as.numeric(prop2),y = newclus2), size = 2, color = 'black') + geom_line(data = avsdf2, aes(x = as.numeric(prop2),y = newclus2), size = 2, color = 'black')

# colnames(results) = c('NAprop','nClusters')
# results = cbind(results[,1:2],res.time, results[,3:length(results)])
# crossv = rbind(crossv, results)


#sim data 2, 10 to 70 percent by 10perc intervals, NA  by variable
#vari.7results = results
#write_rds(vari.7results, "vari7results.rds", compress = "xz")


# riskProfObj <- calcAvgRiskAndProfile(clusObj)
# 
# write_rds(clusObj, "../clusObj2.rds", compress = "xz")
# write_rds(riskProfObj, "../riskProfObj2.rds", compress = "xz")
