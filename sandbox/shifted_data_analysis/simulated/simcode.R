
# Simulate weather variables (which were present in the PCA) from 
# normal distributions with parameters those from the data set. 
# If there is any value outside the range of the variables, then substitute 
# it by the extreme.

rm(list = ls(all = TRUE))
subpath = "/Users/Austin/Documents/SimulationEnv"
setwd(subpath)
fold1 = c("/Media", "/Scratch")
fold2 = c("/Data_2014_Sent18-05-24",
          "/PCA_14_15",
          "/Results_PCA14-15",
          "/Simulation_14-15Dataset")
for (i in 1:length(fold1)){
    dir.create(paste0(subpath,fold1[i]))
    for (j in 1:length(fold2)){
        dir.create(paste0(subpath,fold1[i],fold2[j]))
    }
}
for (i in 1:length(fold1)){
    file.copy(from=paste0(subpath,"/AICs_PvalsPCA14-15_ModellingStartingPC1-9_26Pedis_2-7-18_p90.Rdata"),to=paste0(subpath,fold1[i],fold2[3],"/AICs_PvalsPCA14-15_ModellingStartingPC1-9_26Pedis_2-7-18_p90.Rdata"))
    
    file.copy(from=paste0(subpath,"/dataset_ready2_PCA_14_15_m50p90_22-6-18.RData"),to=paste0(subpath,fold1[i],fold2[2],"/dataset_ready2_PCA_14_15_m50p90_22-6-18.RData"))
    
    file.copy(from=paste0(subpath,"/EigenvectorsMeansSds_PCA14-15AllWeatherVars_p90_n50_24-6-18.RData"),to=paste0(subpath,fold1[i],fold2[3],"/EigenvectorsMeansSds_PCA14-15AllWeatherVars_p90_n50_24-6-18.RData"))
    
    file.copy(from=paste0(subpath,"/PediLabs26_2014_CommonTo45All10StationsIn2015.RData"),to=paste0(subpath,fold1[i],fold2[1],"/PediLabs26_2014_CommonTo45All10StationsIn2015.RData"))
    
    file.copy(from=paste0(subpath,"/SimulDataset1-3_n25p90CoordinFactorialSpace14_15_4-9-18.RData"),to=paste0(subpath,fold1[i],fold2[4],"/SimulDataset1-3_n25p90CoordinFactorialSpace14_15_4-9-18.RData"))
    
    file.copy(from=paste0(subpath,"/TrueParametersMatrix_n26Hyb_p10_2-9-18.RData"),to=paste0(subpath,fold1[i],fold2[4],"/TrueParametersMatrix_n26Hyb_p10_2-9-18.RData"))
}





path1 <- paste0(subpath, "/Media/PCA_14_15/")
path2 <- paste0(subpath, "/Scratch/PCA_14_15/") 
path <- path1
load(paste0(path, 
 "dataset_ready2_PCA_14_15_m50p90_22-6-18.RData"))
dataset.14.15.red.temp <- dataset.14.15.PCAm50Pattern.p90.22.6.18
ntemp <- nrow(dataset.14.15.red.temp)
p <- 90
n.s.st <- 25 # simulated stations
n.replic <- 1 
n <- n.s.st*n.replic  # 25
min.variables.p90 <- apply(dataset.14.15.red.temp, 2, min)
max.variables.p90 <- apply(dataset.14.15.red.temp, 2, max)
#min.variables.p90[1:3]
#      lat        lon  tempMin.1 ...
# 30.54684 -100.74947    1.00000 
means.data90 <- colMeans(dataset.14.15.red.temp)
variances.data90 <- apply(dataset.14.15.red.temp, 2, var)
sds.data90 <- sqrt(variances.data90)
dataset.simulweather.list <- NULL 
class(dataset.simulweather.list) <- c("list")
a <- unique(substr(rownames(dataset.14.15.red.temp), 1, 8))
statlabsimul.n25 <- paste0(a, rep(1:n.replic, n.s.st)) 
# [1] "DEH1.14.1" "GAH1.14.1" ... [25] "TXH1.15.1"
temp <- matrix(NA, nrow=n, ncol=p)
colnames(temp) <- colnames(dataset.14.15.red.temp)
rownames(temp) <- statlabsimul.n25
temp.pattern <- matrix(NA, nrow=n.s.st, ncol=p) # 25 90
colnames(temp.pattern) <- colnames(dataset.14.15.red.temp)
rownames(temp.pattern) <- statlabsimul.n25
no.simulations <- 3
for(i1 in 1:no.simulations)
{
 dataset.simulweather.list[[i1]] <- temp
 # simulated weather data set with a single replicate in each of the 25 stations:
 #dataset.simulweather.list.pattern[[i1]] <- temp.pattern
 seed3 <- 671987827
 seed2 <- 65477611
 seed1 <- 558741269
 if(i1==3) set.seed(seed3)
 if(i1==2) set.seed(seed2)
 if(i1==1) set.seed(seed1)
 for(j in 1:p) 
 {
  a <- rnorm(n.s.st, mean=means.data90[j], sd=sds.data90[j])
  dataset.simulweather.list[[i1]][, j] <- rep(a, each=n.replic)
  # replace values outside range by min or max as it be appropriate
  # if jth variable has any value < min, replace it
  # with its minimum
  logical.m25.temp <- (dataset.simulweather.list[[i1]][, j] < 
   min.variables.p90[j])
  if(sum(logical.m25.temp))
  { # if different from zero necessarily at least one element in logical.m25.temp
    # different from zero too #
   dataset.simulweather.list[[i1]][logical.m25.temp, j] <- 
    min.variables.p90[j]
  }
  logical.m25.temp2 <- (dataset.simulweather.list[[i1]][, j] > 
   max.variables.p90[j])
  if(sum(logical.m25.temp2))
  {
   dataset.simulweather.list[[i1]][logical.m25.temp2, j] <- 
    max.variables.p90[j]  #cat("j = ", j, "\n")
  } # end if
 } # end jth weather variable
} # end i1-th simulation
# Example:
i1 <- 1 # i1th simulated weather data set; i1=1, 2, 3 #
dataset.simulweather.list[[i1]][1:6, 1:5]
#                lat       lon tempMin.1 tempMin.2 tempMin.3 i1=3
# DEH1.14.1 41.04913 -83.37721  1.407994  9.055444 10.238851
# GAH1.14.1 36.33318 -88.57590  6.241640  8.930316 12.145730 ...
#                lat       lon tempMin.1 tempMin.2 tempMin.3  i1=2
# DEH1.14.1 44.02293 -92.56334  4.190271  8.933133  8.669170
# GAH1.14.1 42.19005 -96.03073 11.651600  8.631356  7.783341 ...
#                lat       lon tempMin.1 tempMin.2 tempMin.3 i1=1
# DEH1.14.1 35.33068 -96.55195  4.579920  8.798100 10.551533
# GAH1.14.1 34.65831 -91.75692  5.747223  6.287599  8.184187 ...
# dataset.weather.n25.4.9.18 <- dataset.simulweather.list[[i1]]
path1 <- paste0(subpath, "/Media/Simulation_14-15Dataset/")
path2 <- paste0(subpath, "/Scratch/Simulation_14-15Dataset/")
path <- path1
save(seed1, seed2, seed3,
 dataset.simulweather.list,
 n.s.st, n, p, n.replic, no.simulations, 
 min.variables.p90, max.variables.p90, 
 file=paste0(path, "SimulatedWeather1-3_n25_4-9-18.RData"))

## Coordinates in the factorial space from the PCA of the 25 simulated
## stations above.
# Read in the eigenvectors, get these coordinates,
# read in the model fit results, pick the betas from there as the truth, and finally 
# simulate yield.
path1 <- paste0(subpath, "/Media/Results_PCA14-15/")
path2 <- paste0(subpath, "/Scratch/Results_PCA14-15/")
path <- path1
load(paste0(path, # eigenvectors.mat.p90.PCAcorrel
  "EigenvectorsMeansSds_PCA14-15AllWeatherVars_p90_n50_24-6-18.RData"))
eigenve.14.15.p90 <- eigenvectors.mat.p90.PCAcorrel # 90 23
eigenve.14.15.p90[1:3, 1:3]
#                   PC1         PC2
# lat        0.05718054 -0.04672100
# lon       -0.01475096 -0.02557768
# tempMin.1 -0.02626378  0.09858152
varlab90 <- rownames(eigenve.14.15.p90) # "lat" "lon" "tempMin.1" "tempMin.2" ...
vars.means.mat <- matrix(rep(vars.means, 
 each=n.s.st), ncol=p) # 25 90
colnames(vars.means.mat) <- names(vars.means)
vars.sds.mat <- matrix(rep(vars.sds, 
  each=n.s.st), ncol=p) # 25 90
colnames(vars.sds.mat) <- names(vars.sds)
rownames(vars.sds.mat) <- 
 rownames(dataset.simulweather.list[[1]]) # "DEH1.14.1" "GAH1.14.1" "IAH2.14.1" ...
vars.means.mat[c(1, 10, 25), 1:4]
vars.sds.mat[c(1, 10, 25), 1:4]
#                lat     lon tempMin.1 tempMin.2
# DEH1.14.1 4.101309 7.00727  4.190763  2.660091
# NEH1.14.1 4.101309 7.00727  4.190763  2.660091 ...
n.pcs <- 23 # =ncol(eigenve.14.15.p90)
# test:
(dataset.simulweather.list[[1]][c(1, 10, 25), 1:4]-
 vars.means.mat[c(1, 10, 25), 1:4]) /
 vars.sds.mat[c(1, 10, 25), 1:4]

coordin.newobs.simul.m25.p23.pca14.15.list <- NULL
class(coordin.newobs.simul.m25.p23.pca14.15.list) <- c("list")

for(i1 in 1:no.simulations)
{
 # COORDINATES OF THE n.s.st=25 REPLICATES FROM THE NEW
 # SIMULATED STATIONS IN FACTORIAL SPACE FROM 14-15 PCA for the i1-th simulation
 # == EIGENVECTORS*ROWS OF
 # new.simul.stations.standardised.m25.p90.pca14.15
 datasetweather.i.temp <- dataset.simulweather.list[[i1]] #dataset.weather.n25.4.9.18
 labels.simul.m50 <- rownames(datasetweather.i.temp)
 # all.equal(colnames(datasetweather.i.temp), names(vars.means)) # TRUE
 new.simul.stations.standardised.m25.p90.pca14.15 <- 
  (dataset.simulweather.list[[i1]]-
    vars.means.mat)/vars.sds.mat 
 #dim(new.simul.stations.standardised.m25.p90.pca14.15) # 25 90
 # one "simulated hybrid" here
 rownames(new.simul.stations.standardised.m25.p90.pca14.15) <-
  rownames(dataset.simulweather.list[[i1]])
 new.simul.stations.standardised.m25.p90.pca14.15[c(1, 10, 25), 1:4]
 #                 lat       lon  tempMin.1    tempMin.2 i1=3
 # DEH1.14.1 0.4519082 0.7772866 -1.4803999 -0.009080997
 # NEH1.14.1 0.3780794 0.3231800 -0.2438437  1.861844912
 # TXH1.15.1 1.1229246 0.1373068 -1.3622263  0.420184366
 #                  lat        lon  tempMin.1   tempMin.2 i1=2
 # DEH1.14.1  1.1769947 -0.5336563 -0.8164930 -0.05506085
 # NEH1.14.1 -0.1643195 -0.4891792 -1.2542294 -0.59795319
 # TXH1.15.1 -0.3515784  1.7046115 -0.3683991 -1.38617980
 #                  lat        lon  tempMin.1  tempMin.2 i1=1
 # DEH1.14.1 -0.9423901 -1.1028665 -0.7235150 -0.1058236
 # NEH1.14.1 -1.2753836 -0.1688790  0.3807964  0.2244476
 # TXH1.15.1 -1.7512735 -0.1914467 -0.1861366 -0.5890927
 # ##
 coordin.newobs.simul.m25.p23.pca14.15.mat <- matrix(
  NA, nrow=n.s.st, ncol=n.pcs)
 colnames(coordin.newobs.simul.m25.p23.pca14.15.mat) <- 
  paste0("pc", 1:n.pcs)
 rownames(coordin.newobs.simul.m25.p23.pca14.15.mat) <-
  rownames(new.simul.stations.standardised.m25.p90.pca14.15)
 for(i in 1:n.pcs) # ith eigenvector
 {
  for(k in 1:n.s.st) # kth new observation 
   coordin.newobs.simul.m25.p23.pca14.15.mat[k, i] <- 
    sum(eigenve.14.15.p90[, i]*
     new.simul.stations.standardised.m25.p90.pca14.15[k, ])
 } # end for i; dim= 25 23
 
 coordin.newobs.simul.m25.p23.pca14.15.list[[i1]] <- 
  coordin.newobs.simul.m25.p23.pca14.15.mat
} # end for the i1-th simulation

# Example:
coordin.newobs.simul.m25.p23.pca14.15.mat[1:5, 1:10]
# i1=3             pc1         pc2        pc3        pc4         pc5       pc6       pc7         pc8
# DEH1.14.1 -0.7111760 -0.05852871  0.8607027  1.0923007 -1.20473440 1.1028074  0.740512  1.60644478
# GAH1.14.1  0.2285559  0.17590470  0.4415041  0.6587912 -0.04722274 0.9112544 -2.548384 -1.01092537
# IAH2.14.1 -1.5947182 -1.05562638 -0.7298831 -0.1584551  1.38671536 0.2744598 -0.927320 -0.05963768
# IAH3.14.1 -0.9751180  1.51989547  0.1368166 -0.2042782  1.04202855 1.3415328  0.458035 -1.22188082
# IAH4.14.1 -0.4801191  1.39225214  0.3829322  1.3470283  0.30317523 1.0125280 -1.476978  0.32720287
# i1=2             pc1       pc2        pc3        pc4         pc5        pc6        pc7
# DEH1.14.1 -0.7666414 0.1849786 -2.1085604 -0.3687502  1.32373592  1.5800751  0.5878928
# GAH1.14.1 -1.3817038 1.0464757  0.9541587  1.2326460 -0.61956479  0.3537497  0.9194408
# IAH2.14.1  0.2884749 0.8877416  0.9410600 -0.3090897  0.03147429 -0.7136006 -0.6357890
# IAH3.14.1  0.3077544 0.7815026  0.1719193  0.4719314  0.52782083 -0.4889243  0.2317708
# IAH4.14.1 -0.4615345 0.3140661 -0.3172026 -1.6031456 -0.55633532  0.3218754  0.1793646
# i1=1             pc1         pc2        pc3        pc4        pc5         pc6        pc7
# DEH1.14.1 -0.3467696  1.14773495  0.2783825  0.6483262 -0.1042338  0.28931051 -0.3292073
# GAH1.14.1 -0.3472328  0.55301006 -0.9809422  0.5768254 -1.8892446  0.10377982 -1.3038280
# IAH2.14.1 -0.1920295 -0.88357098  1.6013617 -0.1782688 -0.5555175  1.76950347 -1.0695048
path1 <- paste0(subpath, "/Media/Simulation_14-15Dataset/")
path2 <- paste0(subpath, "/Scratch/Simulation_14-15Dataset/")
path <- path1
save(coordin.newobs.simul.m25.p23.pca14.15.list,
 dataset.simulweather.list, #new.simul.stations.standardised.m25.p90.pca14.15,
 file=paste0(path,
 "SimulDataset1-3_n25p90CoordinFactorialSpace14_15_4-9-18.RData"))

# ##
## Read in true parameters from the models and get the simulated yield.
# Label the 26 simulated hybrids as the ones
# before
# ##
path1 <- paste0(subpath, "/Media/Data_2014_Sent18-05-24/")
path2 <- paste0(subpath, "/Scratch/Data_2014_Sent18-05-24/")
path <- path1
load(   #pedis26.In2014CommonTo45All10StationsIn2015, 
 paste0(path, 
  "PediLabs26_2014_CommonTo45All10StationsIn2015.RData"))
pedilabs26 <- pedis26.In2014CommonTo45All10StationsIn2015
# Read in models fitted with the MLEs, etc.
path1 <- paste0(subpath, "/Media/Results_PCA14-15/")
path2 <- paste0(subpath, "/Scratch/Results_PCA14-15/")
path <- path1
load(paste0(path,
  "AICs_PvalsPCA14-15_ModellingStartingPC1-9_26Pedis_2-7-18_p90.RData"))
# list of length = 26 with the results of the fittings.
all.equal(pedilabs26, rownames(pcs.present.in.models)) 
strReverse <- function(x)
 sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
n.s.st <- 25 # sample size in data set with a single replicate per station
# Example, i=1st hybrid # ##
i <- 1 # ith simulated hybrid
pedi.i <- pedilabs26[i] # B14A/H95
parameters.taken.as.truth.i.temp <- 
  Fits.26Pedis14.15.ModelsFoundByStepPCs[[i]]$coefficients[, 1]
# Expected values of the observations i.e. E(Y): we need the design
# matrix, i.e. the values of the PCs of the new observations, and
# beta above = parameters.taken.as.truth.i.temp. 
# PC numbers of those present in the model for the ith hybrid
pcs.model.i.labels.temp <- substr(names(parameters.taken.as.truth.i.temp)[-1], 
 start=8, stop=10) # "pc1" "pc5" "pc7" "pc8" "pc9" 
coordin.newobs.simul.m25.i.pca14.15.mat <- 
 coordin.newobs.simul.m25.p23.pca14.15.mat[, pcs.model.i.labels.temp] # 25 x 5
design.matrix.newobs.simul.m25.i.pca14.15 <- 
  coordin.newobs.simul.m25.i.pca14.15.mat # it is a matrix already
design.matrix.newobs.simul.m25.i.pca14.15[1:3, ]
# i1=3             pc1         pc5       pc7         pc8        pc9
# DEH1.14.1 -0.7111760 -1.20473440  0.740512  1.60644478 -0.9141627
# GAH1.14.1  0.2285559 -0.04722274 -2.548384 -1.01092537  1.3575595
# IAH2.14.1 -1.5947182  1.38671536 -0.927320 -0.05963768  0.3473050
# i1=2             pc1         pc5        pc7        pc8        pc9
# DEH1.14.1 -0.7666414  1.32373592  0.5878928 -1.2196098 1.17873591
# GAH1.14.1 -1.3817038 -0.61956479  0.9194408 -0.8144901 0.04394901
# i1=1             pc1        pc5        pc7         pc8        pc9
# DEH1.14.1 -0.3467696 -0.1042338 -0.3292073  1.60329332  0.3201720
# GAH1.14.1 -0.3472328 -1.8892446 -1.3038280 -0.02727596 -0.7189695
# IAH2.14.1 -0.1920295 -0.5555175 -1.0695048 -0.28132811  1.4934194
n.param.i.temp <- 
 ncol(design.matrix.newobs.simul.m25.i.pca14.15) # 5 without including intercept
design.matrix.newobs.simul.m25.i.pca14.15 <- 
 design.matrix.newobs.simul.m25.i.pca14.15 %x% rep(1, n.replic) # Kronecker product
dim(design.matrix.newobs.simul.m25.i.pca14.15) # 25 5
i1 <- 1
rownames(design.matrix.newobs.simul.m25.i.pca14.15) <- 
 rownames(dataset.simulweather.list[[i1]])
#colnames(design.matrix.newobs.simul.m25.i.pca14.15) <- 
# colnames(coordin.newobs.simul.m25.p23.pca14.15.list[[i1]])
# End of example for the ith hybrid # ##
# ######################################################################### #
# The sets of PCs present in the models by hybrid differ. 
# Parameter vectors with length=9 and zeros in absent parameters.
# Column with the (simulated) hybrid label and later we will 
# another one for yield. n=25
# ####################################################################### #
# Matrix with all the true parameters
n.ped26 <- 26
parameters.taken.as.truth.mat.n26 <- matrix(0, nrow=n.ped26,
 ncol=10)
colnames(parameters.taken.as.truth.mat.n26) <- c("Int",
 paste0("pc", 1:9))
rownames(parameters.taken.as.truth.mat.n26) <- 1:n.ped26
# ith hybrid
n.s.obs <- n.replic*n.s.st*n.ped26 # 650
#pedi.labs.m650 <- rep(NA, n.s.obs)
sigma.n26 <- rep(NA, n.ped26)
names(sigma.n26) <- 1:n.ped26
# complete this matrix with the 'true' parameters for all
# the 26 hybrids:
for(i in 1:n.ped26) 
{
 # fill in the ith row of parameters.taken.as.truth.mat.n26 with
 # the "true" parameters from the ith hybrid
 # using strReverse function defined above, n.replic, etc.
 # Create skeleton of the simulated data set, with the values of
 # the explanatory variables = coordinates of the PCs
 pedi.i <- pedilabs26[i] # B14A/OH43
 rownames(parameters.taken.as.truth.mat.n26)[i] <-
  pedi.i
 names(sigma.n26)[i] <- pedi.i
 parameters.taken.as.truth.i.temp <- 
  Fits.26Pedis14.15.ModelsFoundByStepPCs[[i]]$coefficients[, 1]
 pcs.model.i.labels.temp <- substr(names(parameters.taken.as.truth.i.temp)[-1], 
  start=8, stop=10)
 reverse.label.temp <- 
  strReverse(names(parameters.taken.as.truth.i.temp))
 pc.numbers.temp <- as.numeric(substr(
  reverse.label.temp[-1], start=1, stop=1)) # 1 5 7 8 9
 sigma.truth.i.temp <- 
  Fits.26Pedis14.15.ModelsFoundByStepPCs[[i]]$sigma # 26.2238
 # coordinates of the new observations 
 design.matrix.newobs.simul.m25.i.pca14.15 <- 
  coordin.newobs.simul.m25.p23.pca14.15.mat[, 
   pcs.model.i.labels.temp]
 n.param.i.temp <- 
  ncol(design.matrix.newobs.simul.m25.i.pca14.15) 
 designmatrix.replicates.i.temp <-
  design.matrix.newobs.simul.m25.i.pca14.15 %x% 
  rep(1, n.replic)
 rownames(designmatrix.replicates.i.temp) <- 
  rownames(design.matrix.newobs.simul.m25.i.pca14.15)
 colnames(designmatrix.replicates.i.temp) <- 
  colnames(design.matrix.newobs.simul.m25.i.pca14.15)
 designmatrix.replicates.i.temp[1:2, 1:3]
 #                  pc1        pc5        pc7
 # DEH1.14.1 -0.3467696 -0.1042338 -0.3292073
 # GAH1.14.1 -0.3472328 -1.8892446 -1.3038280
 # AUGMENT THE VECTORS WITH ZEROS
 # IN THE POSITIONS OF THE ABSENT PARAMETERS
 parameters.taken.as.truth.mat.n26[i, 1] <- 
  parameters.taken.as.truth.i.temp[1] # 1 5 7 8 9
 parameters.taken.as.truth.mat.n26[i, pc.numbers.temp+1] <-
  parameters.taken.as.truth.i.temp[-1]
 sigma.n26[i] <- Fits.26Pedis14.15.ModelsFoundByStepPCs[[i]]$sigma
} # end for ith pedi

# Example:
parameters.taken.as.truth.mat.n26[1:3, ]
#                Int      pc1       pc2       pc3       pc4       pc5
# B14A/H95  135.1326 1.191715  0.000000  0.000000  0.000000 -7.860861
# B14A/OH43 123.1277 0.000000 -5.310215 -3.403208 -6.228156 -5.422406
# B37/C103  116.5044 0.000000  0.000000  0.000000  0.000000 -6.897222
#                pc6       pc7       pc8       pc9
# B14A/H95  0.000000 -4.038158 -3.481349 -3.580952
# B14A/OH43 0.000000  0.000000 -8.116765  0.000000
# B37/C103  8.072153 -6.561759  0.000000  0.000000
path # ProjMeeting3_b_WithData2014_AllPCAMixEtc/Simulation_14-15Dataset/
save(parameters.taken.as.truth.mat.n26,
 sigma.n26,
 file=paste0(path,
 "TrueParametersMatrix_n26Hyb_p10_2-9-18.RData"))

# NEXT STEP: CREATE A DATA FRAME
# WITH n.s.obs=650 ROWS. REPEAT THE VALUES OF THE EXPLANATORY VARIABLES
# ACROSS (from design.matrix.newobs.simul.m25.pca14.15.p9)
# 26 MORE TIMES, ONE FOR EACH HYBRID. ADDITIONALLY, REPEAT EACH
# HYBRID LABEL n.replic*25 TIMES AND PUT ALL THEM IN A VECTOR. FINALLY,
# SIMULATE YIELD PICKING
# 91 columns for simulated weather values plus yield
# need to read in pedilabs26 and 
# simulated weather variables above 
# The values of the simulated weather variables coincide for all
# the hybrids in the same simulated station. Assume all the hybrids
# are defined in the 25 simulated stations.
#rm(list = ls(all = TRUE)) 
rm(list=(ls()[ls()!="subpath"]))
path1 <- paste0(subpath, "/Media/Data_2014_Sent18-05-24/")
path2 <- paste0(subpath, "/Scratch/Data_2014_Sent18-05-24/")
path <- path1
load(   #pedis26.In2014CommonTo45All10StationsIn2015, 
 paste0(path, 
  "PediLabs26_2014_CommonTo45All10StationsIn2015.RData"))
path1 <- paste0(subpath, "/Media/Simulation_14-15Dataset/")
path2 <- paste0(subpath, "/Scratch/Simulation_14-15Dataset/")
path <- path1
load(paste0(path, 
 "SimulDataset1-3_n25p90CoordinFactorialSpace14_15_4-9-18.RData"))
# the simulated weather variables without standardising are in
# dataset.simulweather.list.pattern[[i1]] (which is == dataset.simulweather.list above)
i1 <- 1
dataset.simulweather.list[[i1]][1:2, 1:3]
# i1=3           lat       lon tempMin.1
# DEH1.14.1 41.04913 -83.37721  1.407994
# GAH1.14.1 36.33318 -88.57590  6.241640
# i1=2           lat       lon tempMin.1
# DEH1.14.1 44.02293 -92.56334  4.190271
# GAH1.14.1 42.19005 -96.03073 11.651600
# i1=1           lat       lon tempMin.1
# DEH1.14.1 35.33068 -96.55195  4.579920
# GAH1.14.1 34.65831 -91.75692  5.747223
# need to row bind
# the whole data set with itself 26 times, one for
# each pedi.
pedilabs26 <- pedis26.In2014CommonTo45All10StationsIn2015
n.replic <- 1 
n.sim.stat <- 25
n.sim.ped26 <- 26
n <- n.replic*n.sim.stat*n.sim.ped26 # 650
n.wea.var <- 90
n.pcs <- 9
no.simulations <- 3
rownames(coordin.newobs.simul.m25.p23.pca14.15.list[[1]]) # "DEH1.14.1" "GAH1.14.1" ...
replic.lab.n650 <- rep(
 rownames(coordin.newobs.simul.m25.p23.pca14.15.list[[1]]),
 n.sim.ped26) 
# hybrid labels per observation
# i.e. sequences of 25 repeated hybrid labels
pedi.sim.n650 <- rep(pedilabs26, each=n.replic*n.sim.stat) 
simul.d1.categ.n650 <- data.frame(cbind(pedi.sim.n650,
  replic.lab.n650))
colnames(simul.d1.categ.n650) <- c("Pedi", "StYRe") 
# station-year-replicate
simul.d1.categ.n650[c(1:2, (nrow(simul.d1.categ.n650)-1):nrow(simul.d1.categ.n650)), ]
#         Pedi     StYRe
# 1   B14A/H95 DEH1.14.1
# 2   B14A/H95 GAH1.14.1 ...
# 649  WF9/H95 ONH1.15.1
# 650  WF9/H95 TXH1.15.1
# summary: simul.d1.categ.n650 with
# the hybrid and station-year-repl labels
# add the (simulated) weather variables
# replicate labels for this hybrid at the moment
n2 <- n.sim.stat*n.replic
simul.dataset.n650.frame.list <- NULL
class(simul.dataset.n650.frame.list) <- c("list")
for(i1 in 1:no.simulations)
{
 simul.dataset.n650.temp <- matrix(NA, nrow=n,
  ncol=n.wea.var) 
 colnames(simul.dataset.n650.temp) <-  
   colnames(dataset.simulweather.list[[i1]])
 simul.dataset.n650.temp[1:n.sim.stat, ] <- 
  dataset.simulweather.list[[i1]] %x%
   rep(1, n.replic)
 for(i in 2:n.sim.ped26) # ith pedi
 { 
  # need to row bind dataset.temp 26 times
  # i=2: from row n2*(i-1)+1 to n2*i
  simul.dataset.n650.temp[(n2*(i-1)+1):(n2*i), ] <- 
   dataset.simulweather.list[[i1]]
 } # end for ith pedi
 simul.dataset.n650.frame.list[[i1]] <- 
  cbind(simul.d1.categ.n650, simul.dataset.n650.temp) 
}

simul.dataset.n650.frame.list[[i1]][c(1, 25:27, n), 1:3] 
simul.dataset.n650.frame.list[[i1]][1:5, 1:4]
# i1=3  Pedi     StYRe      lat       lon
# 1 B14A/H95 DEH1.14.1 41.04913 -83.37721
# 2 B14A/H95 GAH1.14.1 36.33318 -88.57590
# 3 B14A/H95 IAH2.14.1 44.07085 -91.16290
# i1=2  Pedi     StYRe      lat       lon
# 1 B14A/H95 DEH1.14.1 44.02293 -92.56334
# 2 B14A/H95 GAH1.14.1 42.19005 -96.03073 ...
#       Pedi     StYRe      lat       lon
# 1 B14A/H95 DEH1.14.1 35.33068 -96.55195
# 2 B14A/H95 GAH1.14.1 34.65831 -91.75692
# 3 B14A/H95 IAH2.14.1 35.62113 -92.44338
# 4 B14A/H95 IAH3.14.1 36.61757 -95.36402
# 5 B14A/H95 IAH4.14.1 32.27722 -92.08616 # good, done
save(simul.dataset.n650.frame.list, simul.d1.categ.n650,
 coordin.newobs.simul.m25.p23.pca14.15.list, # coordinates of the below in PC space
 dataset.simulweather.list, # simulated weather variables
 n.replic, n, no.simulations, n.wea.var, 
 n.sim.stat, n.sim.ped26, pedilabs26,
 file=paste0(path, 
 "SimulDataset1-3_n650Frame_19-9-18_1replic.RData"))
# summary: we have to generate yield
# and add it to this data set. 

# Simulate yield (25 stations, 26 hybrids, 1 replicate per station
# per hybrid):
# Read in the coordinates of the simulated weather
# variables in the factorial space plus the true parameters
dim(coordin.newobs.simul.m25.p23.pca14.15.list[[1]]) # 25 23
# true parameters  
path1 <- paste0(subpath, "/Media/Simulation_14-15Dataset/")
path2 <- paste0(subpath, "/Scratch/Simulation_14-15Dataset/")
path <- path1
load(paste0(path, 
 "TrueParametersMatrix_n26Hyb_p10_2-9-18.RData"))
# parameters.taken.as.truth.mat.n26, sigma.n26
# Expectation of Y_i = X*beta
# and a random error with variance=sigma.n26[i]^2.
# (the design matrix X is the same for all the hybrids because 
# all of them have 1 replicate per station and appear in 
# the same stations.

Y.yield.i1.n650.vec.list <- NULL
class(Y.yield.i1.n650.vec.list) <- c("list")

for(i1 in 1:no.simulations)
{ # true parameters are the same for all i1
 # Set Xtemp_i1,
 # parameters.taken.as.truth.mat.n26[i, , drop=FALSE],
 # seed for each i1, epsilon_i_i1
 # using n.replic, coord.new..., n.sim.ped26,
 # parameters.taken.as.truth..., sigma.n26, n2=25,
 # pedilabs26
 Xtemp <- coordin.newobs.simul.m25.p23.pca14.15.list[[i1]][, 1:9] %x%
  rep(1, n.replic)
 colnames(Xtemp) <- colnames(
  coordin.newobs.simul.m25.p23.pca14.15.list[[i1]])[1:9]
 rownames(Xtemp) <- rownames(coordin.newobs.simul.m25.p23.pca14.15.list[[i1]])
 Xtemp <- cbind(rep(1, n2), Xtemp)
 Xtemp[1:6, c(1:3, 10)] # 25 x 10
 #           Int        pc1         pc2         pc9 i1=3
 # DEH1.14.1   1 -0.7111760 -0.05852871 -0.91416265
 # GAH1.14.1   1  0.2285559  0.17590470  1.35755948
 #           Int        pc1         pc2         pc9
 # DEH1.14.1   1 -0.7666414  0.18497860  1.17873591
 # GAH1.14.1   1 -1.3817038  1.04647573  0.04394901 ...
 # i1=1      Int        pc1         pc2        pc9
 # DEH1.14.1   1 -0.3467696  1.14773495  0.3201720
 # GAH1.14.1   1 -0.3472328  0.55301006 -0.7189695
 seed3 <- 11765
 seed2 <- 712320
 seed1 <- 558469
 if(i1==1) seed.temp <- seed1
 if(i1==2) seed.temp <- seed2
 if(i1==3) seed.temp <- seed3
 Y.yield.i1.temp.mat <- matrix(NA,
  nrow=n2, ncol=n.sim.ped26)
 colnames(Y.yield.i1.temp.mat) <- pedilabs26
 rownames(Y.yield.i1.temp.mat) <-
  rownames(Xtemp)
 for(i in 1:n.sim.ped26) # i <- 1
 {
  if(i==1) set.seed(seed.temp)
  pedi.sim.i <- pedilabs26[i] # B14A/H95
  param.taken.as.truth.i.temp <-
   parameters.taken.as.truth.mat.n26[i, , drop=FALSE] # 6 including int. here
  E.Y.temp.ithhybrid <- Xtemp %*% t(param.taken.as.truth.i.temp)
  epsilon.ithhybrid.temp.n25 <- rnorm(n2, 
   mean=E.Y.temp.ithhybrid, sd=sigma.n26[i]) 
  # check
  check.negative.yield.temp <-  
   (E.Y.temp.ithhybrid+epsilon.ithhybrid.temp.n25)<0
  iteration.temp <- 1
  while(sum(check.negative.yield.temp))
  {
   cat("i1=", i1, ", iteration ", iteration.temp, "\n")
   ntemp <- sum(check.negative.yield.temp)
   epsilon.temp <- rnorm(ntemp, 
    mean=E.Y.temp.ithhybrid, sd=sigma.n26[i])
   epsilon.ithhybrid.temp.n25[check.negative.yield.temp] <-
     epsilon.temp
   check.negative.yield.temp <- # update
    (E.Y.temp.ithhybrid+epsilon.ithhybrid.temp.n25)<0
   iteration.temp <- iteration.temp + 1
  } # end check negative yield #
  Y.yield.i1.temp.mat[, i] <- E.Y.temp.ithhybrid + epsilon.ithhybrid.temp.n25
 } # end for ith hybrid
 Y.yield.i1.n650.vec.list[[i1]] <- as.vector(Y.yield.i1.temp.mat)
 names(Y.yield.i1.n650.vec.list[[i1]]) <- simul.dataset.n650.frame.list[[1]]$Pedi
} # end for i1-th simulation

# rbind(Y.yield.i1.n650.vec.list[[1]][1:5],
# Y.yield.i1.n650.vec.list[[2]][1:5],
# Y.yield.i1.n650.vec.list[[3]][1:5])
#      B14A/H95 B14A/H95 B14A/H95 B14A/H95 B14A/H95
# [1,] 288.7769 277.8768 294.9048 270.4862 272.6799 ...
# [2,] 254.6857 282.3477 302.8399 231.2668 327.7499 ...
# [3,] 303.0052 249.6948 236.9799 295.3954 223.9013 ...

simul.dataset.n650.p93.frame.list <- NULL
class(simul.dataset.n650.p93.frame.list) <- c("list")
for(i1 in 1:no.simulations)
{
 simul.dataset.n650.p93.frame.list[[i1]] <-
  cbind(simul.dataset.n650.frame.list[[i1]][, c(1, 2)],
   Y.yield.i1.n650.vec.list[[i1]],
   simul.dataset.n650.frame.list[[i1]][, 3:92])
 colnames(simul.dataset.n650.p93.frame.list[[i1]])[3] <-
  c("Yield")
}
rbind(simul.dataset.n650.p93.frame.list[[1]][1:2, 1:5],
 simul.dataset.n650.p93.frame.list[[2]][1:2, 1:5],
 simul.dataset.n650.p93.frame.list[[3]][1:2, 1:5])
#       Pedi     StYRe    Yield      lat       lon
# 1 B14A/H95 DEH1.14.1 288.7769 35.33068 -96.55195
# 2 B14A/H95 GAH1.14.1 277.8768 34.65831 -91.75692
# 3 B14A/H95 DEH1.14.1 254.6857 44.02293 -92.56334
# 4 B14A/H95 GAH1.14.1 282.3477 42.19005 -96.03073
# 5 B14A/H95 DEH1.14.1 303.0052 41.04913 -83.37721
# 6 B14A/H95 GAH1.14.1 249.6948 36.33318 -88.57590
path1 <- paste0(subpath, "/Media/Simulation_14-15Dataset/")
path2 <- paste0(subpath, "/Scratch/Simulation_14-15Dataset/")
path <- path1
save(simul.dataset.n650.p93.frame.list, 
 no.simulations, n.sim.ped26, n2, n, 
 n.wea.var, n.replic, n.pcs, n.sim.stat,
 seed1, seed2, seed3, pedilabs26, sigma.n26,
 coordin.newobs.simul.m25.p23.pca14.15.list,
 simul.d1.categ.n650, simul.dataset.n650.frame.list, 
 parameters.taken.as.truth.mat.n26, 
 file=paste0(path, 
 "SimulDataset1-3_n650p93WithYield_19-9-18.RData"))
# END # ############################################################# #
# ################################################################### #
# ################################################################### #
# ################################################################### #





