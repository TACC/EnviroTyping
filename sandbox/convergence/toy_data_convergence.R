library(PReMiuM)
library(tidyverse)
library(coda)
setwd("~/GitHub/EnviroTyping/sandbox/convergence/output")

toy.data <- read_rds("../../../data/interim/G2F/toy_data.rds")

inputs <- generateSampleDataFile(clusSummaryBernoulliDiscrete())
runInfoObj <- profRegr(yModel = inputs$yModel, xModel = inputs$xModel, nSweeps = 10000, nBurn = 10000, data = inputs$inputData, output = "output", covNames = inputs$covNames,fixedEffectsNames = inputs$fixedEffectNames, reportBurnIn = TRUE)

globalParsTrace(runInfoObj, parameters = "nClusters", plotBurnIn = TRUE, whichBeta = 2)

betaChain <- mcmc(read.table("output_nClusters.txt")[, 1])
autocorr.plot(betaChain)


inputs <- generateSampleDataFile(clusSummaryBernoulliDiscrete())
nClusInit <- c(10, 20, 50, 75)
for (i in 1:length(nClusInit)) {
    runInfoObj <- profRegr(yModel = inputs$yModel, xModel = inputs$xModel,nSweeps = 10000, nBurn = 10000, data = inputs$inputData,output = paste("init", nClusInit[i], sep = ""),covNames = inputs$covNames, alpha = 1,fixedEffectsNames = inputs$fixedEffectNames,nClusInit = nClusInit[i])
    margModelPosterior(runInfoObj)
    print(i)
}

mmp <- list()
for (i in 1:length(nClusInit)) {
    mmp[[i]] <- read.table(paste("init", nClusInit[i], "_margModPost.txt", sep = ""))[, 1]
    }
plot(c(head(nClusInit, n = 1) - 0.5, tail(nClusInit, n = 1) + 0.5),c(min(unlist(mmp)), max(unlist(mmp))), type = "n",ylab = "Log marginal model posterior",xlab = "Initial number of clusters", cex.lab = 1.3, xaxt = "n") 
axis(1, at = nClusInit, labels = nClusInit)

for (i in 1:length(nClusInit)) {
    boxplot(mmp[[i]], add = TRUE, at = nClusInit[i], pch = ".",boxwex = 5, col = "lightblue")
}

set.seed(1234)
runInfoObj <- profRegr(covNames, yModel = "Normal", xModel = "Mixed", nSweeps = 28000, nBurn = 400, data = toy.data, outcome = 'Yield',discreteCovs = "Pedi", continuousCovs = names(toy.data)[3:10], output = "output", seed = 1234, reportBurnIn = TRUE)
# margModelPosterior(runInfoObj)

globalParsTrace(runInfoObj, parameters = "alpha", plotBurnIn = TRUE)
chain <- mcmc(read.table("output_alpha.txt")[, 1])
autocorr.plot(chain, lag.max = 500)
str(chain)
effectiveSize(chain)
raftery.diag(chain)


old.job <- read_rds("../../../data/interim/G2F_Hybrid/month_45_81covs/runInfoObj.rds")
globalParsTrace(old.job, parameters = "nClusters")

