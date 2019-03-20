To get a better understanding of how PReMiuM handles real data a small dataset is extracted from the G2F data.

Load the libraries to manipulate data, run PReMiuM, and tictoc, which is a simple package to replace the use of _proc.time_ for checking runtime on procedures..
```r
library(tidyverse)
library(PReMiuM)
library(tictoc)
```

The PReMiuM _profRegr_ function returns a lot of output.  Creating a separate directory for the output can help when recovering files later.
```r
setwd("~/GitHub/EnviroTyping/sandbox/interim_datasets/2015/")
dir.create("output")
setwd("./output")
```

Next, import a dataset.  This one in particular has 5 different hybrids (2 replicates of each) found in 5 locations for a total of 50 observations.  The columns are the variables which comprise of the hybrid label, the yield in bushels per acre collected at the end of the season, and weather variables temperature and dew point for months May through October.
```r
df <- read_rds("../toy_data.rds")
head(df)
```
As mentioned in the PReMiuM manual it is a good idea to set the seed in the _profRegr_ function and outside the PReMiuM workflow.  Also depending on the size of the dataset (particularly the number of columns), the runtime on _profRegr_ can increase dramatically.  Here is process time is captured with the _tic_ and _toc_ functions.  As this is model is mixed the continuous covariates need to be identified.
```r
cont_vars <- names(df[,3:10])
cont_vars
set.seed(1234)
tic()
runInfoObj <- profRegr(covNames, outcome = 'Yield',
                       yModel = 'Normal', xModel = "Mixed",
                       discreteCovs = "Pedi",
                       continuousCovs = cont_vars,
                       data = df, nSweeps = 100, nBurn = 900,
                       nProgress = 25, seed = 1234)
toc()
```
The object created by _profRegr_ can be useful for analysis later so keep a copy of it is a good idea.  It also can be quite large depending on the size of the dataset and number of iterations during the MCMC run so compressing it to move across the servers should occur.
```r
write_rds(runInfoObj, path = "../runInfoObj.rds", compress = "xz")
```
Next, the rest of the PReMiuM workflow is completed.  The object **riskProfObj** is also saved for future work.  A summary of the analysis can be seen in the plot created.
```r
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
write_rds(riskProfObj, "../riskProfObj.rds", compress = "xz")

clusterOrderObj<-plotRiskProfile(riskProfObj,"summary.png")
```
