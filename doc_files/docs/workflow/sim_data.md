We can use a simulated data set and run through the PReMiuM workflow to get an idea of how it works and an example of the results.

## Workflow

Download the `rds` file [here](https://github.com/TACC/EnviroTyping/blob/master/sandbox/shifted_data_analysis/simulated/data/hyb_simulated.rds) and read it into R using `df <- read_rds("path/hyb_simulated.rds")[[1]]` where `path` is the path to the downloaded file. The `rds` file contains a list of 3 simulated data sets so we select the first data set.

Check for constant (no variance) variables and for this example select the minimum variables.

``` r
variance.var <- names(which(map_dbl(df[,6:93], var, na.rm = TRUE) != 0))
min.vars <- str_subset(variance.var, "Min")
```

Run the profile regression and post-processing.

``` r
runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 1000, nProgress = 100)
calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
riskProfObj <- calcAvgRiskAndProfile(clusObj)
```

Save the output as `rds` files to easily read in for post-hoc analysis.

``` r
write_rds(riskProfObj, "../riskProfObj.rds", compress = "xz")
write_rds(clusObj, "../clusObj.rds", compress = "xz")
```
