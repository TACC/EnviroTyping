We can use a simulated data set and run through the PReMiuM workflow to get an idea of how it works and an example of the results.

## Creating the Data

We simulate 3 data sets with weather variables, p=90, and yield, n = 650 observations. The data sets have 25 (simulated) stations, 1 replicate of a hybrid in each station, 26 simulated hybrids (thus the total number of observations is 650).

#### Staging the simulation

In your desired local folder place the following 5 files:

* dataset_ready2_PCA_14_15_m50p90_22-6-18.RData
* EigenvectorsMeansSds_PCA14-15AllWeatherVars_p90_n50_24-6-18.RData
* PediLabs26_2014_CommonTo45All10StationsIn2015.RData
* AICs_PvalsPCA14-15_ModellingStartingPC1-9_26Pedis_2-7-18_p90.RData
* TrueParametersMatrix_n26Hyb_p10_2-9-18.RData

Then load the R file located [here.](https://github.com/TACC/EnviroTyping/blob/master/sandbox/shifted_data_analysis/simulated/simcode.R) Change the following line to where you have placed the files.

```
subpath = "/Users/Austin/Documents/SimulationEnv"
```

Run the entire script. The following file `SimulDataset1-3_n650p93WithYield_19-9-18.Rdata` will contain the list of 3 simulated data sets. In the R environment they will be in the list titled `simul.dataset.n650.p93.frame.list`.




## Workflow

Download the `rds` file [here](https://github.com/TACC/EnviroTyping/blob/master/sandbox/shifted_data_analysis/simulated/data/hyb_simulated.rds) and read it into R using `df <- read_rds("path/hyb_simulated.rds")[[1]]`. The `rds` file contains a list of 3 simulated data sets so we select the first data set.

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
