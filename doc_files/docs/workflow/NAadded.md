We develop a tool designed to add NAs to our G2F data that mirrors situations that may cause NAs in a real world setting. The user can control the proportion of NAs being added to the data and whether they are added by time interval or by variable.


## Reason for adding NAs

In previous attempts to account for NAs when testing PReMiuM, NAs were added randomly to the data. In that test the NAs did not seem to affect the results, but in real world scenarios NAs are usually present in blocks of the data due to failing equipment, bad reporting, etc. This program is designed to mimic those scenarios.

## Function for adding NAs

The following is the full code for the function with comments at each step.

<details><summary>CLICK TO EXPAND</summary>
<p>

```{r}
naBlockFunc <- function(df, desiNA, type = "variable") {
    dat = df
    propNA = 0

    ##check for improper input
    if (desiNA > 1){
        stop("desiNA should be between 0 and 1")
        geterrmessage("desiNA should be between 0 and 1")
    }

    ##Isolate variables
    if (type == 'variable') {
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


        ##String of all variables/stations

        all.vars = c(min,mean,max)
        all.loc = c(as.character(unique(dat$StYRe)))


        #Begin while loop, replace randomly selected intervals/variables with NAs until desired NA proportion is reached

        while (isTRUE(all.equal(0, desiNA - propNA, tolerance = .03)) == FALSE) {

            sel.var = all.vars[sample(1:length(all.vars),1)]
            #all.vars = all.vars[-which(all.vars == sel.var)]

            sel.loc = all.loc[sample(1:length(all.loc),1)]
            #all.loc = all.loc[-which(all.loc == sel.loc)]

            dat[grep(sel.loc,dat$StYRe),grep(sel.var,colnames(dat))] = NA
            propNA = mean(is.na(dat[,1:length(dat)]))
        }
        return(dat)
    }
    else if (type == "intervals") {
        # tim1 = colnames(dat)[grep('1', colnames(dat))]
        # tim2 = colnames(dat)[grep('2', colnames(dat))]
        # tim3 = colnames(dat)[grep('3', colnames(dat))]
        # tim4 = colnames(dat)[grep('4', colnames(dat))]
        #
        # all.tim = c(tim1, tim2, tim3, tim4)
        all.loc = c(as.character(unique(dat$StYRe)))

        while (isTRUE(all.equal(0, desiNA - propNA, tolerance = .03)) == FALSE) {


            sel.tim = colnames(dat)[grep(paste0(sample(1:4,1)), colnames(dat))]
            #sel.tim = all.tim[round(runif(1,1,length(all.tim)))]
            #all.vars = all.vars[-which(all.vars == sel.var)]

            sel.loc = all.loc[sample(1:length(all.loc),1)]
            #all.loc = all.loc[-which(all.loc == sel.loc)]
            for (i in 1:length(sel.tim)) {
                dat[grep(sel.loc,dat$StYRe),grep(sel.tim[i],colnames(dat))] = NA
            }
            propNA = mean(is.na(dat[,1:length(dat)]))
        }
        return(dat)
        #return(print(paste("The total proportion of NA's in the data set is", round(propNA,4), "and NA's were blocked by", type, sep = " ")))

    }
}
```

</p>
</details>


## Workflow Example

In our example, we load our simulated data sets, and select one of the three to use. We then load the function into the environment for use.

```{r}
dat = read_rds('~/EnviroTyping/sandbox/shifted_data_analysis/simulated/data/hyb_simulated.rds')
dat = dat[[1]]
source("~/EnviroTyping/sandbox/Simulation/naBlockFunc.R")
```

Before running the profile regression, we will apply our function for adding NAs. We choose our preferred stopping point of `0.40` as the proportion of total NAs in the data set. We want to insert the NAs by time by variable, which indicates there were problems with certain equipment that measure these variables for the months they are missing.

```{r}
df = naBlockFunc(dat, 0.40, "variable")
```

We then proceed with the typical PReMiuM workflow as previously detailed.

```{r}
variance.var <- names(which(map_dbl(df[,6:93], var, na.rm = TRUE) != 0))
min.vars <- str_subset(variance.var, "Min")

runInfoObj <- profRegr(covNames, outcome = 'Yield', yModel = 'Normal', xModel = "Mixed", discreteCovs = "Pedi", continuousCovs = min.vars, data = df, nSweeps = 3000, nBurn = 1000, nProgress = 500)

calcDists <- calcDissimilarityMatrix(runInfoObj)
clusObj <- calcOptimalClustering(calcDists)
```

## Results after running

After experimenting with an increasing percentage under both types of NA injection, we find that more NAs tend to lead to faster runs and fewer clusters. This is what is expected, however we can further interpret how these results can help in a real world setting. Such as: using this function to determine how much data should be collected before PReMiuM will provide meaningful results if there are too many NAs.
