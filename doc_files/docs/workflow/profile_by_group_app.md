Post hoc Analysis Shiny App
---------------------------

#### Loading data
To better visualize the output from the post hoc group a Shinyapp was created to let users look at how the different hybrid groups compare in weather covariates.

The first step to using the app is to open the *shiny_function.R* file and change the paths to both your original data and the **clusObj** from the *PReMiuM* output.  Below is an example:

```r
path_to_clusObj <- "~/github/EnviroTyping/sandbox/shifted_data_analysis/2016/min_vars_3000_no_outliers/clusObj.rds"
path_to_original_data <- "~/github/EnviroTyping/data/interim/2016/hyb_by_mon_calib_wide_shifted.rds"
```

#### Run the App

One of the first lines of code in the *app.R* sources the *shiny_function.R* which will create the new clusters (groups) and the datasets needed to display your plots.  If you are using the same image you can comment out the line of code as below to reduce setup time when reloading the app.

```r
# source("shiny_function.R")
```

Next click **RunApp** on the top right of your script from within RStudio.

#### Understanding the Output

You are presented with two ways to view the output.  You can check the *Scaled Data* box to view a min/max normalization of the weather variables independently or all together.  With the box unchecked you can only view the variables independently as they all have different scales for the y-axis according to the type of weather factor.

Below the plots there is a table which allows you to filter the which hybrids are in each group.  The table defaults to showing all groups and only 10 observations at a time.
