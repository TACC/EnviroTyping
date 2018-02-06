In this directory are the input and output files for a job I ran on the hybrid data subset by month.  In *launcherFunc.R* I made a 2 layer function which receives an integer as input from the command line and then uses that value to subset the data by month.  There is another subset which takes the 14 covariate values with the highest variance.  The error and output files are for each month using the months integer as a naming scheme.

Questions?

1.  Why did I receive errors on certain months not experienced on the others? (september and october in particular)

2.  Should I be using a max number of clusters based on the number of covariates? (i.e. if I have 14 weather variables and 1 phenotype variable should I set max clusters to 15)

3.  Are the covariates in the regression limited for the sake of the plot?

Hey 
