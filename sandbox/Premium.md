---
output: 
  html_document: 
    self_contained: no
    theme: cerulean
---
1. ggTimeSeries
2. change color mapping to exp variable
3. ggStance - make horizontal violin plots
4. what are the most similar location and are there discernible groups of hybrids that our similar within each group of similar locations
5. read about crossover interactions


3/19
1. Put together items to present:
    * Be able to associate which hybrids grow are most similar by profile
    * Determine how the crossover interaction between hybrid and weather covariates is driving the cluster association
    * Run tests to determine the lowest number of iterations necessary to generate results (combination of burn in + steps)
    * Find alternative to pam in the calcOptimalClustering step
        * show
2. Sharing and storing data with worldwide users:alternative email accounts

Tues at noon review everything



4/13
1.  Ran subsets of the data to test the predictions options
2.  Used both the long and wide data formats
3.  Used the built in simulations with 50/50 split and 80/20 5 fold CV
    *   around 89 R2 and 1.3 RMSE
4.  Also tested using the subset of 45 samples across each location
    *   results were comparable in all trials with subset data
    *   both clusters and runtime increases with more columns


