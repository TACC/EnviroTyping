library(PReMiuM)
library(dplyr)
library(readr)


# read in data from analysis script
temp <- read_csv("data/interim/G2F_Hybrid/hybrid_bymonth_cleaned_weather.csv")


# the arg we are interested in is the month number
#mon <- as.integer(args[6])

# creates a data frame for the month number
#temp <- df %>% filter(Month==y)

# find continous variables with highest variance and keep 14 as numericVars
val<- grep("Min|Max",names(temp))
numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))]

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                yModel = 'Normal', xModel = "Mixed",
                discreteCovs = "Pedi",
                continuousCovs = numericVars,
                #output = "output/",
                data = temp,
                nSweeps = 1000,
                nBurn = 1000)


calcDists <- calcDissimilarityMatrix(runInfoObj)

clusts <- calcOptimalClustering(calcDists,maxNClusters = 6)

riskProfileOb <- calcAvgRiskAndProfile(clusts)

optAlloc<-clusts$clustering

clusterOrderObj<-plotRiskProfile(riskProfileOb,"./scripts/newPlots/summary.png", whichCovariates = c("tempMin", "tempMax", "dewMin", "dewMax", "Pedi"))


