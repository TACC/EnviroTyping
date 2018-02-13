library(PReMiuM)
library(dplyr)
library(readr)


# read in data from analysis script
df <- read_csv("data/interim/G2F_Hybrid/hybrid_bymonth_cleaned_weather.csv")


# the arg we are interested in is the month number
mon <- as.integer(args[6])

# creates a data frame for the month number
temp <- df %>% filter(Month==y)

# find continous variables with highest variance and keep 14 as numericVars
            va<- grep("Min|Max",names(temp))
            # numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))]
            numericVars <- names(sort(unlist(lapply(temp[val],function(x) var(x))), decreasing = TRUE)[1:14])

            # create directory using month number to name and changes to working directory
            dir.create(paste(month.name[x]))
            setwd(paste(getwd(),month.name[x], sep = "/"))

            mod <- profRegr(covNames, outcome = 'Yield',
                            yModel = 'Normal', xModel = "Mixed",
                            discreteCovs = "Pedi",
                            continuousCovs = numericVars,
                            data = temp,
                            nSweeps = 1000,
                            nBurn = 1000)


            calcDists <- calcDissimilarityMatrix(mod)

            clusts <- calcOptimalClustering(calcDists,maxNClusters = 10)

            riskProfileOb <- calcAvgRiskAndProfile(clusts)

            clusterOrderObj<-plotRiskProfile(riskProfileOb,paste(month.name[x],".png",sep = ""))

# runs the workflow
g(mon)
