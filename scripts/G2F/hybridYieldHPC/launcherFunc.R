library(PReMiuM)
library(dplyr)
library(readr)

setwd("/work/04734/dhbrand/stampede2/Projects/Premium/hybridAnalysis/hybridYieldHPC")

# read in data from analysis script
df <- read_csv("../hybrid.csv")

# grabs the args from the command
args <- commandArgs()

# the arg we are interested in is the month number
mon <- as.integer(args[6])

# workflow
g <- function(y){

    # creates a data frame for the month number
    temp <- df %>% filter(Month==y)


        f <- function(x){

            # find continous variables with variance and return as numericVars
            val <- grep("Min|Max",names(temp))
            numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))]

            # limited to 15 covariates in plotRiskProfile; subsetting to only min and max weather data
            numericVars <- sample(numericVars,14)

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

        }

        f(y)

}

# runs the workflow
g(mon)
