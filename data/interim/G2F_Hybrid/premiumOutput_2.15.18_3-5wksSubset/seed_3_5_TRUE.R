library(PReMiuM)
library(dplyr)
library(readr)
# install.packages("ggplus", repos = "https://cran.revolutionanalytics.com/")
# library(ggplus)

setwd("/work/04734/dhbrand/stampede2/EnviroTyping/data/interim/G2F_Hybrid/premiumOutput_2.14.18")

# read in data from analysis script
df <- read_csv("../hybrid_by_weeksSincePlanted_cleaned_weather.csv", 
               col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(),
                                "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(),
                                "solarMax" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number()))
# df <- read_csv("data/interim/G2F_Hybrid/hybrid_by_weeksSincePlanted_cleaned_weather.csv", 
#                col_types = cols("Repl" = col_integer(), "rainMin" = col_number(), "rainMax" = col_number(),
#                                 "rainMean" = col_number(), "rainMedian" = col_number(), "solarMin" = col_number(),
#                                 "solarMax" = col_number(), "windDirMin" = col_number(), "windDirMax" = col_number()))


# creates a data frame for the month number
temp <- df %>% filter(seed_3_5 == TRUE)

# find continous variables with highest variance and keep 14 as numericVars
val <- grep("Min|Max",names(temp))

numericVars <- names(temp[val])[vapply(temp[val], function(x) var(x) != 0, logical(1))]

setwd("./output_seed_3_5")

runInfoObj <- profRegr(covNames, outcome = 'Yield',
                yModel = 'Normal', xModel = "Mixed",
                discreteCovs = "Pedi",
                continuousCovs = numericVars,
                data = temp,
                nSweeps = 1000,
                nBurn = 1000)

saveRDS(runInfoObj, file = "runInfoObj.rda")

calcDists <- calcDissimilarityMatrix(runInfoObj)

clusObj <- calcOptimalClustering(calcDists)

optAlloc <- clusObj$clustering

riskProfileOb <- calcAvgRiskAndProfile(clusObj)

clusterOrderObj<-plotRiskProfile(riskProfileOb,  whichCovariates = c("tempMin", "tempMax", "dewMin", "dewMax", 
                                                                     "humidMin", "humidMax", "solarMin", "solarMax", "rainMax"),
                                 outFile = "temp_dew_humid_solar_rainMax.png")

clusterOrderObj<-plotRiskProfile(riskProfileOb,  whichCovariates = c("windSpdMin", "windSpdMax", "windDirMin", "windDirMax",
                                                                     "windGustMin", "windGustMax", "soilTempMin", "soilTempMax",
                                                                     "soilMoistMin", "soilMoistMax"),
                                 outFile = "windSpd_windDir_windGust_soilTemp_soilMoist.png")

tmp_vp<-data.frame(opt=as.factor(optAlloc), outc=clusts$clusObjRunInfoObj$yMat, known=as.factor(temp$Pedi))
saveRDS(tmp_vp, file = "temp_vp.rda")

# pdf("./violinPlots.pdf", paper="a4")
# p2 <- ggplot(tmp_vp, aes(x=opt, y=outc)) +
#     geom_violin(aes(fill = opt)) + 
#     geom_point(size = 1, stroke = 1, shape = 16) +
#     labs(x="clusters", y = "outcome") +
#     scale_y_continuous(breaks=seq(0, 300, 75))
# 
# facet_multiple(plot = p2, 
#                facets = 'known', 
#                ncol = 3, 
#                nrow = 3)
# dev.off()