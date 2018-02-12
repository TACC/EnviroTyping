library(PReMiuM)
library(ggplot2)
library(readr)
library(dplyr)
library(grid)

devtools::install_github("guiastrennec/ggplus")
library(ggplus)
getwd()
df <- read_csv("data/interim/G2F_Hybrid/hybrid_bymonth_cleaned_weather.csv")
x <- 8
temp <- df %>% filter(Month==x)

        val <- grep("Min|Max",names(temp))
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


dissimObj<-calcDissimilarityMatrix(mod)

clusObj<-calcOptimalClustering(dissimObj,maxNClusters = 10)

riskProfileObj<-calcAvgRiskAndProfile(clusObj)

optAlloc<-clusObj$clustering

length(optAlloc)
plot(optAlloc,inputs$inputData$outcome[1:1992])
outcome=inputs$inputData$outcome[1:1992]
outc <- mod$yMat

inputs <- generateSampleDataFile(clusSummaryNormalDiscrete())
known<-c(rep("A",200),rep("B",200), rep("C",150),rep("D",250),rep("E",196),
         rep("F",200),rep("G",200),rep("H",150),rep("I",250),rep("J",196))

tmp_boxplot<-data.frame(opt=as.factor(optAlloc), outc, known=as.factor(known))
p <- ggplot(tmp_boxplot, aes(x=known, y=outc, fill=opt)) +
    geom_violin()+
    labs(title="",x="Known Truth", y = "outcome") +
    facet_grid(~known,scales='free',space='free') +
    guides(fill=guide_legend(title="Clusters")) +
    theme(strip.text.x = element_blank(), strip.background = element_blank())
# Use brewer color palettes
p+scale_fill_brewer(palette="Set3")


pdf("hybrids_violinplots.pdf", paper="a4")
tmp_bp<-data.frame(opt=as.factor(optAlloc), outc=mod$yMat, known=as.factor(temp$Pedi))
p2 <- ggplot(tmp_bp, aes(x=opt, y=outc)) +
    geom_violin(aes(fill = opt)) + 
    geom_jitter(height = 0, width = 0.1) +
    labs(x="clusters", y = "outcome") +
    scale_y_continuous(breaks=seq(0, 300, 75))
    #facet_grid(~opt,scales='free',space='free') +
    #guides(fill=guide_legend(title="Clusters")) +
    #theme(strip.text.x = element_blank(), strip.background = element_blank()) +
    #scale_fill_brewer(palette="Set3")

facet_multiple(plot = p2, 
               facets = 'known', 
               ncol = 4, 
               nrow = 4)
dev.off()


tmp_bp<-data.frame(opt=as.factor(optAlloc), outc=mod$yMat, known=as.factor(temp$Pedi))
pdf("test.pdf", paper="a4")
for (i in seq(1, length(unique(tmp_bp$known)), 6)) {
    
    print(ggplot(tmp_bp[tmp_bp$known %in% levels(tmp_bp$known)[i:(i+5)], ],
           aes(x=known, y=outc, fill=opt)) +
        geom_violin()+
        labs(title="",x="Known Truth", y = "outcome") +
        facet_grid(~known,scales='free',space='free') +
        guides(fill=guide_legend(title="Clusters")) +
        theme(strip.text.x = element_blank(), strip.background = element_blank()) +
        scale_fill_brewer(palette="Set3"))
    
    }
dev.off()


# tmp_bp %>%
#     select_if(function(x) any(is.na(x))) %>%
#     summarise_all(funs(sum(is.na(.))))

print(ggplot(Baseball[Baseball$team87 %in% levels(Baseball$team87)[i:(i+5)], ], 
             aes(hits86, sal87)) + 
          geom_point() +
          facet_wrap(~ team87) +
          scale_y_continuous(limits=c(0, max(Baseball$sal87, na.rm=TRUE))) +
          scale_x_continuous(limits=c(0, max(Baseball$hits86))) +
          theme_bw())
