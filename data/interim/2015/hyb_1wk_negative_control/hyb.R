library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)

setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/hyb_1wk_negative_control")
date <- as_date("2015-04-30")
isoweek(date)
byWeek <- read_rds("../hybrid_by_week_cleaned_weather.Rds")

hyb <- read_rds("riskProfObj.rds")

s1 <- byWeek %>% filter(Week >= isoweek(Planted) & Week < isoweek(Planted) + 2 )
val <- grep("Min|Max",names(s1))
numericVars <- names(s1[val])[vapply(s1[val], function(x) var(x) != 0, logical(1))]

hyb1 <- data.frame(optcl = hyb$riskProfClusObj$clustering,Exp = s1$Exp, Hyb = s1$Pedi, Yield = s1$Yield, s1[numericVars])

hyb2 <- hyb1 %>% group_by(optcl, Exp) %>% summarise(meanYield = mean(Yield)) %>% arrange(desc(meanYield))
hyb3 <- hyb1 %>% group_by(optcl) %>% summarise(meanYield = mean(Yield)) %>% arrange(desc(meanYield))
table(hyb1$optcl,hyb1$Exp)
hyb4 <- hyb1 %>% filter(optcl %in% 3 & Exp == "NCH1") %>% arrange(desc(Yield))
hyb5 <- hyb1 %>% filter(optcl %in% 3 & Exp == "GAH1") %>% arrange(desc(Yield))

profMu <- matrix(ncol = 19, nrow = 14)
for (i in 1:14){
    temp <- data.frame(riskProfObj$profileMu[,i,])
    profMu[i,] <- apply(temp,2,mean)
}
profMu[,1:2]


p <- ggplot(clusDF, aes(optcl, Yield)) + geom_violin() + facet_grid(~optcl)
print(p)

p <- ggplot(s0) + geom_violin()
print(p)

dev.off()

temp <- clusDF %>% filter(Exp=="ONH1")
p2 <- ggplot(temp, aes(x=optcl, y=temp[,2], fill = "#0f00ea")) + geom_violin() + ylab(names(temp[2])) + scale_fill_brewer() + guides(fill=FALSE)
p3 <- ggplot(temp, aes(x=optcl, y=temp[,3], fill = "#820707")) + geom_violin() + ylab(names(temp[3])) + scale_fill_brewer()  + guides(fill=FALSE)
p4 <- ggplot(temp, aes(x=optcl, y=temp[,4], fill = "#00cc00")) + geom_violin() + ylab(names(temp[4])) + scale_fill_brewer()  + guides(fill=FALSE)
p5 <- ggplot(temp, aes(x=optcl, y=temp[,5], fill = "yellow")) + geom_violin() + ylab(names(temp[5])) + guides(fill=FALSE)
p6 <- ggplot(temp, aes(x=optcl, y=temp[,6], fill = "orange")) + geom_violin() + ylab(names(temp[6])) + guides(fill=FALSE)
p7 <- ggplot(temp, aes(x=optcl, y=temp[,7], fill = "purple")) + geom_violin() + ylab(names(temp[7])) + guides(fill=FALSE)
p8 <- ggplot(temp, aes(x=optcl, y=temp[,8], fill = "#CC0033")) + geom_violin() + ylab(names(temp[8])) + guides(fill=FALSE)
p9 <- ggplot(temp, aes(x=optcl, y=temp[,9], fill = "#CC0033")) + geom_violin() + ylab(names(temp[9])) + guides(fill=FALSE)
p10 <- ggplot(temp, aes(x=optcl, y=temp[,10], fill = "#CC0033")) + geom_violin() + ylab(names(temp[10])) + guides(fill=FALSE) 
p11 <- ggplot(temp, aes(x=optcl, y=temp[,11], fill = "#CC0033")) + geom_violin() + ylab(names(temp[11])) + guides(fill=FALSE)
p12 <- ggplot(temp, aes(x=optcl, y=temp[,12], fill = "#CC0033")) + geom_violin() + ylab(names(temp[12])) + guides(fill=FALSE) 
p13 <- ggplot(temp, aes(x=optcl, y=temp[,13], fill = "#CC0033")) + geom_violin() + ylab(names(temp[13])) + guides(fill=FALSE)
p14 <- ggplot(temp, aes(x=optcl, y=temp[,14], fill = "#CC0033")) + geom_violin() + ylab(names(temp[14])) + guides(fill=FALSE)
p15 <- ggplot(temp, aes(x=optcl, y=temp[,15], fill = "#CC0033")) + geom_violin() + ylab(names(temp[15])) + guides(fill=FALSE) 
p16 <- ggplot(temp, aes(x=optcl, y=temp[,16], fill = "#CC0033")) + geom_violin() + ylab(names(temp[16])) + guides(fill=FALSE) 
p17 <- ggplot(temp, aes(x=optcl, y=temp[,17], fill = "#CC0033")) + geom_violin() + ylab(names(temp[17])) + guides(fill=FALSE) 
p18 <- ggplot(temp, aes(x=optcl, y=temp[,18], fill = "#CC0033")) + geom_violin() + ylab(names(temp[18])) + guides(fill=FALSE)
p19 <- ggplot(temp, aes(x=optcl, y=temp[,19], fill = "#CC0033")) + geom_violin() + ylab(names(temp[19])) + guides(fill=FALSE) 
p20 <- ggplot(temp, aes(x=optcl, y=temp[,20], fill = "#CC0033")) + geom_violin() + ylab(names(temp[20])) + guides(fill=FALSE)
p21 <- ggplot(temp, aes(x=optcl, y=temp[,21], fill = "#CC0033")) + geom_violin() + ylab(names(temp[21])) + guides(fill=FALSE)

grid.arrange(p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,nrow=2, ncol=10)

p <- list()
temp <- clusDF %>% filter(optcl==1)
for (i in 2:21){
    
    p[[i]] <- ggplot(temp, aes(x=optcl, y=temp[,i])) + geom_violin() + ylab(names(temp[i]))
}
do.call(grid.arrange,p)
l[2]

library(grid)
grid.arrange(rectGrob(), rectGrob())
## Not run:  
library(ggplot2)
pl <- lapply(1:11, function(.x) qplot(1:10, rnorm(10), main=paste("plot", .x)))
ml <- marrangeGrob(pl, nrow=2, ncol=2)
## non-interactive use, multipage pdf
ggsave("multipage.pdf", ml)
## interactive use; open new devices
ml
dev.off()
