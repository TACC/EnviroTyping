
setwd("~/GitHub/EnviroTyping/data/interim/G2F_Hybrid/month_45_162covs")
members <- as.numeric(strsplit(readLines("output/output_nMembers.txt",1)," ")[[1]])

ind <- which(members != 0)
clusters <- members[ind]
str(clusters)
max(clusters[-876])
table(clusters[-876])

