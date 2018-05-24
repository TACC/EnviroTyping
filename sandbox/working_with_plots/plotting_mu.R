library(viridis)

# contd from var_sel_min_only

post.mu.clus1 <- riskProfObj2$profileMu[,1,] %>% as_tibble %>% set_names(contVars) %>% cbind(.,clus = as.character(rep(1,3000)))
post.mu.clus2 <- riskProfObj2$profileMu[,2,] %>% as_tibble %>% set_names(contVars) %>% cbind(., clus = as.character(rep(2,3000)))

post.mu <- bind_rows(post.mu.clus1,post.mu.clus2)

min.values <- df %>% map_at(contVars,min) %>% as_tibble
min.values <- min.values[1,contVars]
max.values <- df %>% map_at(contVars,max) %>% as_tibble
max.values <- max.values[1,contVars]
min.max <- rbind(min.values, max.values)

ggplot(gather(post.mu.clus1,key,value), aes())

l <- list()
n <- 1
for (i in names(df[contVars])) {
    min <- as.numeric(min.max[1,n])
    max <- as.numeric(min.max[2,n])
    p <- ggplot(post.mu,aes(x = clus,y = eval(substitute(i)))) + geom_boxplot() + scale_y_continuous(limits = c(min , max))
    l[[i]] <- p
    n <- n + 1
    print(eval(substitute(i)))
}
l[[1]]
plotter <- function(var,index) {
    min <- as.numeric(min.max[1,index])
    max <- as.numeric(min.max[2,index])
    ggplot(post.mu,aes(x = clus,y = var, fill = clus)) + 
        geom_boxplot() + xlab("Cluster") + ylab(substitute(var)) +
        scale_y_continuous(limits = c(min , max)) + 
        theme(legend.position = "none") +
        scale_fill_brewer(palette = "Blues")
}
plotter(post.mu$humidMin_5,5)


l <- list()
n <- 1
for (i in names(df[contVars])) {
    p <- plotter(paste("post.mu$",i,sep = ""),n)
    l[[i]] <- p
    n <- n + 1
}
str(l[[2]])
dev.off()
pdf("post.mu.pdf")
plotter(post.mu$dewMin_5,1)
plotter(post.mu$dewMin_6,2)
plotter(post.mu$dewMin_7,3)
plotter(post.mu$dewMin_8,4)
plotter(post.mu$dewMin_9,5)
plotter(post.mu$humidMin_5,6)
plotter(post.mu$humidMin_6,7)
plotter(post.mu$humidMin_7,8)
plotter(post.mu$humidMin_8,9)
plotter(post.mu$humidMin_9,10)
plotter(post.mu$soilMoistMin_5,11)
plotter(post.mu$soilMoistMin_6,12)
plotter(post.mu$soilMoistMin_7,13)
plotter(post.mu$soilMoistMin_8,14)
plotter(post.mu$soilMoistMin_9,15)
plotter(post.mu$soilTempMin_5,16)
plotter(post.mu$soilTempMin_6,17)
plotter(post.mu$soilTempMin_7,18)
plotter(post.mu$soilTempMin_8,19)
plotter(post.mu$soilTempMin_9,20)
plotter(post.mu$tempMin_5,21)
plotter(post.mu$tempMin_6,22)
plotter(post.mu$tempMin_7,23)
plotter(post.mu$tempMin_8,24)
plotter(post.mu$tempMin_9,25)
dev.off()

