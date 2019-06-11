naBlockFunc <- function(df, desiNA, seed) {
    set.seed(seed)
    dat = df
    propNA = 0
    ##Isolate variables
    
    min = colnames(dat)[grep('Min', colnames(dat))]
    #min.ind = min
    min = substring(min, 1, nchar(min)-2)
    min = unique(min)
    
    max = colnames(dat)[grep('Max', colnames(dat))]
    #max.ind = max
    max = substring(max, 1, nchar(max)-2)
    max = unique(max)
    
    mean = colnames(dat)[grep('Mean', colnames(dat))]
    #mean.ind = mean
    mean = substring(mean, 1, nchar(mean)-2)
    mean = unique(mean)
    
    
    ##String of all variables/stations
    
    all.vars = c(min,mean,max)
    all.loc = c(as.character(unique(dat$StYRe)))
    
    #Begin while loop
    
    while (isTRUE(all.equal(0, desiNA - propNA, tolerance = .05)) == FALSE) {
        
        sel.var = all.vars[round(runif(1,1,length(all.vars)))]
        #all.vars = all.vars[-which(all.vars == sel.var)]
        
        sel.loc = all.loc[round(runif(1,1,length(all.loc)))]
        #all.loc = all.loc[-which(all.loc == sel.loc)]
        
        dat[grep(sel.loc,dat$StYRe),grep(sel.var,colnames(dat))] = NA
        propNA = mean(is.na(dat[,1:length(dat)]))
    }
    return(dat)
}
#     
#     ##Choose variable
#     
#     sel.var = all.vars[round(runif(1,1,length(all.vars)))]
#     all.vars = all.vars[-which(all.vars == sel.var)]
#     
#     
#     ##Set corresponding variable to NA and check total % of NA
#     
#     dat[,grep(sel.var,colnames(dat))] = NA
#     propNA = mean(is.na(dat[,1:length(dat)]))
#     
#     toggle = isTRUE(all.equal(0, desiNA - propNA, tolerance = .05))
#     
#     if (toggle == TRUE) {
#     return(dat)
#     } else {
#         sel.var = all.vars[round(runif(1,1,length(all.vars)))]
#         all.vars = all.vars[-which(all.vars == sel.var)]   
#         dat[,grep(sel.var,colnames(dat))] = NA
#         propNA = mean(is.na(dat[,1:length(dat)]))
#     } 
# }