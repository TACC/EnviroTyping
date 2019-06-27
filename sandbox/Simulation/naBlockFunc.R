naBlockFunc <- function(df, desiNA, type = "variable") {
    dat = df
    propNA = 0
    
    if (desiNA > 1){
        stop("desiNA should be between 0 and 1")
        geterrmessage("desiNA should be between 0 and 1")
    }
    
    ##Isolate variables
    if (type == 'variable') {
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
    
        while (isTRUE(all.equal(0, desiNA - propNA, tolerance = .03)) == FALSE) {
        
            sel.var = all.vars[sample(1:length(all.vars),1)]
            #all.vars = all.vars[-which(all.vars == sel.var)]
        
            sel.loc = all.loc[sample(1:length(all.loc),1)]
            #all.loc = all.loc[-which(all.loc == sel.loc)]
        
            dat[grep(sel.loc,dat$StYRe),grep(sel.var,colnames(dat))] = NA
            propNA = mean(is.na(dat[,1:length(dat)]))
        }
        return(dat)
    }
    else if (type == "intervals") {
        # tim1 = colnames(dat)[grep('1', colnames(dat))]
        # tim2 = colnames(dat)[grep('2', colnames(dat))]
        # tim3 = colnames(dat)[grep('3', colnames(dat))]
        # tim4 = colnames(dat)[grep('4', colnames(dat))]
        # 
        # all.tim = c(tim1, tim2, tim3, tim4)
        all.loc = c(as.character(unique(dat$StYRe)))
        
        while (isTRUE(all.equal(0, desiNA - propNA, tolerance = .03)) == FALSE) {
            
            
            sel.tim = colnames(dat)[grep(paste0(sample(1:4,1)), colnames(dat))]
            #sel.tim = all.tim[round(runif(1,1,length(all.tim)))]
            #all.vars = all.vars[-which(all.vars == sel.var)]
            
            sel.loc = all.loc[sample(1:length(all.loc),1)]
            #all.loc = all.loc[-which(all.loc == sel.loc)]
            for (i in 1:length(sel.tim)) {
                dat[grep(sel.loc,dat$StYRe),grep(sel.tim[i],colnames(dat))] = NA
            }
            propNA = mean(is.na(dat[,1:length(dat)]))
        }
        return(dat)
        #return(print(paste("The total proportion of NA's in the data set is", round(propNA,4), "and NA's were blocked by", type, sep = " ")))
        
    }
}
