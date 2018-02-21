library(sprint)
library(parallel)

calcOptimalClustering<-function(disSimObj,maxNClusters=NULL,useLS=F){
    
    disSimRunInfoObj=NULL
    directoryPath=NULL
    fileStem=NULL
    lsOptSweep=NULL
    nSubjects=NULL
    nPredictSubjects=NULL
    reportBurnIn=NULL
    nBurn=NULL
    nFilter=NULL
    nSweeps=NULL
    onlyLS=NULL
    
    for (i in 1:length(disSimObj)) assign(names(disSimObj)[i],disSimObj[[i]])
    for (i in 1:length(disSimRunInfoObj)) assign(names(disSimRunInfoObj)[i],disSimRunInfoObj[[i]])
    
    if (onlyLS==TRUE) useLS <- TRUE
    
    if(useLS){
        # maniupulation for least squares method, but computation has been done in previous function
        zFileName <- file.path(directoryPath,paste(fileStem,'_z.txt',sep=''))
        zFile<-file(zFileName,open="r")
        
        optZ<-scan(zFile,what=integer(),skip=lsOptSweep-1,n=nSubjects+nPredictSubjects,quiet=T)
        optZFit<-optZ[1:nSubjects]
        if(nPredictSubjects>0){
            optZPredict<-optZ[(nSubjects+1):(nSubjects+nPredictSubjects)]
        }
        uZFit<-unique(optZFit)
        chosenNClusters<-length(unique(uZFit))
        clustVec<-match(optZFit,uZFit)
        clustSizes<-rep(0,chosenNClusters)
        for(c in 1:chosenNClusters){
            clustSizes[c]<-length(which(clustVec==c))
        }
        
        clusteringPred<-NULL
        if(nPredictSubjects>0){
            clusteringPred<-match(optZPredict,uZFit)
        }
        avgSilhouetteWidth<-NULL
        close(zFile)
        
    }else{
        
        if(is.null(maxNClusters)){
            # Determine the maximum number of clusters
            nMembersFileName<-file.path(directoryPath,paste(fileStem,'_nMembers.txt',sep=''))
            nMembersFile<-file(nMembersFileName,open="r")
            nClustersFileName<- file.path(directoryPath,paste(fileStem,'_nClusters.txt',sep=''))
            nClustersFile<-file(nClustersFileName,open="r")
            
            # Restrict to sweeps after burn in
            firstLine<-ifelse(reportBurnIn,nBurn/nFilter+2,1)
            lastLine<-(nSweeps+ifelse(reportBurnIn,nBurn+1,0))/nFilter
            maxNClusters<-0
            
            for(sweep in firstLine:lastLine){
                if(sweep==firstLine){
                    skipVal<-firstLine-1
                }else{
                    skipVal<-0
                }
                
                # Get the current number of members for each cluster
                nClusters<-scan(nClustersFile,what=integer(),skip=skipVal,n=1,quiet=T)
                currNMembers<-scan(nMembersFile,what=integer(),skip=skipVal,n=nClusters+1,quiet=T)
                currNMembers<-currNMembers[1:nClusters]
                # Find the number of non-empty clusters
                nNotEmpty<-sum(currNMembers>0)
                if(nNotEmpty>maxNClusters){
                    maxNClusters<-nNotEmpty
                }
            }
            # Add on another 5 just to make sure bound is safe (but don't let it exceed no. of subjects -1)
            maxNClusters<-min(maxNClusters+5,nSubjects-1)
            
            close(nMembersFile)
            close(nClustersFile)
        }
        
        # If the input was a list of dissimilarity matrices then take the average
        if(is.list(disSimMat)){
            for(i in 1:length(disSimMat)){
                if(i==1){
                    tmpMat<-disSimMat[[i]]
                }else{
                    tmpMat<-tmpMat+disSimMat[[i]]
                }
            }
            tmpMat<-tmpMat/length(disSimMat)
            disSimMat<-tmpMat
        }
        
        # Loop over the possible number of clusters
        avgSilhouetteWidth<--1.0;
        cat(paste("Max no of possible clusters:",maxNClusters,"\n"))
        for(c in 2:maxNClusters){
            cat(paste("Trying",c,"clusters\n"))
            tmpObj<-ppam(disSimMat, k = 5, is_dist = TRUE)
            # Check whether the silhouette width from this clustering improves previous best
            if(avgSilhouetteWidth<tmpObj$silinfo$avg.width){
                avgSilhouetteWidth<-tmpObj$silinfo$avg.width
                chosenNClusters<-c
                clustVec<-tmpObj$clustering
                clustSizes<-tmpObj$clusinfo[,1]
                # The id of the objects chosen as the medoids
                clustMedoids<-tmpObj$id.med
            }
        }
        pterminate()
        # Work out the clustering of the prediction objects
        clusteringPred<-NULL
        if(nPredictSubjects>0){
            disSimMatPred<-matrix(disSimMatPred,nrow=nPredictSubjects,byrow=T)
            clusteringPred<-rep(0,nPredictSubjects)
            for(i in 1:nPredictSubjects){
                tmpVec<-disSimMatPred[i,clustMedoids]
                whichMin <- which(tmpVec==min(tmpVec))
                if (length(whichMin)>1) {
                    clusteringPred[i]<-sample(whichMin,1)
                } else {
                    clusteringPred[i]<-whichMin
                }
            }
        }
    }
    
    return(list("clusObjRunInfoObj"=disSimObj$disSimRunInfoObj,
                "clusObjDisSimMat"=disSimObj$disSimMat,
                "nClusters"=chosenNClusters,
                "clusterSizes"=clustSizes,
                "clustering"=clustVec,
                "avgSilhouetteWidth"=avgSilhouetteWidth,
                "clusteringPred"=clusteringPred))
}
