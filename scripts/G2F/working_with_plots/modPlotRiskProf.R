for (i in 1:length(riskProfObj)) assign(names(riskProfObj)[i],riskProfObj[[i]])
for (i in 1:length(riskProfClusObj)) assign(names(riskProfClusObj)[i],riskProfClusObj[[i]])
for (i in 1:length(clusObjRunInfoObj)) assign(names(clusObjRunInfoObj)[i],clusObjRunInfoObj[[i]])
outfile = "summary.png"
whichClusters=NULL
orderBy=NULL
if(!is.null(orderBy)){
    if(!includeResponse){
        if(orderBy!='Empirical'&&orderBy!='ClusterSize'&&!orderBy%in%covNames){
            if(is.numeric(orderBy)){
                if(length(orderBy)==nClusters){
                    orderProvided<-T
                    meanSortIndex<-orderBy
                }else{
                    cat("Order vector provided not of same length as number of clusters. Reverting to default ordering.\n")
                    orderBy<-NULL
                }
                orderBy<-NULL
            }
            #orderBy<-NULL
        }
    }else{
        if(orderBy!='Risk'&&orderBy!='Empirical'&&orderBy!='ClusterSize'&&!orderBy%in%covNames){
            if(is.numeric(orderBy)){
                if(length(orderBy)==nClusters){
                    orderProvided<-T
                    meanSortIndex<-orderBy
                }else{
                    cat("Order vector provided not of same length as number of clusters. Reverting to default ordering.\n")
                    orderBy<-NULL
                }
                orderBy<-NULL
            }
            #orderBy<-NULL
        }
    }

}
if(!orderProvided){
    if(!is.null(risk)){
        if(is.null(orderBy)){
            # Default is to order by posterior theta risk
            # Compute the means
            orderStat<-apply(risk,2,median)
        }else{
            if(orderBy=='Risk'){
                orderStat<-apply(risk,2,median)
            }else if(orderBy=='Empirical'){
                orderStat<-empiricals
            }else if(orderBy=='ClusterSize'){
                orderStat<-clusterSizes
            }else{
                whichCov<-match(orderBy,covNames)
                if(xModel=='Normal'){
                    orderStat<-apply(profile[,,whichCov],2,median)
                }else{
                    # This assumes that there is some order to the categories
                    # and then uses an expected value
                    tmpMat<-profile[,,whichCov,1]
                    if(nCategories[whichCov]>1){
                        for(k in 2:nCategories[whichCov]){
                            tmpMat<-tmpMat+k*profile[,,whichCov,k]
                        }
                    }
                    orderStat<-apply(tmpMat,2,median)
                }
            }
        }
    }else{
        if(is.null(orderBy)){
            # Default is to order by empirical risk
            orderStat<-empiricals
        }else{
            if(orderBy=='Empirical'){
                orderStat<-empiricals
            }else if(orderBy=='ClusterSize'){
                orderStat<-clusterSizes
            }else{
                whichCov<-match(orderBy,covNames)
                if(xModel=='Normal'){
                    orderStat<-apply(profile[,,whichCov],2,median)
                }else{
                    # This assumes that there is some order to the categories
                    # and then uses an expected value
                    tmpMat<-profile[,,whichCov,1]
                    if(nCategories[whichCov]>1){
                        for(k in 2:(nCategories[whichCov])){
                            tmpMat<-tmpMat+k*profile[,,whichCov,k]
                        }
                    }
                    orderStat<-apply(tmpMat,2,median)
                }
            }
        }
    }
    # Sort into ascending mean size
    meanSortIndex<-order(orderStat,decreasing=F)
}

png(outFile,width=1200,height=800)
orderProvided<-F


for(j in 1:nCovariates){
    if (j<=nDiscreteCovs){
            # profileDF<-data.frame("prob"=c(),"cluster"=c(),"category"=c(),"meanProb"=c(),
            # 	"lowerProb"=c(),"upperProb"=c(),"fillColor"=c())

            #################################################################
            my.list <- vector('list', length(whichClusters)*nCategories[j])
            z=1
            #################################################################

            for(k in 1:nCategories[j]){
                if (nDiscreteCovs==1) {
                    probMat<-profilePhi[,meanSortIndex,1,k]
                } else {
                    probMat<-profilePhi[,meanSortIndex,j,k]
                }
                nPoints<-nrow(probMat)
                probMeans<-apply(probMat,2,mean)
                probMean<-sum(probMeans*clusterSizes)/sum(clusterSizes)
                probLower<-apply(probMat,2,quantile,0.05)
                probUpper<-apply(probMat,2,quantile,0.95)

                # Get the plot colors
                probColor<-ifelse(probLower>rep(probMean,length(probLower)),"high",
                                  ifelse(probUpper<rep(probMean,length(probUpper)),"low","avg"))



                for(c in whichClusters){
                    # profileDF<-rbind(profileDF,data.frame("prob"=probMat[,c],"cluster"=rep(c,nPoints),
                    # 	"category"=rep(k-1,nPoints),
                    # 	"meanProb"=rep(probMean,nPoints),
                    # 	"lowerProb"=rep(probLower[c],nPoints),
                    # 	"upperProb"=rep(probUpper[c],nPoints),
                    # 	"fillColor"=rep(probColor[c],nPoints)))
                    # 	 rownames(profileDF)<-seq(1,nrow(profileDF),1)

                    #################################################################
                    my.list[[z]] <- data.frame("prob"=probMat[,c],"cluster"=rep(c,nPoints),
                                               "category"=rep(k-1,nPoints),
                                               "meanProb"=rep(probMean,nPoints),
                                               "lowerProb"=rep(probLower[c],nPoints),
                                               "upperProb"=rep(probUpper[c],nPoints),
                                               "fillColor"=rep(probColor[c],nPoints))
                    z=z+1
                    #################################################################
                }
            }

            #################################################################
            profileDF <- do.call('rbind', my.list)
            rownames(profileDF)<-seq(1,nrow(profileDF),1)

            #print(str(profileDF))
            #print(head(profileDF))
            #################################################################


            plotObj<-ggplot(profileDF)
            plotObj<-plotObj+facet_wrap(~category,ncol=1,as.table=F,scales="free_y")
            plotObj<-plotObj+geom_hline(aes(yintercept=meanProb))
            plotObj<-plotObj+geom_violin(aes(x=as.factor(cluster),y=prob,fill=as.factor(fillColor)),outlier.size=0.5)
            plotObj<-plotObj+geom_point(aes(x=as.factor(cluster),y=lowerProb,colour=as.factor(fillColor)),size=1.5)
            plotObj<-plotObj+geom_point(aes(x=as.factor(cluster),y=upperProb,colour=as.factor(fillColor)),size=1.5)
            plotObj<-plotObj+
                scale_fill_manual(values = c(high ="#CC0033",low ="#0066CC", avg ="#33CC66"))+
                scale_colour_manual(values = c(high ="#CC0033",low ="#0066CC", avg ="#33CC66"))+
                theme(legend.position="none")+labs(x="Cluster")+theme(axis.title.x=element_text(size=10))
            if(j==1){
                plotObj<-plotObj+labs(y="Probability")+theme(axis.title.y=element_text(size=10,angle=90))
            }else{
                plotObj<-plotObj+theme(axis.title.y=element_blank())
            }
            plotObj<-plotObj+labs(title=covNames[j],plot.title=element_text(size=10))
            plotObj<-plotObj+theme(plot.margin=unit(c(0.5,ifelse(j==nCovariates,1,0),0.5,ifelse(j==1,0.5,0)),'lines'))+
                theme(plot.margin=unit(c(0,0,0,0),'lines'))

            print(plotObj,vp=viewport(layout.pos.row=1:6,layout.pos.col=j+2))
        } else {
            # Plot the means
            # profileDF<-data.frame("mu"=c(),"cluster"=c(),"muMean"=c(),
            # 	"lowerMu"=c(),"upperMu"=c(),"fillColor"=c())
            #################################################################
            my.list <- vector('list', length(whichClusters))
            z=1
            #################################################################
            if (nContinuousCovs==1){
                muMat<-profileMu[,meanSortIndex,1]
            } else {
                muMat<-profileMu[,meanSortIndex,(j-nDiscreteCovs)]
            }
            muMeans<-apply(muMat,2,mean)
            muMean<-sum(muMeans*clusterSizes)/sum(clusterSizes)
            muLower<-apply(muMat,2,quantile,0.05)
            muUpper<-apply(muMat,2,quantile,0.95)
            # The next line is to avoid outliers spoiling plot scales
            plotMax<-max(muUpper)
            plotMin<-min(muLower)

            # Get the plot colors
            muColor<-ifelse(muLower>rep(muMean,length(muLower)),"high",
                            ifelse(muUpper<rep(muMean,length(muUpper)),"low","avg"))
            for(c in whichClusters){
                plotMu<-muMat[,c]
                plotMu<-plotMu[plotMu<plotMax&plotMu>plotMin]
                nPoints<-length(plotMu)
                # profileDF<-rbind(profileDF,data.frame("mu"=plotMu,"cluster"=rep(c,nPoints),
                # 	"meanMu"=rep(muMean,nPoints),
                # 	"lowerMu"=rep(muLower[c],nPoints),
                # 	"upperMu"=rep(muUpper[c],nPoints),
                # 	"fillColor"=rep(muColor[c],nPoints)))
                #################################################################
                my.list[[z]] <- data.frame("mu"=plotMu,"cluster"=rep(c,nPoints),
                                           "meanMu"=rep(muMean,nPoints),
                                           "lowerMu"=rep(muLower[c],nPoints),
                                           "upperMu"=rep(muUpper[c],nPoints),
                                           "fillColor"=rep(muColor[c],nPoints))
                z=z+1
                #################################################################

            }
            #rownames(profileDF)<-seq(1,nrow(profileDF),1)
            #################################################################
            profileDF <- do.call('rbind', my.list)
            rownames(profileDF)<-seq(1,nrow(profileDF),1)

            #print(str(profileDF))
            #print(head(profileDF))
            #################################################################
            plotObj<-ggplot(profileDF)
            plotObj<-plotObj+geom_hline(aes(yintercept=meanMu))
            plotObj<-plotObj+geom_violin(aes(x=as.factor(cluster),y=mu,fill=as.factor(fillColor)),outlier.size=0.5)
            plotObj<-plotObj+geom_point(aes(x=as.factor(cluster),y=lowerMu,colour=as.factor(fillColor)),size=1.5)
            plotObj<-plotObj+geom_point(aes(x=as.factor(cluster),y=upperMu,colour=as.factor(fillColor)),size=1.5)
            plotObj<-plotObj+
                scale_fill_manual(values = c(high ="#CC0033",low ="#0066CC", avg ="#33CC66"))+
                scale_colour_manual(values = c(high ="#CC0033",low ="#0066CC", avg ="#33CC66"))+
                theme(legend.position="none")+labs(x="Cluster")+theme(axis.title.x=element_text(size=10))
            if(j==1){
                plotObj<-plotObj+labs(y="Mean")+theme(axis.title.y=element_text(size=10,angle=90))
            }else{
                plotObj<-plotObj+theme(axis.title.y=element_blank())
            }
            plotObj<-plotObj+labs(title=covNames[j],plot.title=element_text(size=10))
            plotObj<-plotObj+
                theme(plot.margin=unit(c(0.5,ifelse(j==nCovariates,1,0),0.5,ifelse(j==1,0.5,0)),'lines'))+
                theme(plot.margin=unit(c(0,0,0,0),'lines'))

            print(plotObj,vp=viewport(layout.pos.row=1:3,layout.pos.col=j+2))

            # Plot the variances
            # profileDF<-data.frame("sigma"=c(),"cluster"=c(),"sigmaMean"=c(),
            # 	"lowerSigma"=c(),"upperSigma"=c(),"fillColor"=c())
            #################################################################
            my.list <- vector('list', length(whichClusters))
            z=1
            #################################################################
            if (nContinuousCovs==1){
                sigmaMat<-profileStdDev[,meanSortIndex,1,1]
            } else {
                sigmaMat<-profileStdDev[,meanSortIndex,(j-nDiscreteCovs),(j-nDiscreteCovs)]
            }
            sigmaMeans<-apply(sigmaMat,2,mean)
            sigmaMean<-sum(sigmaMeans*clusterSizes)/sum(clusterSizes)
            sigmaLower<-apply(sigmaMat,2,quantile,0.05)
            sigmaUpper<-apply(sigmaMat,2,quantile,0.95)
            # The next line is to avoid outliers spoiling plot scales
            plotMax<-max(sigmaUpper)

            # Get the plot colors
            sigmaColor<-ifelse(sigmaLower>rep(sigmaMean,length(sigmaLower)),"high",
                               ifelse(sigmaUpper<rep(sigmaMean,length(sigmaUpper)),"low","avg"))
            for(c in whichClusters){
                plotSigma<-sigmaMat[,c]
                plotSigma<-plotSigma[plotSigma<plotMax]
                nPoints<-length(plotSigma)
                # profileDF<-rbind(profileDF,data.frame("sigma"=plotSigma,"cluster"=rep(c,nPoints),
                # 	"meanSigma"=rep(sigmaMean,nPoints),
                # 	"lowerSigma"=rep(sigmaLower[c],nPoints),
                # 	"upperSigma"=rep(sigmaUpper[c],nPoints),
                # 	"fillColor"=rep(sigmaColor[c],nPoints)))
                #################################################################
                my.list[[z]] <- data.frame("sigma"=plotSigma,"cluster"=rep(c,nPoints),
                                           "meanSigma"=rep(sigmaMean,nPoints),
                                           "lowerSigma"=rep(sigmaLower[c],nPoints),
                                           "upperSigma"=rep(sigmaUpper[c],nPoints),
                                           "fillColor"=rep(sigmaColor[c],nPoints))
                z=z+1
                #################################################################
            }
            # rownames(profileDF)<-seq(1,nrow(profileDF),1)
            #################################################################
            profileDF <- do.call('rbind', my.list)
            rownames(profileDF)<-seq(1,nrow(profileDF),1)

            #print(str(profileDF))
            #print(head(profileDF))
            #################################################################

            plotObj<-ggplot(profileDF)
            plotObj<-plotObj+geom_hline(aes(yintercept=meanSigma))
            plotObj<-plotObj+geom_violin(aes(x=as.factor(cluster),y=sigma,fill=as.factor(fillColor)),outlier.size=0.5)
            plotObj<-plotObj+geom_point(aes(x=as.factor(cluster),y=lowerSigma,colour=as.factor(fillColor)),size=1.5)
            plotObj<-plotObj+geom_point(aes(x=as.factor(cluster),y=upperSigma,colour=as.factor(fillColor)),size=1.5)
            plotObj<-plotObj+
                scale_fill_manual(values = c(high ="#CC0033",low ="#0066CC", avg ="#33CC66"))+
                scale_colour_manual(values = c(high ="#CC0033",low ="#0066CC", avg ="#33CC66"))+
                theme(legend.position="none")+labs(x="Cluster")+theme(axis.title.x=element_text(size=10))
            if(j==1){
                plotObj<-plotObj+labs(y="Std Dev")+theme(axis.title.y=element_text(size=10,angle=90))
            }else{
                plotObj<-plotObj+theme(axis.title.y=element_blank())
            }
            plotObj<-plotObj+
                theme(plot.margin=unit(c(0.5,ifelse(j==nCovariates,1,0),0.5,ifelse(j==1,0.5,0)),'lines'))+
                theme(plot.margin=unit(c(0,0,0,0),'lines'))

            print(plotObj,vp=viewport(layout.pos.row=4:6,layout.pos.col=j+2))
        }


}
