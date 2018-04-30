# simulation of large dataset

# dataset created has Normal outcome and Mixed covariates

# see parameters below used in the creation of the dataset
# these can be changed
###############
###############
nClusters<-30 #(number of clusters)
nSizes<-500 # size of the clusters (all clusters are of equal size)
nContCovs<-30 # number of continuous covariates
nDiscrCovs<-30 # number of discrete covariates
###############
###############

nCovariates<-nContCovs+nDiscrCovs

dataMatrix<-matrix(NA,nrow=nClusters*nSizes,ncol=c(1+nContCovs+nDiscrCovs))

# continuous covariates 
sigmaSq<-0.3
for (i in 1:nContCovs){
  centresCont<-sample(1:10,nContCovs,replace = TRUE)
  for (j in 1:nClusters){
    dataMatrix[c((j-1)*nSizes+1):c(j*nSizes),c(1+i)]<-rnorm(nSizes,centresCont[j],sigmaSq)
  }
}

# discrete covariates
sampleProbs<-c(0.1,0.3,0.5,0.2)
for (i in 1:nDiscrCovs){
  for (j in 1:nClusters){
    catProbs<-sample(sampleProbs)
    dataMatrix[c((j-1)*nSizes+1):c(j*nSizes),c(1+nContCovs+i)]<-
      sample(length(catProbs), nSizes, replace = T, prob = catProbs)-1
  }
}

# outcome 
sigmaSq<-0.3
for (j in 1:nClusters){
  centresOutcome<-sample(20:30,1,replace = TRUE)
  dataMatrix[c((j-1)*nSizes+1):c(j*nSizes),1]<-rnorm(nSizes,centresOutcome,sigmaSq)
}

datasetFinal<-data.frame(dataMatrix)
colnames(datasetFinal)<-c("outcome",paste("Cont",1:nContCovs,sep=""),paste("Discr",1:nDiscrCovs,sep=""))

write.table(datasetFinal,"sim_large_dataset_NormalMixed.dat",row.names = FALSE, quote =FALSE)
