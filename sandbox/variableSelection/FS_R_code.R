#           -----------------------------------

#               Feature Selection in R
#
#              Houtao Deng hdeng3@asu.edu

#           -----------------------------------


library(RRF)
library(FSelector)
library(varSelRF)
library(glmnet)
library(RWeka)
set.seed(1)

#1: linear case; 2: nonlinear case; 3: XOR case
flag=2


#data simulation
#only the first and the 21th feature are needed
nCol = 100
X=matrix(runif(400*nCol, min=-2, max=2), ncol=nCol)


#linear case
if(flag==1)
{class = (X[,1]) + (X[,21])
ix=which(class>quantile(class, 1/2));
class = class*0-1; class[ix]=1}

#nonlinear case
if(flag==2){
class = (X[,1])^2 + 1*(X[,21])^2
ix=which(class>quantile(class, 6/10));
ix=c(ix,which(class<quantile(class, 1/10)));
class = class*0-1; class[ix]=1}

#plot
if(flag==1|flag==2){
ix = which(class==1)
X11();
plot(X[ix,1],X[ix,21],col="blue",pch=1,
xlim=c(-3,3),ylim=c(-3,3),xlab="Variable 1",ylab="Variable 2")
ix = which(class==-1)
points(X[ix,1],X[ix,21],pch=3,col="red")
legend("topright",legend=c("class 1","class 2"),
col=c("blue","red"), pch=c(1,3))}

#XOR case
if(flag==3){
bSample = sample(0:1,400*nCol,replace=TRUE)
X=matrix(bSample,ncol=nCol)
class = (xor(X[,1],X[,21]))
}

#duplicate
#X[,1:20]=X[,1];X[,21:40]=X[,21];

data = data.frame(cbind(X,class));data[,"class"]=as.factor(data[,"class"])

listFea = list()

#Chisquare
weights <- chi.squared(class~., data)
subset <- cutoff.k(weights, 5)
subset=c("Chi-Square Top 5",paste(subset))
listFea[[length(listFea)+1]]=subset

#Information Gain
weights=information.gain(class~., data)
subset <- cutoff.k(weights, 5)
subset=c("Information Gain Top 5",paste(subset))
listFea[[length(listFea)+1]]=subset


#CFS from FSelector
subset <- cfs(class~., data)
subset=c("CFS-FSelector",paste(subset))
listFea[[length(listFea)+1]]=subset

#--- CFS from RWeka
nombi=make_Weka_filter("weka/filters/supervised/attribute/AttributeSelection")
datbin <- nombi(class ~., data=data, control =Weka_control(
E="weka.attributeSelection.CfsSubsetEval ",
S="weka.attributeSelection.BestFirst -D 1 -N 5"
))
CFSRweka=colnames(datbin)
CFSRweka=c("CFSRweka",paste(CFSRweka))
listFea[[length(listFea)+1]]=CFSRweka

#LASSO
cvob1=glmnet(X,as.factor(class),family="binomial",lambda=0.1,alpha=1)
coef=coef(cvob1)
coef=which(coef>0.001)-1;coef=coef[-(which(coef==0))]
coef=c("LASSO",paste("V",coef,sep=""))
listFea[[length(listFea)+1]]=coef

#RF-RFE Ignore the warning here.
RFE=varSelRF(X,as.factor(class), c.sd = 1, mtryFactor = ncol(X), ntree = 500,
 vars.drop.num = NULL, vars.drop.frac = 0.2,
whole.range = TRUE, recompute.var.imp = FALSE, verbose = FALSE,
returnFirstForest = TRUE, fitted.rf = NULL, keep.forest = FALSE)
RFEFS=RFE$selected.vars;
RFEFS=c("RF-RFE",RFEFS)
listFea[[length(listFea)+1]]=RFEFS

#----RRF---
#ordinary random forest.
rf <- RRF(X,as.factor(class), flagReg = 0,importance=TRUE)
impRF=rf$importance
impRF=impRF[,"MeanDecreaseAccuracy"]
imp=impRF/(max(impRF))#normalize the importance score
coefReg=0.9*0.8+0.0*imp #weighted average
rrf <- RRF(X,as.factor(class),coefReg=coefReg,mtry=ncol(X),importance=TRUE)
imp=rrf$importance
imp=imp[,"MeanDecreaseAccuracy"]
FS_RRF = which(imp>0)
FS_RRF=c("RRF",paste("V",FS_RRF,sep=""))
listFea[[length(listFea)+1]]=FS_RRF


print(listFea)
