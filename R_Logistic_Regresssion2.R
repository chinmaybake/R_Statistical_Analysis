xtrain<-read.table(file="C:/Texas State University/xtrain.csv",header=TRUE,sep=",")
ytrain<-read.table(file="C:/Texas State University/ytrain.csv",header=TRUE,sep=",")
xtest<-read.table(file="C:/Texas State University/xtest.csv",header=TRUE,sep=",")
ytest<-read.table(file="C:/Texas State University/ytest.csv",header=TRUE,sep=",")


attach(xtrain)
attach(ytrain)

xtrain$sex=as.character(xtrain$sex)
xtrain$pclass=as.character(xtrain$pclass)

o4<-glm(survived~sex+pclass,data=xtrain,family=binomial(link="logit"))

xtest$sex=as.character(xtest$sex)
xtest$pclass=as.character(xtest$pclass)

predict(o4,newdata = xtest,"response")

ytestpred=(predict(o4,newdata = xtest,"response")>0.5)*1
sum(ytest==ytestpred)/nrow(ytest)


library(InformationValue)
optCutOff <- optimalCutoff(ytest, predict(o4,newdata = xtest,type="response"))

ytestpred=(predict(o4,newdata = xtest,"response")>optCutOff)*1
sum(ytest==ytestpred)/nrow(ytest)