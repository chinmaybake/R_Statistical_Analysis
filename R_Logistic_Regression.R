#you will have to use these import statements after you create the csv files
xtrain<-read.table(file="C:/Texas State University/xtrain.csv",header=TRUE,sep=",")
ytrain<-read.table(file="C:/Texas State University/ytrain.csv",header=TRUE,sep=",")
xtest<-read.table(file="C:/Texas State University/xtest.csv",header=TRUE,sep=",")
ytest<-read.table(file="C:/Texas State University/ytest.csv",header=TRUE,sep=",")


attach(xtrain)
attach(ytrain)
#R will need to recognize these as character variables
#we used to do this with the factor function but
#since the predict new data function will cause problems we need to
#do the conversion before
ytrain=ytrain-1
for(i in 1:ncol(xtrain)){
  xtrain[,i]=as.character(xtrain[,i])
}
xtrain$ApplicantIncome=as.numeric(xtrain$ApplicantIncome)
xtrain$CoapplicantIncome=as.numeric(xtrain$CoapplicantIncome)
xtrain$LoanAmount=as.numeric(xtrain$LoanAmount)
xtrain$Loan_Amount_Term=as.numeric(xtrain$Loan_Amount_Term)

o4<-glm(ytrain$Loan_Status~. ,data=xtrain,family=binomial(link="logit"))
o5<-glm(ytrain$Loan_Status~Dependents+Credit_History+Property_Area+Married ,data=xtrain,family=binomial(link="logit"))
#####change the values to character in the test data as well
ytest=ytest-1
for(i in 1:ncol(xtest)){
  xtest[,i]=as.character(xtest[,i])
}

xtest$ApplicantIncome=as.numeric(xtest$ApplicantIncome)
xtest$CoapplicantIncome=as.numeric(xtest$CoapplicantIncome)
xtest$LoanAmount=as.numeric(xtest$LoanAmount)
xtest$Loan_Amount_Term=as.numeric(xtest$Loan_Amount_Term)

predict(o4,newdata = xtest,"response")
predict(o5,newdata = xtest,"response")
ytestpred=(predict(o4,newdata = xtest,"response")>0.5)*1
sum(ytest==ytestpred)/nrow(ytest)

#######Now use the optimized accuracy

library(InformationValue)
optCutOff <- optimalCutoff(ytest, predict(o4,newdata = xtest,type="response"))
optCutOff <- optimalCutoff(ytest, predict(o5,newdata = xtest,type="response"))
ytestpred=(predict(o4,newdata = xtest,"response")>optCutOff)*1


threshold=0.5
predicted_values<-ifelse(predict(o4,type="response")>threshold,1,0)
actual_values<-o4$y
conf_matrix<-table(predicted_values,actual_values)

library(caret)
print(paste("Logistic Regression Accuracy is: ", sum(ytest==ytestpred)/nrow(ytest)))
sensitivity(conf_matrix)
specificity(conf_matrix)
