# Stats Method Final Project 
# Chinmay Bake & Venkata Sowjanya Koka

#Reading the dataset into R 
Game <- read.table(file="cookie_cats.csv", sep = ",",header = T,
                colClasses = c("numeric","factor","numeric","factor",
                               "factor"))
head(Game)

#Calculating number of unique user id's

UniqueUserID <- unique(Game$userid); length(UniqueUserID)

#Displaying number of unique ID's

length(UniqueUserID)

#Splitting the data by versions

versions = split(Game,Game$version)
Data_Gate30 <- versions$gate_30
Data_Gate40 <- versions$gate_40

#Displaying summary statistics of gate_40

summary(Data_Gate30)

#Displaying summary statistics of gate_40

summary(Data_Gate40) 


# Finding count and Plotting One Day Retention

Seven_Day_Retention_Counts <- table(Data_Gate30$retention_7)

barplot(Seven_Day_Retention_Counts, main="Seven Day Retention Gate 30",
        ylab="Number of Users")


# Finding Count and Plotting Seven Day Retention

Seven_Day_Retention_Counts_40 <- table(Data_Gate40$retention_7)

barplot(Seven_Day_Retention_Counts_40, main="Seven Day Retention Gate 40",
        ylab="Number of Users")


#Explanatory Logistic Regression Model

logm1<-glm(retention_7 ~ sum_gamerounds + version,
           family=binomial("logit"),
           data=Game)

summary(logm1)

#predictions for generating a probability of retention vs version plot
pred<-predict(logm1, data=Game, type = "response")

#Probability of retention vs version plot
plot(Game$version, pred, #pred = logm1$fit,
type="p", pch=16,
     xlab="Version",
     ylab="Probability of Retention",
     ylim = c(-0.1, 1.10))


#Predictive Model

#Train test split. Split raio: 80:20
trainIndex <- sample(x = 1:length(Game$retention_7),
                     size = round(0.80*length(Game$retention_7),0))

#train data and test data
train<-Game[trainIndex,]
test<-Game[-trainIndex,] 

#Predictive model training 
logm2=glm(retention_7 ~ sum_gamerounds + version,
          family=binomial("logit"),
          data=train)

summary(logm2)

#Prediction with test data
pred1<-predict(logm2, data=test, type = "response")

#threshold probability set to 0.5
pred1 <- ifelse(pred1 > 0.5,TRUE,FALSE)

missed <- mean(pred1 != test$retention_7)
#print accuracy
print(paste('Accuracy',1-missed))

# Evaluate if accuracy improves with a new predictor variable 

logm3=glm(retention_7 ~ sum_gamerounds + version + retention_1,
          family=binomial("logit"),
          data=train)

#new model with additional predictor
summary(logm3)

pred2<-predict(logm3, data=test, type = "response")

pred2 <- ifelse(pred2 > 0.5,TRUE,FALSE)

missed <- mean(pred2 != test$retention_7)
#accuracy for the new model
print(paste('Accuracy',1-missed))


######End#######