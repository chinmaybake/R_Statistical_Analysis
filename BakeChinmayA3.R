# Chinmay Bake
# 11/5/2020
# Assignment 3: North Carolina Births


#1) Read the dataset into R.

# Solution-> 

library(readr)
url <- "https://www.openintro.org/data/csv/ncbirths.csv"
a<-read_csv(file = url)

head(a) 


#2) Create a new variable, netgain = gained - weight, call it netgain and make it part of the dataset

# Solution -> 

a$netgain<-(a$gained - a$weight); a$netgain

#3) Estimate the following model and display the summary results

# Solution -> 

m1<-lm(formula=weight~weeks+mage+netgain, data=a)
summary(m1)

#4) What is the value and the meaning of the slope of Weeks? Explain the implication of the value of the coefficient, and the implication (not the meaning) 
#of the p-value. Three sentences suffice.

# Solution -> 

# Value of slope of week is 0.342249 and it means that 'weeks' is 
# positively correlated with 'weight', greater the weeks greater the 
# weight. Also it implies that, if the 'weeks' increases by 1 unit, the 
# weight would increase by 0.342249 pounds. The P value implies that 
# 'weeks' is a significant predictor and we have sufficient evidence 
# to conclude that its slope is not equal to zero.

#5) What is the meaning of the p-value for the slope coefficient of mage?

# Solution ->

# The p value of 0.000617 means that there is a 0.0617% chance
# of finding a t-value beyond the computed t-value of 3.435. 


#6) Assess the goodness of fit of this model to the data? 

# Solution -> 

summary(m1)

# The model has a Root Mean Square Error of 1.111, which implies that
# that there is an average error of 1.111 pounds while predicting the 
# value of weight. An R-Squared of 0.4473 indicate that changes in 
# 'weeks','mage' and 'netgain' together explain 45% change in the 'weight'. 

#7) Briefly, analyze the residuals and assess the Gauss-Markov Theorem 
# and Normality

# Solution -> 

# 1.Mean Residual - 

mean(resid(m1))

# Our residuals are very close to the actual value of zero and hence 
# they confirm mean-residual test in the Gauss Markov Theorem. 

# 2.Heteroscedasticity

library(lmtest)
bptest(formula=m1)

# The obtained P value is not very significant indicating that there
# there is no evidence of heteroscedasticity in our model. 

# 3.Serial Correlation

dwtest(formula=m1,alternative="two.sided")

# The value of Durbin Watson test is almost equal to 2 and hence 
# there is no first order autocorrelation detected. 

# 4.Normality of Residuals  

hist(m1$residuals)

library(car)
qqPlot(x=m1$residuals)

shapiro.test(m1$residuals)

# The histogram appears to be fairly symmetric, the QQ plot has 
# certain points which have departed from the straight line, assessing 
# the P-value from the Shapiro-Wilk test although suggest that the Null 
# Hypothesis should not be rejected and we can conclude that the residuals
# are normally distributed. 