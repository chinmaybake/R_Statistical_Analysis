# Chinmay Bake 
# 11/24/2020
# Assignment 4: ANOVA

#----------------------------------------------------------------------------------#

# 0) Read the dataset. Make sure that the variable "Area" is a factor.

# SOlution -> 

a <- read.table(file="assessed_value.csv", sep = ",",header = T,
                colClasses = c("numeric","numeric","factor","factor"))

head(a)

#----------------------------------------------------------------------------------# 

# 1) Formulate an ANOVA model

# Solution -> 

m1 <- lm(Assess_year_2~Area,data=a)

summary(m1)

#-----------------------------------------------------------------------------------# 

# 2) Based on the results, is there evidence of a difference in mean assessed property value for
# the various areas in Wisconsin

# Solution -> 

# The F statistic or the F ratio is significantly larger than 1, 
# thus indicating that the 'between' variability is greater than the 'within' 
# variability and, therefore it is evident that there is a difference among mean 
# assessed property values for different areas. The P value of 0.01184, is the chance
# of finding an F static value beyond the computed f statistic.The P value being 
# very small and highly significant, we can conclude from it that, there is evidence
# of at least one area's mean assessed property value being different from some
# of the other or any of the other areas. 

#-------------------------------------------------------------------------------------#

# 3) Conduct a post-hoc analysis and display the results

# Solution ->

library("multcomp")
postHocs<-glht(aov(m1), linfct = mcp(Area = "Tukey"))

summary(postHocs)

confint(postHocs)

#--------------------------------------------------------------------------------#

# Which areas have statistically significant different mean assessed values?

# Solution -> 

# Statistically Significant differences -> 'far west - east central' and 'near east - far west'. 
# East central and near east areas have their means significantly 
# different from the mean of far west area. 

# Marginally significant differences-> 'far west - far east', 'near west - far west', 'west central-far west'
# Far east, near west and west central have their means marginally
# different from the mean of the far west area. 

#--------------------------------------------------------------------------------# 

# Which areas seem to have greater assessed property values?

# Solution -> 

# Areas seemingly having greater assessed property values ->
# Far west, southwest, far south, near north

#----------------------------------------------------------------------------------# 

# 4) Briefly, analyze the residuals and assess the Gauss-Markov Theorem 
# and Normality of residuals -> 

# 1.Mean Residual - 

mean(resid(m1))

# The obtained residuals are very close to the actual value of zero and hence 
# they confirm mean-residual test in the Gauss Markov Theorem. 

# 2.Heteroscedasticity

library(lmtest)
bptest(formula=m1)

# The obtained P value is not very significant indicating that there
# there is no evidence of heteroscedasticity in our model. 

# 3.Serial Correlation

dwtest(formula=m1,alternative="two.sided")

# The p value returned in the Durbin-Watson test is not very significant, 
# thus indicating that there is no evidence of first order serial auto-correlation.

# 4.Normality of Residuals

hist(m1$residuals)

library(car)
qqPlot(x=m1$residuals)

shapiro.test(m1$residuals)

# The histogram of the residuals is positively skewed and does not appear to be 
# symmetric, the Q-Q plot has a lot of data points departing from the straight line 
# and the Shapiro-Wilk test returns a very small P value, all this essentially 
# suggesting that the residuals are not normally distributed. 

#--------------------------------------------------------------------------------#

