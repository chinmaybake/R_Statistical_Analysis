# Chinmay Bake
# 10/13/2020
# Assignment 2: North Carolina Births

# 1) Read the dataset into R.

#SOlution -> 

library(readr)
url <- "https://www.openintro.org/data/csv/ncbirths.csv"
a<-read_csv(file = url)

head(a) 

# 2) Create a contingency table for these two variables: habit and premie. Round the probabilities to four
# decimal places.

# Solution -> 

round(addmargins(prop.table(table("Habit"=a$habit, "Birth_Term"=a$premie))),digits = 4)

# 3) Conduct a test of significance (level of 0.01) to assess the relationship between 
# a woman smoking habits during pregnancy and having a premature baby or not.

# What are the hypotheses? Express using properly constructed hypothesis statement

# Solution -> 
# Statement: At a level of 0.01 significance, is there an association between a woman smoking
# during pregnancy and subsequently having a premature baby? 
# Null Hypothesis: A woman smoking during pregnancy and having a premature baby are independent 
# of each other. 
# Alternative Hypothesis: A woman smoking during pregnancy and having a premature baby are 
# are two factors which are dependent on each other. 

# Select the appropriate test

# Solution -> We are trying to evaluate a possible association between two categorical variables.
# Hence we utilize the Pearson Chi Square Test to perform the test of independence. 

# Conduct the test 

#Solution -> 
chisq.test(x=a$habit,y=a$premie,correct=FALSE)

# 4) Based on the results from the test in #3, what do statistical evidence suggest about the 
# relationship. Provide an explanation of the meaning of the p-value in this case. 

# Solution -> 
# Result - Do not reject the Null Hypothesis
# At 0.01 level of significance, we do not have sufficient evidence to conclude that a woman 
# smoking during pregnancy and having a premature baby are dependent. The P-value 
# of 0.9597 in this case implies that there is a 96% chance of finding Chi-square value 
# greater than the one which we have computed, given the variables are independent. 

# 5) Conduct a test of significance (level of 0.01) to assess the difference in weight of babies from women
# who smoked during pregnancy and those who did not.

# What are the hypotheses? Express using properly constructed hypothesis statement

# Solution -> 
# Statement: At level of 0.01 significance, can we conclude that the population averages for 
# for baby weights from women who smoked and who did not smoke during pregnancy are not the same?
# Null Hypothesis: The average for women who smoked is the same as the average for 
# women who did not smoke (MUs = MUn)
# Alternative Hypothesis: The average for women who smoked is not the same as the average for 
# women who did not smoke (MUs not equal to MUn)

# Select the appropriate test

# Solution -> We would be utilizing the Two Sample t test. 

# Conduct the test

# Solution -> 
t.test(x$smoker,x$nonsmoker, alternative = "two.side")

# 6) Based on the results from the test, what do statistical evidence suggest about the the difference in
# birth weights. Provide an explanation of the meaning of the p-value in this case;

# Solution->
# Result- Do not reject the Null Hypothesis
# At 0.01 level of confidence, we do not have enough evidence to suggest that the average 
# weight for babies from mothers who smoked during pregnancy is not exactly equal to average 
# weight for babies from mothers who did not smoked during pregnancy. The P value of 0.01945 
# is the probability of drawing a random average birth weight value, beyond our computed
# t values, given that Null hypothesis condition is true.


