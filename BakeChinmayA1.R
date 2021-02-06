# Chinmay Bake
# 09/19/2020
# Assignment 1: North Carolina Births

# 1) Read the dataset into R.

# Solution->

library(readr)
url <- "https://www.openintro.org/data/csv/ncbirths.csv"
a<-read_csv(file = url)

head(a) 



# 2) Create a contingency table for these two variables. Round the
# probabilities to four decimal places

# Solution->

round(addmargins(prop.table(table("Habit"=a$habit, "Birth_Term"=a$premie))),digits = 4)



# 3) Based on the table of contingency from part 2, what is the probability that a woman who smokes
# during pregnancy will give birth prematurely?

# Solution->   P(premie|smoker) = P(premie and smoker)/P(smoker)

0.0190/0.1263


# 4) Based on the table of contingency from part 2, what is the probability that a woman who does
# not smoke during pregnancy will give birth prematurely?

# Solution-> P(premie|nonsmoker) = P(premie and nonsmoker)/P(nonsmoker)

0.1333/0.8737


# 5) Compare the likelihood of giving birth prematurely, for the smoker versus the non-smoker. As a
# comment in your code, provide the odds ratio and interpret in one sentence.

# Solution-> Odds Ratio = P(premie|nonsmoker)/P(premie|smoker)

0.1525695/0.1504355

# Interpretation- A woman who is a nonsmoker has almost equal likelihood 
#                 of getting a premature baby as compared to a woman 
#                 who is a smoker.




# 6) Having established the comparison from part 5 as a basis. Now compare the likelihood of lowbirthweight,
# for a mother who smokes versus a mother who does not. As a comment in your code, provide
# the odds ratio and its interpretation.

# Solution-> Odds Ratio = P(bw=low|nonsmoker)/P(bw=low|smoker)

round(addmargins(prop.table(table("Weight"=a$lowbirthweight,"Habit"=a$habit))),digits = 4)

(0.0921/0.8739)/(0.0180/0.1261)

# Interpretation- A woman who is a nonsmoker has a very less likelihood
#                 (0.73 times) of giving birth to a baby with low 
#                 birth weight as compared to woman who is a smoker.



# 7) Create a boxplot that compares the distribution of birth weights for Low/Not Low Birthweights. The
# plot should look as close as possible to the one shown below. Research on your own what a "normal"
# birthweight should be. Provide your sources

# SOlution-> Any birth weight between 5.5lb to 10lb is considered as normal.
#            Source- https://www.uofmhealth.org/health-library
           

boxplot(weight ~ lowbirthweight,data=a, main="Distribution of birth weights",
        xlab="Baby Weight (lbs)",ylab="Birth Weight",
        horizontal = TRUE)


# 7a. Based on the plot, what seems to be the skewness for the distributions of weight at
# birth, for "low" and "not low"?. A one word answer suffices

# Solution-> For birth weight low - Negatively Skewed
#            For birth weight not low - Uniform/Symmetric


# 7b. Does the classification for birth weight that you found agree with "North Carolina
# births" (2004) data? Explain in one sentence.

# Solution-> Yes, it agrees; values for category 'low' has smaller 
#   numbers in its range from approx. 0.2 to 5.8 lbs whereas category   
#   'not low' has bigger numbers in its range from approx. 6 to 12 lbs.
 

# 8) Create a boxplot that compares the distribution of weights by baby gender. Does it seems as if the
# gender of the baby makes a difference in weight at birth? Explain from your boxplot in
# one sentence.

# Solution-> From the box plot, a higher median and maximum value 
#            indicate that male babies could be slightly heavier than 
#            female babies.

boxplot(weight ~ gender,data=a, main="Distribution of birth weights by gender",
        xlab="Baby Weight (lbs)",ylab="Gender",
        horizontal = TRUE)



