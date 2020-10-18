tbl <- table(survey$Smoke, survey$Exer)
chisq.test(tbl)
tbl

# Results of the Chi-squared test
# x^2 = 5.4885
# df = 6
# p-value = 0.4828

# QUESTION 1 : How were the degrees of freedom for this test calculated?


# The degrees of freedom is calculated with the following formula:
# df = (no. of rows - 1)(no. of columns -1)
# We are doing a chi-squared test on tbl, which does a table of (survey$Smoke, survey$Exer).
# On R Studio, if you go into the 'Enviroment' tab , and then into the 'values' subsection, the table
# consists of integers, and has 4 rows and 3 columnns, denoted by int [1:4, 1:3].

# Hence, for 'tbl', it has 4 rows and 3 columns.
# Following the degrees of freedom formula:
# df = (no. of rows - 1)(no. of columns -1)
# df = (4 - 1)(3 -1)
# df = 6
# This is correct since it was also equivalent to our result above from running the code.


# QUESTION 2: What are the assumptions of the chi square test?

# The assumptions of the chi-squared test are that:
# 1. All observations must contribute to only one cell.
# 2. If it's a 2x2 table, all expected values must be greater than 5. (not relevant for this question)
# 3. In a larger table (as relevant here), N must be greater than 2, expected values must be greater than 1
#    and no more than 20% or 0.2 of the expected values can be less than 5.


# QUESTION 3: Why did the code give an error message?

# We get the error message "In chisq.test(tbl), Chi-squared approximation may be incorrect"
# So Why does this occur?

#To answer this question, we must get the expected values. 
# To get the expected values: 
 question3 <- chisq.test(tbl) #Make a vector of the chi-squared test into 'question3' as an example.
 question3$expected #get the expected values

# Here are the expected values using that code:

#         Freq      None      Some
#Heavy  5.360169  1.072034  4.567797
#Never 92.097458 18.419492 78.483051
#Occas  9.258475  1.851695  7.889831
#Regul  8.283898  1.656780  7.059322

 
# It gave that warning because many of the expected values are very small, henceforth the approximations of p may not be right.

 #From our table, there are 4 expected values that are less than 5. 
 # 4/12 (total) = 0.33 or 33% of our expected values are less than 5.
 
 # However, from our assumptions, no more than 20% or 0.2 of the expected values can be less than 5.
 # This goes against our assumptions
 # Therefore the approximations of the p-values may not be right in the chisq.test function.
 
 
 # QUESTION 4 : What is the null hypothesis for this test and the alternative hypothesis for this test?
 
 # The survey asked people how often do you smoke and how often do you exercise.
 # H0: There is no association between Smoke and Exercise (null hypothesis)
 # H1 : There is an association between Smoke and Exercise (alternative hypothesis)
 
 #QUESTION 5: Using a significance level of 0.05, give your conclusions for the result of this test.
 
 # The P-Value is 0.4828 from the chi squared test
 # df = 6 as pointed out earlier
 # Alpha = 0.05 (given in the question)
 # Since 0.4828 > 0.05, we fail to reject H0. - we do not have evidence to accept H1 that there is an association between Smoke and Exercise.
 