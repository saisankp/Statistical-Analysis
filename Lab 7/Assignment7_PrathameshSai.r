
# QUESTION 1: Check the correlation between the variables eruptions and waiting

head(faithful) # Inspect the start of the dataset.
cor(faithful$eruptions, faithful$waiting) # Check the correlation between eruptions and waiting.

# QUESTION 2: Create a plot that shows both the points and a smoothed line of the points 

plot(faithful$eruptions,faithful$waiting) # Create a plot showing both the points.
scatter.smooth(x=faithful$eruptions, y=faithful$waiting, main="Waiting ~ Eruptions") # Create a smoothed line of the points.

# QUESTION 3: Create box plots for the variables

par(mfrow=c(1, 2))  # Divide the graph area into two columns.
boxplot(faithful$eruptions, main="Eruptions", sub=paste("Outlier rows: ", boxplot.stats(faithful$eruptions)$out))  # Box plot for 'Eruptions'.
boxplot(faithful$waiting, main="Waiting", sub=paste("Outlier rows: ", boxplot.stats(faithful$waiting)$out))  # Box plot for 'Waiting'.

# QUESTION 4:	Create graphs of the densities of the variables

par(mfrow=c(1, 2))  # Divide the graph area into 2 columns
plot(density(faithful$eruptions), main="Density Plot: Eruptions", ylab="Frequency")  # Density plot for 'Eruptions'.
plot(density(faithful$waiting), main="Density Plot: Waiting", ylab="Frequency") # Density plot for 'Waiting'.

# QUESTION 5: Fit a simple linear model that predicts eruptions from waiting
faithful.lm <- lm(waiting ~ eruptions, data=faithful)  # Build a linear regression model on the full data. (formula = waiting ~ eruptions)
print(faithful.lm) # Print it out.

summary(faithful.lm) # Inspect the results

# Answering Question 7 in comments :
# The syntax for building a linear model in R is lm(formula, data = ?).
# In this case, the formula is (waiting ~ eruptions).
# waiting is the dependent variable, and eruptions is the independent variable.
# Hence, writing an equation that describes the linear model that I have fitted would be :
# formula = waiting ~ eruptions


# QUESTION 6: Visualise the resulting regression line on a scatterplot of the data
plot(faithful$eruptions, faithful$waiting)  # Create a plot showing both the points.
abline(faithful.lm) #Visualise the resulting regression line on a scatterplot of the data

# QUESTION 7:	In comments in your code, write an equation that describes the linear model you have fitted


# y-hat = B0-hat + B1-hat(x) (that is the sample regression line, y-hat is predicted value of y)
# B0 = y-intercept
# B1 = slope

# B1-hat = r * (Sy/Sx) where:

# r = the correlation between x & y 
# Sy = standard deviation of y
# Sx = standard deviation of x

standDevX <- sd(faithful$eruptions)
standDevY <- sd(faithful$waiting)
B1 = (correlation) * standDevY/standDevX
print(B1) # print it out

# B0-hat = y-bar - B1(x-bar), where 
# x-bar = mean of x values 
# y-bar = mean of y values

x_bar <- mean(faithful$eruptions)
y_bar <- mean(faithful$waiting)
B2 = (y_bar) - B1*(x_bar)
print(B2) # print it out 

# (beta 0 hat) B0-hat = 33.4744 
# (beta 1 hat) B1-hat = 10.72964
# Henceforth, we can deduce that :
# y-hat = 33.4744 - 10.72964(x)

# Question 8 : At a significance level of 0.05, does there appear to be a statistically significant relationship between eruptions and waiting? Explain your answer in comments in your code. 
# H0 (Null Hypothesis) :  there appears to NOT be a statistically significant relationship between eruptions and waiting. (population slope is equal to 0).
# H1 (Alternate Hypothesis) : there appears to be a statistically significant relationship between eruptions and waiting. (population slope is not equal to 0).
#   (taking a significance level of 0.05)

# If p< 0.05, p is statistically significant
# if p >0.05, p is not statistically significant


# p-value = 2.2e-16
# Hence,  p< .001 - sufficient evidence to reject H0 , and p < 0.05 so p is statistically significant. This indicates strong evidence against the null hypothesis H0.
# Thus, we reject H0 (there appears to NOT be a statistically significant relationship between eruptions and waiting)
# So, in conclusion, we accept H1, that there appears to be a statistically significant relationship between eruptions and waiting.
# Hence, we should include include eruptions.

# Question 9 :	Why are the p-values for the variable waiting and the overall F test so similar for this model?

# The F-statistic has to change depending on the number of variables in the model and the also depending on whether the fit is good or not.
# The F test is a test of the significance of the model overall, and it tests the null hypothesis (H0) so that a model with only the 
# intercept considered fits the data as well as the overall model we have considered.
# We have also concluded in question 6 that that there appears to be a statistically significant relationship between eruptions and waiting.
# But, since there is only one variable, the p-values and the overall f-test are similar because we are essentially testing the same thing:
# i.e whether or not the variable waiting adds to the model or whether the model would be better off with only the intercept.



