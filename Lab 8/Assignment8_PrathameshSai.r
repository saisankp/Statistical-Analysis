# Statistics Lab 8.

# QUESTION 1:	
# Goal: Plot the residuals density

#fit our linear model as follows:
faithful.lm <- lm(waiting ~ eruptions, data=faithful)  # Build a linear regression model on the full data. (formula = waiting ~ eruptions)
print(faithful.lm) # Print it out.
faithful.res <- resid(faithful.lm) # We can compute the residuals

# We can plot the residuals against the observed values
plot(faithful$waiting, faithful.res, 
     ylab="Residuals", xlab="Waiting", 
     main="Faithful's Linear Model") 
abline(0, 0)                  # the horizon

# Inspect the residuals density
plot(density(faithful.res), main="Density Plot: residuals", ylab="Frequency") 

# QUESTION 2:	
# goal : Use the plot function to generate 4 graphs of the residuals vs fitted values, etc. in a single plot
#We can also see a number of other plots with a single command
par(mfrow=c(2,2)) # Divide the plot into 4 sections (i.e 2 columns and 2 rows.)
plot(faithful.lm) #Plot a graph of the residuals vs fitted values.

# QUESTION 3:	
# goal: Comment on whether or not you feel the model is appropriate, 
# given both what you have seen this week in terms of the residuals plots, 
# and what you saw last week in terms of the results of the model. Particularly, comment on the appearance of the density plots of the two variables that you made last week as compared to the density plots of the residuals.

# I feel that the model is appropriate because of the following reasons: 

#POINT 1: (in terms of the residuals plots from this week)
# The (residuals) density plot is normally distrubuted.
# This follows our assumption that the residuals should be normally distrubuted.
# Therefore this model is appropriate.

#POINT 2: (in terms of the scale-location graph from this week)
# For the scale-location graph, the red line should be approximately flat if the distrubances are homoscedastic.
# In our case, this is true. 
# Therefore this also follows another of our assumptions of homoscedasticity (equal variance) of residuals.
# Therefore this model is appropriate.

#POINT 3 (from last week - results of the model)
# Last week, when we ran the code below
faithful.lm <- lm(waiting ~ eruptions, data=faithful)  # Build a linear regression model on the full data. (formula = waiting ~ eruptions)
print(faithful.lm) # Print it out.
summary(faithful.lm) # Inspect the results
# We got a Multiple R-squared of 0.8115 and an Adjusted R-squared value of 0.8108. 
# The difference between these two values is only 7x10^-4. Hence, we have not overfitted our model
# This means our model is appropriate.

#POINT 4: (What I saw this week in terms of the results of the model)
# Last week, we used the same faithful dataset. 
# Another of our assumptions was that the X variable and residuals are uncorrelated.
# We can double-check this by running the code below.
mod.lm <- lm(waiting ~ eruptions, data=faithful)
cor.test(faithful$eruptions, mod.lm$residuals) # do correlation test
# Check p-value, if >0.05, no evidence of correlation
# When we run the code above, we get a p-value of 1. This is greater than 0.05.
# Hence, there is no evidence of correlation.
# Therefore this model is appropriate.

#POINT 5: (What I saw this week in terms of the results of the model)
# Another of our assumptions was that the mean of residuals is (close to) zero
mod <- lm(waiting ~ eruptions, data=faithful)
mean(mod$residuals)
# This mean of residuals gives us -1.178592e-17, which is indeed close to zero.
# Therefore this model is appropriate.


# POINT 6: (from last week and this week) - asked in the question
# The appearance of the density plots of the two variables that I made last week as compared to the density plots of the residuals:
# If we compare the density plots of the two variables agaisnt the density plot of the residuals, We find out that:
# The density plots for eruptions and waiting (the 2 variables we made) are both not normal.
# However, if we look at the density plot of the residuals, it is normal.
# Therefore the model is appropriate.
 
# The amalgamation of these points lead me to believe that 
# this model is appropriate.