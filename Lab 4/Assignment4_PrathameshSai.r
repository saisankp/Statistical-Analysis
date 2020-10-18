# Code for the one-sample hypothesis test function is below.
OneSampTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd)
{
  # We calculate the test statistic below.
  se = (sd/sqrt(n))
  test_stat <- (x_bar-mu)/(sd/sqrt(n))
  
  if (type=="z") {  
    # Get the p-value for this test statistic below.
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    # Define df.
    df <- n-1  
    # Get the p-value of this test statistic.
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  # Check if it is significant.
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("One Sample", type, "test.", tails, "tailed"), mu=mu, n=n, x_bar=x_bar, se=se, p = p_val, alpha = alpha, significance = sig )
  # Return the list.
  return( ret )
}
# The First task!
P <- rnorm(100,4,5) #making a normal destribution
OneSampTest("z", "left" ,0.05, 0, 100, 4, 5)
#doing a z test with true population sd using alpha 0.05

# The Second task!
Psample <-sample(P,10) #taking a random sample of size 10
sampleSD<- sd(Psample) #standard deviation of the sample
OneSampTest("t", "right" ,0.025, 4.2, 10, 4, sampleSD)
# Do a t test with sample sd and alpha 0.025
# By analyzing the result we fail to reject the hypotheses since p_value > 0.5

#The Third Task!
Psample2<- sample(P,10)
t.test(Psample,Psample2)
# Using t.test function with two randomly chosen samples of size 10
# By analyzing the result we reject the hypotheses since p_value > 0.5

#The Fourth Task!
population <- rbinom(100,100,.3) # Making a bernoulli destrubtion of size 100
Sp <- sample(population,20) # Taking a sample of size 20
Mp <- mean(population) # Calculating the mean of the population
Pprop <- Sp/population # Calculating the population proportion
SDp <- sd(population) # Calculating the standard deviation of the population
# Now treat Pprop as the known mean for the function
OneSampTest("z", "two" ,0.05, 0.28,20, Pprop, SDp)
# By analyzing the result we fail to reject the hypotheses since the p value ranges around 0.5

#The Fifth Task!
OneSampTest("z", "left" ,0.05, 0.35,20, Pprop, SDp) 
#doing a left tailed z test for 0.35
#by analyzing the p_value we reject the hypotheses







