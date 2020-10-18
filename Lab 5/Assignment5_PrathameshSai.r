library("Rlab") 
#In this assignment, we modify our code from last week's assignment.

#Key for variables:
#var n1 = sample size of first sample
#var n2 = sample size of second sample
#var mu_hat_1 = mean of the population under the null hypothesis (first sample)
#var mu_hat_2 = mean of the population under the null hypothesis (second sample)
#var x_bar = mean of the sample
#var sd =  known population standard deviation (our estimate of it from our sample)
#alpha = the significance level i.e 0.05
#string var tails = "two" "left" or "right" which indicates a two-tailed test or direction of one-tailed.
#Ho = null Hypothesis
#HA = alternate Hypthesis

#Program for a two sampled test


#Two-sample hypothesis test function
# Explanation of modification:
# We change the code from "OneSampTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd) "
# into the code below, note: we put typeOfTest equal to NULL and it takes input to be either z or t
# and we include two standard deviations in our code instead of one (sd1, sd2).
TwoSampTest <- function(type=NULL, typeOfTest=NULL, tails=NULL, alpha, mu_hat_1, mu_hat_2, n1, n2, sd1, sd2, D)
{
  
  # We calculate the test statistic
  if(type =="proportion"){
    pooled_prop <- ((mu_hat_1*n1) + (mu_hat_2*n2))/(n1 + n2) #Here, we calculate the pooled proportion
    print(pooled_prop)
    
    
    se <- sqrt(pooled_prop*(1-pooled_prop)*((1/n1)+(1/n2))) #Here, we calculate the standard error.
    print(se)
    
  }else if(type == "population"){
    pooled_sd <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1 + n2 - 2)) #Here, we calculate the pooled standard deviation
    
    se <- pooled_sd*sqrt((1/n1) + (1/n2)) # Here, we calculate the standard error
    
  }
  else{stop("Sorry, you must choose proportion or population")}
  
  test_stat <- (mu_hat_1 - mu_hat_2 - D)/se #calculate the test statistic from the forumula.
  #The test statistic is the same for both tests.
  
  #We can use if statements for the z and t-tests
  if (typeOfTest=="z") {   
    #get the p-value of test statistic (same code as last week, no modifications needed)
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (typeOfTest =="t") {
    #modification: last week we used df = n - 1, but now we use 
    df <- n1 + n2 - 1  
    #Why?: this is because we have n1 and n2 instead of just n.
    
    #get the p-value of test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("Sorry, you must choose z or t")}
  
  # Check if it's significant  or not.
  if (p_val < alpha) {sig <-"significant" #it is significant.
  }  else {sig <-"not significant"}       #it is not significant.
  
  
  ret <- list(type=paste("Two Sample", typeOfTest, "test of", type, ".", tails, "tailed"), n1=n1, n2=n2, pop_D=abs(mu_hat_1-mu_hat_2), se_est=se, p = p_val, alpha = alpha, significance = sig, test_stat=test_stat )
  #Now we return the list to the user
  return( ret )
}


pop1 <- rnorm(100,4,5) # Simulating a vector of size 100 drawn from a Normal(4,5) distribution.
pop2 <- rnorm(80,3.5,2) # Simulating a vector of size 80 drawn from a Normal(3.5,2) distribution.
alpha <- 0.05 # We set alpha to be 0.05 as it's stated in the question. (Note: this is the significance value)

#ASSIGNMENT QUESTIONS

# Task One

# we use a z test in this question with the true population standard deviations.
TwoSampTest("population", "z","two", alpha, 4, 3.5,100,80, 5, 2, 1)
#Note: We put 1 at the end for D, which is the difference of the means of the populations. We want to test if it's equal to one so we put 1 here.

# p = 0.3999
# test statistic = -0.8417998

#Since p > 0.05 (i.e 0.3999 < 0.05), we fail to reject Ho (the difference in means of the populations are equal to 1)

# Task Two

# we use sample standard deviations this time for the second task.
sd_pop1 <- sd(pop1) #get the standard deviation of pop1 and assign it to sd_pop1.
sd_pop2 <- sd(pop2) #get the standard deviation of pop2 and assign it to sd_pop2.
pop1_mean <- mean(pop1) #get the mean of pop1 and assign it to pop1_mean.
pop2_mean <- mean(pop2) #get the mean of pop2 and assign it to pop2_mean.
# we do a sample z test in this question using the sample standard deviations and means.

TwoSampTest("population", "z","two", alpha, pop1_mean, pop2_mean,100,80,sd_pop1 ,sd_pop2  , 0)
#Note: We put 0 at the end for D, which is the difference of the means of the populations. We want to test if it's not equal so we put 0 here.

# p = 0.7411947
# test statistic = 0.3302717
#Since p > 0.05 (i.e 0.7411947 < 0.05), we fail to reject Ho (the means of the populations are equal)

# Task Three
#We do a two sample t-test using alpha = 0.05 (we set this already to be =0.05).
TwoSampTest("population", "t","two", alpha, 4, 3.5,100,80, 5, 2, 0)
#Note the difference is 0 since we want to test if they are not equal.

#p = 0.2005116
# test statistic = 0.8417998
#Since p > 0.05 (i.e 0.2005116 < 0.05), we fail to reject Ho (the means of the populations are equal)

# Task four
#We do a t-test to see if the means of the populations are equal or not
t.test(pop1,pop2) 
#note: if we dont put var.equal as we assume equal variance

# t = 0.35663
#df = 140.87
# p = 0.7219

#Since p > 0.05 (i.e 0.7219 < 0.05), we fail to reject H0 (the means of the populations are equal)


# Task five
#We do a t-test to see if the means of the populations are equal or not without assuming equal variance.
t.test(pop1,pop2,var.equal=FALSE)
#Note: we use var.equal = FALSE because we don't assume equal variance.

# t = 0.35663
# df = 140.87
# p = 0.7219

#Since p > 0.05 (i.e 0.7219 < 0.05), we fail to reject H0 (the means of the populations are equal)

# Task six

b1 <- rbern(100,0.3) #100 is from the 100 bernouilli trials, and 0.3 is the probability of success.
b2 <- rbern(85,0.7)  #85 is from the 85 bernouilli trials, and 0.7 is the probability of success.
#Note: Had to import the library RLab for this to work (rbern function)

b1_mean <- mean(b1) #Get the mean and store it in b1_mean
b2_mean <- mean(b2) #Get the mean and store it in b2_mean
b1_sd <- sd(b1) #Get the standard deviation and store it in b1_sd
b2_sd <- sd(b2) #Get the standard deviation and store it in b2_sd

#Do a t-test, but now not assuming equal variance.
TwoSampTest("proportion","t","two", alpha, b1_mean ,b2_mean,100,85, b1_sd,b2_sd, 0)


# p = 6.761416e-08
# test statistic = -5.484991


#Since p < 0.05 (i.e 6.761416e-08 < 0.05), we reject H0 (the population proportions are equal)
#Hence we accept HA (the population proportions are not equal)

# Task seven
pooled_proportion <- (b1_mean*100 + b2_mean*85)/(185)
standardE <- sqrt(pooled_proportion*(1-pooled_proportion)*((1/100)+(1/85))) # we calculate the standard error using the formula.
mean_diff <- b1_mean - b2_mean # we calculate the mean difference.
test_stat <- (mean_diff)/standardE
test_stat
# test statistic = -5.484991
#The test statistic is not in the range of -1.96 to +1.96.
#Therefore, we reject H0 and accept HA
#Hence we recalculate standardE (Standard error)
standardE <- sqrt((b1_mean*(1-b1_mean))/100+(b2_mean*(1-b2_mean))/85)
t_value <- 1.96 #We set the t value as a 96 percent confidence value.
Lower_limit <- mean_diff - (standardE*t_value) #calculate the lower limit for the confidence interval.
Upper_limit <- mean_diff + (standardE*t_value)#calculate the upper limit for the confidence interval.
Upper_limit #Show the upper limit.
Lower_limit #Show the lower limit.
