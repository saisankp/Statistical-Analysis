# Statistical-Analysis-I
During the Module STU11002 (Statistical-Analysis-I) in my first year of computer science at Trinity College Dublin, I completed Interesting Labs to improve my understanding of statistical analysis using R.

## Lab 1 (Questions)
Read in the csv file Lab1.csv using code, not the Import button
Display summary statistics for the variable EARN
Display frequencies of the variable Job.class
Display a three-way cross-tabulation of the proportions of variables Educational Level, Gender and Job.Class
Create a basic histogram of the variable EARN
Create a basic boxplot of the variable EARN by Job Class
Create a new variable EARNx10000 that is equal to Earnings divided by 10,000
Create a scatterplot with EARNx10000 on the x axis and AGE on the Y axis

## Lab 2 (Questions)
```R
Multiple Games
So far, we have written code for a single play of the game. But if you want to see how the game turns out over multiple plays, you need to do a little more work. The code below runs 100 games and prints the results

n_stay <- 0    
n_switch <- 0

for ( i in 1:100) {
  door <- c(1,2,3) 
  cardoor <- sample(door,1) 
  choice <- sample(door,1) 
  goatdoors <- setdiff(door, cardoor) 
  reveal_options <- setdiff(goatdoors, choice) 
  if (choice == cardoor) { 
      reveal <- sample(reveal_options,1)  }  
  else {
    reveal <- reveal_options 
  }
  remaining_doors <-setdiff(door, reveal)
  newchoice <- setdiff(remaining_doors, choice)   
  
  if (choice == cardoor) {
    n_stay <- n_stay + 1
  }
  
  if (newchoice == cardoor) {
    n_switch <- n_switch + 1
  }
}
print(n_stay/100)
print(n_switch/100)

```
Copy the code on the previous page for Multiple Games into your R Script
Comment the code appropriately to explain what each piece of code is doing.
Add new code to give a third line of printed output that gives the results for a player who, in each game, randomly chooses to switch or stay. (make sure to comment any new code you add)

## Lab 3 (Questions)

Simulate a population of 10,000 with a variable from a Normal(4,5) distribution and a population of 10,000 with a variable from an Exponential(1) distribution { use the command rexp(n,lambda) }
Draw graphs to help you assess the normality of each population, as well as samples of size 10, 50 and 500 from each population. 
Calculate confidence intervals for the mean of the sample of size 50 from each distribution, using a) the Z-score and the known population SD, and b) the t-distribution and the sample SD.

## Lab 4 (Questions)

Simulate a vector of size 100 drawn from a Normal(4,5) distribution. Using the function on the previous pages, test the following hypotheses (don’t forget to comment your use of the function) and include in comments in your code the resulting test statistic, p-value, and your conclusion. You do not have to add additional comments to the code for the function in your submitted R script.
1)	H0: the mean of the population is equal to 0. HA: the mean of the population is not equal to 0. Use a Z-test with the true population SD. Use alpha = 0.05
2)	H0: the mean of the population is greater than or equal to 4.2. HA: the mean of the population is less than 4.2. Use a t-test with the sample SD. Use alpha = 0.025
3)	Repeat the previous task using the t.test function in the package {stats}
Simulate a vector containing the results of 100 independent Bernoulli trials, each with probability of success 0.3 using the function rbern(n, prob). Using the function on the previous pages,  test the following hypotheses and include in comments in your code the resulting test statistic, p-value, and your conclusion:
4)	H0: the population proportion is equal to 0.28. HA: the population proportion is not equal to 0.28. Use alpha = 0.05
5)	H0: the population proportion is less than or equal to 0.35. HA: the population proportion is greater than 0.35. Use alpha = 0.05

## Lab 5 (Questions)
Take the function from last week and modify it to create a function that performs two-sample Z-tests and t-tests, with the assumption of equal variance. Make sure to account for the case where you want to test proportions. (don’t forget to comment your modifications to the code)
Simulate a vector of size 100 drawn from a Normal(4,5) distribution, and another vector of size 80 drawn from a Normal(3.5,2) distribution. Using your new function, test the following hypotheses and include in comments in your code the resulting test statistic, p-value, and your conclusion:
1)	H0: the difference in means of the populations are equal to 1. HA: the difference in means of the populations are not equal to 1. Use a Z-test with the true population SDs. Use alpha = 0.05
2)	H0: the means of the populations are equal. HA: the means of the populations are not equal. Use a Z-test with the sample SDs. Use alpha = 0.05
3)	H0: the means of the populations are equal. HA: the means of the populations are not equal. Use a t-test. Use alpha = 0.05.
4)	Repeat the previous task using the t.test function in the package {stats}
5)	Repeat the previous task using the t.test function in the package {stats}, without the assumption of equal variance.
Simulate a vector containing the results of 100 independent Bernoulli trials, each with probability of success 0.3 using the function rbern(n, prob). Simulate another vector containing the results of 85 independent Bernoulli trials, each with probability of success 0.7. Using the function on the previous pages, test the following hypothesis and include in comments in your code the resulting test statistic, p-value, and your conclusion:
6)	H0: the population proportions are equal. HA: the population proportions are not equal 
Finally, 
7)	write code to calculate the 95% Confidence Interval for your estimate of the difference between the two population proportions.
Your output from your modified function should look something like this
```R
$type
[1] "Two Sample t test. two tailed"

$n1
[1] 100

$n2
[1] 100

$Pop_D
[1] 1

$diff
[1] -0.9024856

$se_est
[1] 0.1329663

$test_stat
[1] -14.30803

$p
[1] 1.178824e-32

$alpha
[1] 0.05

$significance
[1] "significant"

```

## Lab 6 (Questions)
Load the csv file survey.csv in R.
Use the following code to create a contingency table and run a chi square test.

tbl <- table(survey$Smoke, survey$Exer)
chisq.test(tbl)

In comments in your code, give the results of the chi square test, and answer the following questions:
1)	How were the degrees of freedom for this test calculated?
2)	What are the assumptions of the chi square test?
3)	Why did the code give an error message?
4)	What is the null hypothesis for this test and the alternative hypothesis for this test?
5)	Using a significance level of 0.05, give your conclusions for the result of this test.

## Lab 7 (Questions)

The faithful dataset is preloaded in R. There are two observation variables in the data set. The first one, called eruptions, is the duration of the geyser eruptions. The second one, called waiting, is the length of waiting period until the next eruption. We are interested in predicting eruptions from waiting. 
1)	Check the correlation between the variables eruptions and waiting
2)	Create a plot that shows both the points and a smoothed line of the points 
3)	Create box plots for the variables
4)	Create graphs of the densities of the variables
5)	Fit a simple linear model that predicts eruptions from waiting
6)	Visualise the resulting regression line on a scatterplot of the data
7)	In comments in your code, write an equation that describes the linear model you have fitted
8)	At a significance level of 0.05, does there appear to be a statistically significant relationship between eruptions and waiting? Explain your answer in comments in your code. 
9)	Why are the p-values for the variable waiting and the overall F test so similar for this model?

## Lab 8 (Questions)

We will continue working with the linear model fitted to the faithful dataset last week.

1)	Plot the residuals density
2)	Use the plot function to generate 4 graphs of the residuals vs fitted values, etc. in a single plot
3)	Comment on whether or not you feel the model is appropriate, given both what you have seen this week in terms of the residuals plots, and what you saw last week in terms of the results of the model. Particularly, comment on the appearance of the density plots of the two variables that you made last week as compared to the density plots of the residuals.



