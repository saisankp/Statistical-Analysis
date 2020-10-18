#PART 1: Simulate a population of 10,000 with a variable from a Normal(4,5) distribution and a population of 10,000 with
# a variable from an Exponential(1) distribution [using the command rexp(n,lambda) ]

# PART 2: Draw graphs to help you assess the normality of each population, as well as samples of size 10, 50 and 500 from each population. 

# PART 3: Calculate confidence intervals for the mean of the
#sample of size 50 from each distribution, using a) the Z-score and the known population SD, and
# b) the t-distribution and the sample SD.

#PART 1:
x0<- rnorm(10000,4,5)
#create a vector of 10000 elements with mean as 4 and standard deviation of 5(normaly distributed)
y0<- rexp(10000,1)
#create a vector of 10000 with rate 1 (which is exponentially distributed)


#PART 2:
#get the samples:
x1<- sample(x0,10)
#take a random sample of 10 items from the normal distribution.
x2<- sample(x0,50)
#take a random sample of 50 items from the normal distribution.
x3<- sample(x0,500)
#taking a random sample of 500 items from the normal distribution.
y1<- sample(y0,10)
#taking a random sample of 10 items from the normal distribution.
y2<- sample(y0,50)
#taking a random sample of 50 items from the normal distribution.
y3<- sample(y0,500)
#taking a random sample of 500 items from the normal distribution

#A qqplot and a histogram with normal density curve for x0:
qqnorm(x0)
qqline(x0)
hist(x0, freq = FALSE)
xfit <- seq(min(x0), max(x0), length = 10000) 
yfit <- dnorm(xfit, mean = mean(x0), sd = sd(x0))
lines(xfit, yfit)
#since the graph forms a bell curve hence we can assume normality

#A qqplot and a histogram with normal density curve for y0:
qqnorm(y0)
qqline(y0)
hist(y0, freq = FALSE)
xfit <- seq(min(y0), max(y0), length = 10000) 
yfit <- dnorm(xfit, mean = mean(y0), sd = sd(y0))
lines(xfit, yfit)
#All the points do not form a bell curve therefore the data does not follow normality.

#A qqplot and a histogram with normal density curve for x1:
qqnorm(x1)
qqline(x1)
hist(x1, freq = FALSE)
xfit <- seq(min(x1), max(x1), length = 10) 
yfit <- dnorm(xfit, mean = mean(x1), sd = sd(x1))
lines(xfit, yfit)
#The graph forms a bell curve hence we can assume normality.

#A qqplot and a histogram with normal density curve for x2:
qqnorm(x2)
qqline(x2)
hist(x2, freq = FALSE)
xfit <- seq(min(x2), max(x2), length = 50) 
yfit <- dnorm(xfit, mean = mean(x2), sd = sd(x2))
lines(xfit, yfit)
#The graph forms a bell curve hence we can assume normality.

#A qqplot and a histogram with normal density curve for x3:
qqnorm(x3)
qqline(x3)
hist(x3, freq = FALSE)
xfit <- seq(min(x3), max(x3), length = 500) 
yfit <- dnorm(xfit, mean = mean(x3), sd = sd(x3))
lines(xfit, yfit)
#The graph forms a bell curve hence we can assume normality.

#make a qqplot and a histogram with normal density curve for y1:
qqnorm(y1)
qqline(y1)
hist(y1, freq = FALSE)
xfit <- seq(min(y1), max(y1), length = 10) 
yfit <- dnorm(xfit, mean = mean(y1), sd = sd(y1))
lines(xfit, yfit)
#All the points do not form a bell curve therefore the data does not follow normality.

#make a qqplot and a histogram with normal density curve for y2:
qqnorm(y2)
qqline(y2)
hist(y2, freq = FALSE)
xfit <- seq(min(y2), max(y2), length = 50) 
yfit <- dnorm(xfit, mean = mean(y2), sd = sd(y2))
lines(xfit, yfit)
#All the points do not form a bell curve therefore the data does not follow normality.

#make a qqplot and a histogram with normal density curve for y3:
qqnorm(y3)
qqline(y3)
hist(y3, freq = FALSE)
xfit <- seq(min(y3), max(y3), length = 500) 
yfit <- dnorm(xfit, mean = mean(y3), sd = sd(y3))
lines(xfit, yfit)
#All the points do not form a bell curve therefore the data does not follow normality.

#PART 3
#calculating confidence intervals for the mean of the sample of size 50 from each distribution, using
#a) the Z-score and the known population SD, and 
#b) the t-distribution and the sample SD.  

# Part 3 (b)
meanX2<- mean(x2)
#Calculating mean of sample x2
meanY2<- mean(y2)
#Calculating mean of sample y2
sdX2<- sd(x2)
#Calculating standard deviation of sample x2
sdY2<- sd(y2)
#Calculating standard deviation of sample y2
knPsdX0<-sd(x0)
#Calculating known sd (standard deviation of the normal distribution x0)
knPsdY0<-sd(y0)
#Calculating known sd (standard deviation of the exponential distribution y0)
t_score<- qt(0.975,49)
#Calculating t score for 95% confidence value with 49 degrees of freedom
Z_score<- qnorm(0.975)
#Calculating t score for 95% confidence value with 49 degrees of freedom
Xleft_Z95_kn <- meanX2-Z_score*knPsdX0
#Calculating left confidence interval for known sd 
Xright_z95_kn <- meanX2+Z_score*knPsdX0
#Calculating right confidence interval for known sd
Xleft_t95_unkn <- meanX2-t_score*sdX2
#Calculating left confidence interval for unknown sd with t_score
Xright_t95_unkn <- meanX2+t_score*sdX2
#calculating right confidence interval for unknown sd with t_score
Yleft_Z95_kn <- meanY2-Z_score*knPsdY0
#calculating left confidence interval for known sd
Yright_z95_kn <- meanY2+Z_score*knPsdY0
#calculating left confidence interval for known sd
Yleft_t95_unkn <- meanY2-t_score*sdY2
#calculating left confidence interval for unknown sd with t_score
Yright_t95_unkn <- meanY2+t_score*sdY2
#calculating left confidence interval for unknown sd with t_score

# Part 3(a)
paste("var: x0", "mean:", mean(x0), "sd:", knPsdX0)
paste("95% CIs for estimate of sample mean (meanX2 and meanY2) and population mean of both normaland exponential distribution(x0 and y0)")
paste("Z-distribution w/known pop sd(normal distribution, Zscore with known sd):", Xleft_Z95_kn, Xright_z95_kn)
paste("Z-distribution w/unknown pop sd(sample sd with t score of normal distribution):", Xleft_t95_unkn, Xright_t95_unkn)
paste("Z-distribution w/known pop sd(exponential distribution, Zscore with known sd):", Yleft_Z95_kn, Yright_z95_kn)
paste("Z-distribution w/unknown pop sd(sample sd with t score, of exponential distribution):", Yleft_t95_unkn, Yright_t95_unkn)


