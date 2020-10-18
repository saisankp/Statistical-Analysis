# ASSIGNMENT 1 INTRO TO STATISTICS 1
#NAME OF STUDENT : PRATHAMESH SAI
#STUDENT ID : 19314123


#QUESTION 1 
#Read in the csv file Lab1.csv using code, not using the Import button
library(readr)
Lab1 <- read_csv("C:/Statistics/Lab1(1).csv") #read the file
View(Lab1) #View it

#QUESTION 2 
#Display summary statistics for the variable EARN
summary(Lab1$EARN) #displays summary statistics for EARN

#QUESTION 3 
#Display frequencies of the variable Job.class
table(Lab1$`Job class`) #Job Class is initialized as "`Job class`" so we use that name.


#QUESTION 4 
#Display a three-way cross-tabulation of the proportions of variables Educational Level, Gender and Job.Class
ftable(Lab1$EDUC, Lab1$Gender, Lab1$`Job class`) #the function ftable allows us to get this

#QUESTION 5 
#Create a basic histogram of the variable EARN
hist(Lab1$EARN) #we use the function hist

#QUESTION 6 
#Create a basic boxplot of the variable EARN by Job Class
boxplot(Lab1$EARN~Lab1$`Job class`) #the function boxplot is used

#QUESTION 7 
#Create a new variable EARNx10000 that is equal to Earnings divided by 10,000
Lab1$EARNx10000 = Lab1$EARN/10000 # new variable EARNX10000 is equal to earnings (EARN) divided by 10,000.

#QUESTION 8
#Create a scatterplot with EARNx10000 on the x axis and AGE on the Y axis
plot(Lab1$EARNx10000, Lab1$AGE) #plot function used to make a scatterplot with EARN10000 on the x axis and AGE on the Y axis.

