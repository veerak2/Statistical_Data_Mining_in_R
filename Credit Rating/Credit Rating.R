library("PerformanceAnalytics")
library("interactions")
library("car")
library("ggplot2")
library(plyr)

setwd("C:/College/Courses/SDM/")
d <- read.csv('CreditRating.csv')
d <- d[,-1]
#checking for null values
colSums((is.na(d)))
class(d)

#univariate analysis
nums <- unlist(lapply(d, is.numeric)) # tking only numerix values
summary(d)
hist(d$Rating)
hist(d$Income)
hist(log(d$Income))
hist(d$Balance)
hist(log(d$Balance))
skewness(d[,nums])
skewness(log(d$Rating))
count(d,'Ethnicity')
count(d,'Student')
count(d,'Gender')
count(d,'Married')

#multivariate analysis
chart.Correlation(d[,nums])
#income and rating corr = 0.79
#income and balance = 0.46
#balance and rating = 0.86
#limit and rating = 1



#interact_plot(m2_2017, pred = DrugUsersPerCap, modx = GunsPerCap, modxvals="plus-minus", modx.labels=
              #  c("Low (-1 SD)", "High (+1 SD)"))

interaction.plot(x.factor = d$Income, trace.factor = d$Student, response = d$Rating , ylim = range(d$Rating), fun = mean)

?interaction.plot


  
d = d[order(d$Rating),]


ggplot() +
  aes(x = log(d$Income), y = d$Rating, color = d$Ethnicity) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

ggplot() +
  aes(x = d$Income, y = log(d$Balance), color = d$Student) +
  geom_point() + geom_smooth(method = "lm", se = FALSE)

ggplot() +
  aes(x = d$Income, y =d$Education) + geom_bar(stat = "identity")
higheducation = subset(d, d$Education > 18)

m1 <- lm(Rating ~ Income + Age + Balance + Cards + Ethnicity + Gender + Education + Student , data=d) 
summary(m1)       
plot(m1)
vif(m1)

#m2 did not make a difference with log of rating
m2 <- lm(log(Rating) ~  Income + Age + Balance + Cards + Ethnicity + Gender + Education + Student  , data=d) 
summary(m2)       
plot(m2)

#Income and balance interact
m3 <- lm(Rating ~ Income + Income*Ethnicity + Age + Income*Balance + Balance + Cards + Ethnicity + Gender + Education + Student  , data=d) 
summary(m3)       
plot(m3)

#
m4 <- lm(Rating ~ Income + Cards  + Age + Income*Balance + Balance + Income*Cards + Ethnicity + Gender + Education + Student  , data=d) 
summary(m4)       
plot(m4)


m5 <- lm(Rating ~ Balance + Age + Cards + Ethnicity + Gender + Education + Student , data=d) 
summary(m5)       
plot(m5)

m6 <- lm(Rating ~ log(Balance), data =d)
summary(m6)
plot(m6)
