#Multi level data analysts
#Data is available at 3 Levels: :Level 1 = "Items" Level 2 = "Outlets" Level 3 = "Tiers"


library(tidyverse)
library(readxl)
library(dplyr)
library(glue)
library(lattice)
library(lme4)

#reading data into dataframe
df <- read_xlsx("C:/College/Courses/SDM/Assignments/Big Mart/BigMartSales.xlsx", sheet = "Data")

#converting categorical variables to factors
names <-c('Item_ID','Outlet_ID','Outlet_Size','City_Type','Outlet_Type','Item_Type','Item_Fat_Content')
df[,names] <- lapply(df[,names],factor)

#DATA CLEANING

#low fat and Low Fat were considered as two different factors
df$Item_Fat_Content <- dplyr::recode_factor(df$Item_Fat_Content,"low fat" = "Low Fat",.default = levels(df$Item_Fat_Content))


# there are null values in OutletSize and Item_weight
colSums(is.na(df))

df <- df[complete.cases(df$Outlet_Size),] #dropping null values only based on Outlet size as am not using item weight

#FEATURE ENGINEERING

#creating a variable to see how old an outlet is
df$Outlet_yrsold <- 2013 - df$Outlet_Year

detach(df)
attach(df)
#EDA
#looking at our dependent variable 
hist(df$Item_Sales)
hist(log(df$Item_Sales))
densityplot(~Item_Sales|Item_Type)
densityplot(~Item_Sales|Outlet_ID,main = "Item Sales By Outlet")
densityplot(~Item_Sales|Outlet_Type,main = "Item Sales by Outlet type")
densityplot(~Item_Sales|City_Type, main = "Item Sales by City type")

#checking for correlations
nums <- unlist(lapply(df, is.numeric))
PerformanceAnalytics::chart.Correlation(df[,nums])

#item sales in different outlets
xyplot(Item_Sales ~ Item_Visibility|Outlet_Size)
xyplot(Item_Sales ~ Item_MRP|Outlet_Type)
xyplot(Item_Sales ~ Item_MRP|Item_Type)


#MRP vs Item Sales
xyplot(Item_Sales ~ Item_MRP)
ggplot(df,aes(Item_MRP,Item_Sales)) + geom_point() + geom_smooth()
#Item sales by outlet
bwplot(Item_Sales ~ Outlet_ID, main = "Item Sales by Outlet")
bwplot(Item_Sales ~ Outlet_Type)

#model building
#The dependant variable follows a poisson distribution. Although its not a count, the underlying distribution is a count
#Looking at how item sales does between outlets. 
glmer_m1 <- glmer(round(Item_Sales) ~ 1 + (1|Outlet_ID),family = poisson(link = "log"))
summary(glmer_m1)
AIC(glmer_m1)
ranef(glmer_m1)     

#model failing to converge. So we cannot rely on the estimates, as they are not the optimum values. 
#Since I dont know much about how to tune the hyperparameters for multilevel models, I will keep it simple and use lmer
#Tried several models before ending up with this one.
glmer_m2 <- glmer(round(Item_Sales) ~ Item_Fat_Content + Item_MRP + Item_Visibility + Item_Type +  Outlet_Size +  Outlet_yrsold +(1|Outlet_ID) + Outlet_Type + City_Type,family = poisson(link = "log"))
summary(glmer_m2)
AIC(glmer_m2)
ranef(glmer_m2) 
                  
#lmer models

#Random intercept model
lmer1 <- lmer(Item_Sales ~ 1 + (1|Outlet_ID),REML = FALSE)
summary(lmer1)
ranef(lmer1) 

#Random intercept model with fixed level 1 predictors
lmer2 <- lmer(Item_Sales ~ Item_Fat_Content + Item_MRP + Item_Visibility + Item_Type + (1|Outlet_ID),REML = FALSE)
summary(lmer2)
ranef(lmer2) 
anova(lmer1,lmer2)


#Random intercept model with fixed level 1 predictors and level 2 predictors
lmer3 <- lmer(Item_Sales ~ Item_Fat_Content  + Item_MRP + Item_Visibility + Item_Type + Outlet_yrsold + Outlet_Size + (1|Outlet_ID),REML = FALSE)
summary(lmer3)
ranef(lmer3) 

#Random intercept model with fixed level 1 predictors and level 2 predictors =
lmer4 <- lmer(Item_Sales ~ Item_Fat_Content + Item_MRP + Item_Visibility + Item_Type + Outlet_yrsold + (1|Outlet_ID) + Outlet_Type,REML = FALSE)
summary(lmer4)
ranef(lmer4)
coef(lmer4)
anova(lmer3,lmer4)

#2 random intercept model with fixed level 1,level 2 predictors
#Assuming outletId and outlet type have independant random effects
lmer5 <- lmer(Item_Sales ~ Item_Fat_Content + Item_MRP + Item_Visibility + Item_Type + Outlet_yrsold + (1|Outlet_ID) + (1|Outlet_Type) ,REML = FALSE)
summary(lmer5)
ranef(lmer5)
anova(lmer3,lmer5)

#2 random intercept model with fixed level 1,level 2 predictors. There are no predictors for level 3 ie city level

#including city_type for my final model to answer the question
lmer6 <- lmer(Item_Sales ~ Item_Fat_Content + Item_Visibility + Item_MRP + Item_Type + Outlet_yrsold + (1|Outlet_ID) + (1|Outlet_Type) + City_Type,REML = FALSE)
summary(lmer6)
ranef(lmer6)
anova(lemer5,lmer6)
car::vif(lmer6)

#adding outlet size makes the variance for the random variable of outerID 0. Tried removing the null values from outlet size, still could not get it to work.
lmer7 <- lmer(Item_Sales ~ Item_Fat_Content + Item_Visibility + Item_MRP + Item_Type + Outlet_yrsold + Outlet_Size + (1|Outlet_ID) + (1|Outlet_Type) + City_Type,REML = FALSE)
summary(lmer7)
ranef(lmer7)
anova(lemer5,lmer7)

stargazer::stargazer(glmer_m2,lmer6,lmer7,type = "text")
