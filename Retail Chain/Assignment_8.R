library(tidyverse)
library(readxl)
library(dplyr)
library(tsibble)
library(lattice)
library(lubridate)
library(lme4)
library(lmtest)


#Importing the dataset


stores <- read_xlsx("C:/College/Courses/SDM/Assignments/Retail Chain/RetailChain.xlsx", sheet = "stores")
products <- read_xlsx("C:/College/Courses/SDM/Assignments/Retail Chain/RetailChain.xlsx", sheet = "products")
trans <- read_xlsx("C:/College/Courses/SDM/Assignments/Retail Chain/RetailChain.xlsx", sheet = "transactions")

colnames(stores) = tolower(make.names(colnames(stores)))
colnames(products) = tolower(make.names(colnames(products)))
colnames(trans) = tolower(make.names(colnames(trans)))

#joining the tables


df <- trans
df <- df %>% inner_join(stores, by = c("store_num" = "store_id"))
df <- df %>% inner_join(products, by = "upc")
df$p_size <- as.double(df$p_size)

head(df)

#extracting weeks, months, year and week of month data from the date field

df$week <- week(df$week_end_date)
df$quarter <- quarter(df$week_end_date)
df$month <- month(df$week_end_date)
df$year <- year(df$week_end_date) 
df.ymd <- ymd(df$week_end_date)
df$weekofmonth<-week(df.ymd) - week(floor_date(df.ymd, unit = "months")) + 1
table(df$weekofmonth)

#removing data not needed for analysis

df <- df %>% filter(category != "ORAL HYGIENE PRODUCTS")
df <- df %>% separate(product_size, c("p_size", "p_unit"), sep = "\\s")

#converting all columns to factors

coltoFactor <- c('store_num','store_name','upc','city','state','segment','description','manufacturer','category','sub_category','p_unit','feature','display','tpr_only','year','quarter','weeks','month','weekofmonth')
df[,coltoFactor] <- lapply(df[,coltoFactor],factor)

#removing null values from dataset

colSums(is.na(df))
df <- df %>% select(-parking)
df <- df[complete.cases(df),]

############################
#EDA

hist(df$spend)
hist(log(df$spend))
hist(df$hhs)
hist(log(df$hhs))
hist(df$units)
hist(log(df$units))


summary(df)
nums <- unlist(lapply(df, is.numeric))
PerformanceAnalytics::chart.Correlation(df[,nums])

dftest <- df %>% group_by(week_end_date,category) %>% summarise(avgunits = mean(units),avghhs= mean(hhs),avgspend = mean(spend)) 
ggplot(aes(week_end_date,avgunits),data = dftest) + facet_wrap(~category)+geom_point() + geom_line() + geom_smooth()
ggplot(aes(week_end_date,avghhs),data = dftest) + facet_wrap(~category)+geom_point() + geom_line() + geom_smooth()
ggplot(aes(week_end_date,avgspend),data = dftest) + facet_wrap(~category)+geom_point() + geom_line() + geom_smooth()

dftest <- df %>% group_by(week_end_date,segment) %>% summarise(avgunits = mean(units),avghhs= mean(hhs),avgspend = mean(spend)) 
ggplot(aes(week_end_date,avgunits),data = dftest) + facet_wrap(~segment) + geom_point() + geom_line() + geom_smooth()
ggplot(aes(week_end_date,avghhs),data = dftest) + facet_wrap(~segment) + geom_point() + geom_line() + geom_smooth()
ggplot(aes(week_end_date,avgspend),data = dftest) + facet_wrap(~segment) + geom_point() + geom_line() + geom_smooth()

#some dependent variables have 0 in them

df <- df %>% filter(spend > 0)
df <- df %>% filter(units > 0)
df <- df %>% filter(hhs > 0)

#models

lmer1 <- lmer(log(spend) ~ price + feature + display + tpr_only + (1|store_num) + msa + category + segment +  weekofmonth + quarter + year ,data =df, REML = FALSE )
summary(lmer1)
ranef(lmer1)
dwtest(lmer1)
car::vif(lmer1)

lmer2 <- lmer(log(hhs) ~ price + feature + display + tpr_only + (1|store_num) + msa + category + segment +  weekofmonth + quarter + year ,data =df, REML = FALSE)
summary(lmer2)
ranef(lmer2)
dwtest(lmer2)
car::vif(lmer2)

lmer3 <- lmer(log(units) ~ price + feature + display + tpr_only + (1|store_num) + msa + category + segment +  weekofmonth + quarter + year ,data =df, REML = FALSE)
summary(lmer3)
ranef(lmer3)
dwtest(lmer3)
car::vif(lmer3)
stargazer::stargazer(lmer1,lmer2,lmer3,type = "text")


#modeling for checking price elasticity

el_lm <- lm(log(spend) ~ price*description ,data =df)
summary(el_lmer)

el_lm2 <- lm(log(spend) ~ price*upc,data =df)
summary(el_lmer2)
unique(df$upc)
