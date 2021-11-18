## ----setup, include=FALSE----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(dplyr)
library(glue)


## ---- echo=FALSE-------------------------------------------------------------------
df <- read_xlsx("C:/College/Courses/SDM/Assignments/Online Retail Promotions/OnlineretailPromotions.xlsx", sheet = "Data")


## ----------------------------------------------------------------------------------
df_v <- df %>% 
  filter(df$visit == 1)


## ----------------------------------------------------------------------------------
df_v %>% 
  mutate(spend.not = ifelse(spend > 0, "spent", "not spent" )) %>% 
  group_by(spend.not) %>% count() %>% 
  ggplot(aes(spend.not,n)) +
          geom_bar(stat='identity') + xlab("Spent or not") + ylab("count")
  

## ----------------------------------------------------------------------------------
df_v %>% 
  group_by(conversion)%>% 
  count()


## ----------------------------------------------------------------------------------
df_v %>% 
  group_by(campaign) %>% 
  count()


## ----------------------------------------------------------------------------------
df_v %>% 
  group_by(campaign,conversion) %>% 
  summarise()
  


## ----------------------------------------------------------------------------------
df_v$campaign <- as.factor(df_v$campaign)


## ----------------------------------------------------------------------------------
df_v %>% 
  ggplot(aes(conversion,fill = campaign) ) + 
  geom_bar(position = 'dodge')


## ----------------------------------------------------------------------------------
df_v %>% rowwise() %>% 
  mutate(merch_purchased = case_when((mens == 1 && womens == 1)~"both",(mens == 1 && womens == 0) ~ "men",(womens == 1 && mens == 0) ~ "women")) %>% 
  group_by(merch_purchased) %>% 
  count()


## ----------------------------------------------------------------------------------
df_v <- df_v %>% rowwise() %>% 
  mutate(merch_purchased = case_when((mens == 1 && womens == 1)~"both",(mens == 1 && womens == 0) ~ "men",(womens == 1 && mens == 0) ~ "women")) 


## ----------------------------------------------------------------------------------
df_v$merch_purchased <- as_factor(df_v$merch_purchased)


## ----------------------------------------------------------------------------------



## ----------------------------------------------------------------------------------
df_v %>% filter(spend > 0) %>% 
  add_count(zipcode,name={"total"}) %>% 
  mutate(zipcode = glue("{zipcode}({total})"),       
         fct_reorder(zipcode,spend)) %>% 
  ggplot(aes(x = zipcode,y=spend)) +
    geom_boxplot() 


## ----------------------------------------------------------------------------------
df_v %>% filter(spend > 0) %>% 
  add_count(campaign,name={"total"}) %>% 
  mutate(campaign = glue("{campaign}({total})"),       
         fct_reorder(campaign,spend)) %>% 
  ggplot(aes(x = campaign,y=spend)) +
    geom_boxplot() 

## ----------------------------------------------------------------------------------
df_v %>% filter(spend > 0) %>% 
  ggplot() +
  aes(x = history, y = spend) +
  geom_point() + geom_smooth(method = "lm", se=FALSE) 

## ----------------------------------------------------------------------------------
df_v %>% filter(spend > 0) %>% 
  add_count(historysegment,name={"total"}) %>% 
  mutate(historysegment = glue("{historysegment}({total})"),       
         fct_reorder(historysegment,spend)) %>% 
  ggplot(aes(x = historysegment,y=spend)) +
    geom_boxplot() 


## ----------------------------------------------------------------------------------
df_v %>% filter(spend > 0) %>% 
  add_count(newcustomer,name={"total"}) %>% 
  mutate(newcustomer = glue("{newcustomer}({total})"),       
         fct_reorder(newcustomer,spend)) %>% 
  ggplot(aes(x = newcustomer,y=spend)) +
    geom_boxplot() 


## ----------------------------------------------------------------------------------
hist(df_v$spend)

## ----------------------------------------------------------------------------------
df_c <- df_v %>% 
  filter(conversion == 1)


## ----------------------------------------------------------------------------------
hist(df_c$spend)

## ----------------------------------------------------------------------------------
df_c %>% 
  filter(spend == 29.99)

## ----------------------------------------------------------------------------------
hist(log(df_c$spend))


## ----------------------------------------------------------------------------------
df_c %>% 
  add_count(campaign,name={"total"}) %>% 
  mutate(campaign = glue("{campaign}({total})"),       
         fct_reorder(campaign,spend)) %>% 
  ggplot(aes(x = campaign,y=spend)) +
    geom_boxplot() 


## ----------------------------------------------------------------------------------
df_c %>% 
  ggplot() +
  aes(x = recency, y = spend) +
  geom_point() + geom_smooth() 


## ----------------------------------------------------------------------------------
df_c %>% filter(spend > 0) %>% 
  ggplot() +
  aes(x = history, y = spend) +
  geom_point() + geom_smooth() 


## ----------------------------------------------------------------------------------
df_c  %>% 
  add_count(zipcode,name={"total"}) %>% 
  mutate(zipcode = glue("{zipcode}({total})"),       
         fct_reorder(zipcode,spend)) %>% 
  ggplot(aes(x = zipcode,y=spend)) +
    geom_boxplot() 


## ----------------------------------------------------------------------------------
df_c %>%
  add_count(newcustomer,name={"total"}) %>% 
  mutate(newcustomer = glue("{newcustomer}({total})"),       
         fct_reorder(newcustomer,spend)) %>% 
  ggplot(aes(x = newcustomer,y=spend)) +
    geom_boxplot() 

## ----------------------------------------------------------------------------------
df_c %>%
  add_count(channel,name={"total"}) %>% 
  mutate(channel = glue("{channel}({total})"),       
         fct_reorder(channel,spend)) %>% 
  ggplot(aes(x = channel,y=spend)) +
    geom_boxplot() 


## ----------------------------------------------------------------------------------
summary(df_c$spend)

## ----------------------------------------------------------------------------------
names <-c('mens','womens','zipcode','newcustomer','channel','campaign','visit','')
dft[conversion,names] <- lapply(dft[,names],factor)


## ----------------------------------------------------------------------------------
nums <- unlist(lapply(df_c, is.numeric))
PerformanceAnalytics::chart.Correlation(df_c[,nums])

