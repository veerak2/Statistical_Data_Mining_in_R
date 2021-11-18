#importing necessary libraries

library(tidyverse)
library(readxl)
library(dplyr)
library(ROCR)

#loading the dataset
df <- read_xlsx("C:/College/Courses/SDM/Assignments/Telco Churn/TelcoChurn.xlsx", sheet = "Data")

#checking for nulls 
colSums(is.na(df)) #total charges has null values not removing nulls since I wont be using it in the models

#checking the overall distribution of the dependent variable
ggplot(df,aes(Churn)) + geom_bar(stat="count")

#recoding churn
df$Churn <- as.factor(recode(df$Churn, Yes = 1, No = 0))

#Data cleaning
df <- subset(df, select = -c(customerID,TotalCharges))

#Subsetting the data for phone, internet and both the services
phone <- df %>% filter(PhoneService == "Yes" & InternetService == "No")
internet <- df %>% filter(PhoneService == "No" & (InternetService == "DSL" | InternetService == "Fiber optic"))
both <- df %>% filter(PhoneService == "Yes" & (InternetService == "DSL" | InternetService == "Fiber optic"))

#removing unnecessary columns in the subsetted dataframes
phone <- subset(phone, select = -c(PhoneService,InternetService,OnlineSecurity,OnlineBackup,DeviceProtection, TechSupport,StreamingTV,StreamingMovies))
internet <- subset(internet, select = -c(PhoneService,MultipleLines,InternetService))
both <- subset(both, select = -c(PhoneService))

#factorising the independent variables
names <-c('gender','SeniorCitizen','Partner','Dependents','MultipleLines','Contract','PaperlessBilling','PaymentMethod')
phone[,names] <- lapply(phone[,names],factor)
names <-c('gender','SeniorCitizen','Partner','Dependents','OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies','Contract','PaperlessBilling','PaymentMethod')
internet[,names] <- lapply(internet[,names],factor)
names <-c('gender','SeniorCitizen','Partner','Dependents','MultipleLines','InternetService','OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies','Contract','PaperlessBilling','PaymentMethod')
both[,names] <- lapply(both[,names],factor)

#EDA for the three dataframes
summary(phone) 
summary(internet)
summary(both)


cowplot::plot_grid(ggplot(df, aes(x=gender,fill=Churn))+ geom_bar(), 
                   ggplot(df, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(df, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(df, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(df, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(df, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+
                     scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                   align = "h")

cowplot::plot_grid(ggplot(df, aes(x=InternetService,fill=Churn))+ geom_bar(), 
                   ggplot(df, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(df, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(df, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(df, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(df, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+
                     scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                   align = "h")


cowplot::plot_grid(ggplot(phone, aes(Churn))+ geom_bar() + xlab("Phone Churn"), 
                           ggplot(internet, aes(Churn))+ geom_bar() + xlab("Internet Churn"),
                           ggplot(both, aes(Churn))+ geom_bar() + xlab("Both Churn") +
                        scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                          align = "h")

table(phone$Churn)
table(internet$Churn)
table(both$Churn)

#--------------- telephone --------------------------------------------------


#EDA 
summary(phone)
table(phone$Churn)

cowplot::plot_grid(
                   ggplot(phone, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(phone, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(phone, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(phone, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(phone, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(phone, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill'),
                   ggplot(phone, aes(x=Churn,y=tenure)) + geom_boxplot(),
                   ggplot(phone, aes(x=Churn,y=MonthlyCharges)) + geom_boxplot()+
                     scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
                   align = "h")

ggplot(phone, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')

#Splitting the data 
set.seed(1024)
trainindex <- sample(1:nrow(phone), size = round(0.75*nrow(phone)), replace = FALSE)
train <- phone[trainindex,]
test <- phone[-trainindex,]
dim(train)
dim(test)
table(train$Churn)
table(test$Churn)
summary(train)

#building the model
p_logit <- glm(Churn ~ SeniorCitizen + Dependents + tenure + MultipleLines + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, family = binomial(link = "logit"),data = train )
summary(p_logit)
car::vif(p_logit)
lmtest::dwtest(p_logit)

#predicting for test data
test_x <- subset( test, select = -c(Churn))
predlogit <- predict(p_logit, newdata=test_x, type="response")
predlogit
predlogit <- ifelse(predlogit>0.2, 1, 0) #predicted 

#Evaluating the model
ConfusionMatrix <- table(predlogit,test$Churn)
ClassificationError <- mean(predlogit != test$Churn) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuracy rate
ConfusionMatrix


#precision_recall curve
pr <- prediction(predlogit, test$Churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)  

#Computing AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Computing precision, recall and F1-score
TP = ConfusionMatrix[2,2]
TN = ConfusionMatrix[1,1]
FN = ConfusionMatrix[1,2]
FP = ConfusionMatrix[2,1]

precision<- round(TP / (TP + FP), 2)
recall<- round(TP / (TP + FN), 2)
f1_score<- round((2 * precision * recall) / (precision + recall), 2)
precision
recall
f1_score


#-----------------------------internet------------------------------------------------
#EDA
summary(internet)
table(internet$Churn)

cowplot::plot_grid(
  ggplot(internet, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(internet, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(internet, aes(x=PaperlessBilling,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(internet, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(internet, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(internet, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(internet, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(internet, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(internet, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(internet, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(internet, aes(x=Churn,y=tenure)) + geom_boxplot(),
  ggplot(internet, aes(x=Churn,y=MonthlyCharges)) + geom_boxplot()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
  align = "h")
ggplot(internet, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')

#Splitting the data 
set.seed(1024)
trainindex <- sample(1:nrow(internet), size = round(0.75*nrow(internet)), replace = FALSE)
train <- internet[trainindex,]
test <- internet[-trainindex,]
dim(train)
dim(test)
table(train$Churn)
table(test$Churn)
summary(train)

#building the model
i_logit <- glm(Churn ~ SeniorCitizen + Dependents + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + tenure + Contract + PaperlessBilling + StreamingTV + PaymentMethod + MonthlyCharges, family = binomial(link = "logit"),data = train )
summary(i_logit)
car::vif(i_logit)
lmtest::dwtest(i_logit)

#predicting for test data
test_x <- subset(test, select = -c(Churn))
predlogit <- predict(i_logit, newdata=test_x, type="response")
predlogit
predlogit <- ifelse(predlogit>0.2, 1, 0) #predicted 

#Evaluating the model
ConfusionMatrix <- table(predlogit,test$Churn)
ClassificationError <- mean(predlogit != test$Churn) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuracy rate
ConfusionMatrix

#precision_recall curve
pr <- prediction(predlogit, test$Churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)  

#Computing AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Computing precision, recall and F1-score
TP = ConfusionMatrix[2,2]
TN = ConfusionMatrix[1,1]
FN = ConfusionMatrix[1,2]
FP = ConfusionMatrix[2,1]

precision<- round(TP / (TP + FP), 2)
recall<- round(TP / (TP + FN), 2)
f1_score<- round((2 * precision * recall) / (precision + recall), 2)
precision
recall
f1_score

#--------------------------------------Both-----------------------------------------
#EDA
summary(both)
table(both$Churn)

cowplot::plot_grid(
  ggplot(both, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(both, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(both, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(both, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(both, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(both, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(both, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(both, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(both, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(both, aes(x=Contract,fill=Churn))+ geom_bar(position = 'fill'),
  ggplot(both, aes(x=Churn,y=tenure)) + geom_boxplot(),
  ggplot(both, aes(x=Churn,y=MonthlyCharges)) + geom_boxplot()+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
  align = "h")
ggplot(internet, aes(x=PaymentMethod,fill=Churn))+ geom_bar(position = 'fill')

#Splitting the data 
set.seed(1024)
trainindex <- sample(1:nrow(both), size = round(0.75*nrow(both)), replace = FALSE)
train <- both[trainindex,]
test <- both[-trainindex,]
dim(train)
dim(test)
table(train$Churn)
table(test$Churn)
summary(train)

#building the model
b_logit <- glm(Churn ~ SeniorCitizen + Dependents + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + tenure + Contract + PaperlessBilling + StreamingTV + PaymentMethod + MonthlyCharges, family = binomial(link = "logit"),data = train )
summary(b_logit)
car::vif(b_logit)
lmtest::dwtest(b_logit)

#predicting on the test data
test_x <- subset( test, select = -c(Churn))
predlogit <- predict(b_logit, newdata=test_x, type="response")
predlogit
predlogit <- ifelse(predlogit>0.2, 1, 0) #predicted 

#Evaluating the model
ConfusionMatrix <- table(predlogit,test$Churn)
ClassificationError <- mean(predlogit != test$Churn) # Classification error
print(paste("Accuracy = ", 1-ClassificationError))        # Accuracy rate
ConfusionMatrix

pr <- prediction(predlogit, test$Churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)  

#Computing AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Computing precision, recall and F1-score
TP = ConfusionMatrix[2,2]
TN = ConfusionMatrix[1,1]
FN = ConfusionMatrix[1,2]
FP = ConfusionMatrix[2,1]

precision<- round(TP / (TP + FP), 2)
recall<- round(TP / (TP + FN), 2)
f1_score<- round((2 * precision * recall) / (precision + recall), 2)
precision
recall
f1_score

stargazer::stargazer(p_logit,i_logit,b_logit,type="text")

