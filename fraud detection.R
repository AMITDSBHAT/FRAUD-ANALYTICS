##CLENING THE ENVIRONMENT####
rm(list = ls())
###IMPORTING LIBRARIES############
library(dplyr)
#install.packages("DMwR")
library(DMwR)
#install.packages("caret")
library(caret)

###IMPORTING DATA##########
setwd("C:\\Users\\DSAMIT\\Desktop\\DATA SCIENCE\\KAGGLE\\FRAUD DETECTION\\creditcard.csv")
getwd()

df = read.csv("C:\\Users\\DSAMIT\\Desktop\\DATA SCIENCE\\KAGGLE\\FRAUD DETECTION\\creditcard.csv\\creditcard.csv")

##DATA EXPLORATION##########
##checking the number of missing values
sapply(df,function(x) { sum(is.na(x))})

str(df)

head(df)

##no missing in all variables, else might have to apply missing value imputation using packages like MICE, HMISC,MISS FOREST,MI AND AMELIA

##checking outliers 
##treating outliers


##DATA PREPARATION#########
##checking the mean and sd if scaling(normalization or standardization) is required
df1 = df[,sapply(df,is.numeric)]
sapply(df,mean)
sapply(df,sd)
summarise_all(df,funs(mean,sd))

##scaling Time and Amount as they are not scaled
df$Time = with(df, (Time-mean(Time))/sd(Time))
df$Amount = with(df, (Amount-mean(Amount))/sd(Amount))

##standarization is prefered over normalization as it shrinks the data within some range and hence outlier information is lost
##outlier effects the parametric tests,classical approaches like linear,logistic regression and LDA   
##if assumptions on functional form(may be linearity,normality etc)OR parametric tests in not met they might not perform well 
##so data has to be prepared to follow the assumptions of parametric tests
##data preparation is not much needed for non-parametric tests(like decision trees and random forest).

##FEATURE ENGINEERING#######
##time can be created as day , hour and weekday basis

###SPLITTING DATA INTO TRAIN AND TEST SETS###########
set.seed(123)
ind = sample(nrow(df),0.7*nrow(df),replace = FALSE)
train = df[ind,]
test = df[-ind,]

#checking the proportion of 0's and 1s in training and test sets
table(train$Class)
prop.table(table(train$Class))
train_prop=as.data.frame(table(train$Class))
prop_train =  train_prop$Freq[2]/train_prop$Freq[1] 
print(prop_train)

table(test$Class)
prop.table(table(test$Class))
test_prop=as.data.frame(table(test$Class))
prop_test =  test_prop$Freq[2]/test_prop$Freq[1]
print(prop_test)


####K-FOLD CROSS-VALIDATION WITH SMOTE AND ROSE############
##oversampling using SMOTE
cv_plus_os = function(k)
{
  for(i in 1 : k)
  {
train$Class = as.factor(train$Class)
table(train$Class)
prop.table(table(train$Class))
trainsplit = SMOTE(Class ~ .,data = train,perc.over = 400,perc.under = 200)
table(trainsplit$Class)
prop.table(table(trainsplit$Class))

nr= nrow(trainsplit)
n=10
splitted_df=split(trainsplit, rep(1:ceiling(nr/n), each=n, length.out=nr))
train_cv = splitted_df[-i]
test_cv = splitted_df[i]

set.seed(123) 
train.control <- trainControl(method = "cv",number = 10,repeats = 2)
  }
}

