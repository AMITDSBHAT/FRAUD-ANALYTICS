##CLENING THE ENVIRONMENT####
rm(list = ls())
###IMPORTING LIBRARIES############
#install.packages("caret")
#install.packages("DMwR")

library(dplyr)
library(DMwR)
library(caret)
library(car)
library(data.table)
library(MLmetrics)

###IMPORTING DATA##########
setwd("C:\\Users\\amitkumar.s\\Desktop\\DATA SCIENCE\\KAGGLE\\CLASSIFICATION\\creditcardfraud")
getwd()

df = read.csv("C:\\Users\\amitkumar.s\\Desktop\\DATA SCIENCE\\KAGGLE\\CLASSIFICATION\\creditcardfraud\\creditcard.csv")

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
model_selection = function(splits)
  
{   f1 = c()
    nr= nrow(train)
    splits = splits
    n = nr / splits
    
    splitted_train=split(train, rep(1:ceiling(splits), each=n, length.out=nr))
   
    f1_score1=c()
    f1_score2=c()
    
    for(i in 1:splits)
    {      
    validation_cv = splitted_train[[i]]
    train_list = splitted_train[-i]
    train_cv = as.data.frame((rbindlist(train_list)))
    
##excluding the validation set and oversampling only the training set in each cv iteration, to avoid overfitting and effective model building
##oversampling only the training set in each iteration prevents the oversampled data to flow into validation set    
##main intension of CV is to test/assess the built model on unseen data, so in each iteration validation set is unseen data in CV
##so oversampling only training set after splitting into training and validation set in each iteration
    
    train_cv$Class = as.factor(train_cv$Class)
    table(train_cv$Class)
    prop.table(table(train_cv$Class))
    train_cv_os = SMOTE(Class ~ .,data = train_cv,perc.over = 1500,perc.under = 120)
    table(train_cv_os$Class)
    prop.table(table(train_cv_os$Class))
    
    #set.seed(123) 
    #train_control <- trainControl(method = "cv",number = 10)
    
    
    #iteration1 for logistic regression
    log_model1 <- glm(Class~.,data=train_cv_os, family="binomial")
    varImp(log_model1)
    vif1 = vif(log_model1)
    print(log_model1)
    summary(log_model1)
    pred1 = predict(log_model1, newdata=validation_cv)
    threshold = 0.5
    pred1 = ifelse(pred1>threshold,1,0)
    pred1 = as.factor(as.character(pred1))
    act1 = as.factor(as.character(validation_cv$Class))
    cm1=confusionMatrix(act1,pred1)
    print(cm1)
    f11 = F1_Score(act1,pred1)
    f1_score1=c(f1_score1,f11)
    print(f1_score1)
    
    ##iteration2 for logistic regression
    log_model2 <- glm(Class ~ Time+V11+V12+V13+V19+V21+V25+V26+V28, data=train_cv_os,family="binomial")
    varImp(log_model2)
    vif2 = vif(log_model2)
    print(log_model2)
    summary(log_model2)
    pred2 = predict(log_model2, newdata=validation_cv)
    threshold = 0.5
    pred2 = ifelse(pred2>threshold,1,0)
    pred2 = as.factor(as.character(pred2))
    act2 = as.factor(as.character(validation_cv$Class))
    cm2=confusionMatrix(act2,pred2)
    print(cm2)
    f12 = F1_Score(act2,pred2)
    f1_score2=c(f1_score2,f12)
    print(f1_score2)
    }    
    
f1_score1_avg = mean(f1_score1)
f1_score2_avg = mean(f1_score2)
f1 = c(f1_score1_avg,f1_score2_avg)
return(f1)
}

final = model_selection(splits = 2)
