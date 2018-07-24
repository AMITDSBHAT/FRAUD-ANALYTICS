##########REGRESSION CHALLENGE###########
#####Reading xlsx file#######
rm(list = ls())
install.packages("xlsx")
library(xlsx)
setwd("C:\\Users\\amitkumar.s\\Desktop\\DATA SCIENCE\\CONCEPTS BY DISCUSSION")
df = read.xlsx("REGRESSION CHALLENGE.xlsx", sheetName = "Sheet1",startRow = 2,endRow = 62)

library(dplyr)
df = df %>% rename(y=GDP_actual,x=Inflation) 
###LINEAR REGRESSION#####
lin_reg = lm(y~x,data = df)
gen_lin_reg = glm(y~x,data = df)
summary(lin_reg)
summary(gen_lin_reg)#very poor results

########RANDOM FOREST############
library(randomForest)
set.seed(71) 
rf <-randomForest(y~x,data=df, ntree=1000) 
print(rf)
summary(rf)

