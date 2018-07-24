rm(list=ls())

y = c(11,22,33,44,55,66,67,78,89,100)
x= c(1,4,2,4,2,5,3,2,2,4)
df = data.frame(y,x)

lr = lm(formula = y~x,data = df)
summary(lr)


x1 = (x-mean(x))/sd(x)
df1 = data.frame(y,x1)
lr1 = lm(formula = y~x1,data = df1)
summary(lr1)

sx = sd(x)

#RELATIONSHIP BETWEEN STANDARDIZED AND NON-STANDARDIZED coefficients
STZ_BETA = sd_x * NON_STZ_BETA