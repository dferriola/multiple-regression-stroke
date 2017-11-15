
#Perform multiple linear regression using 95% confidence level. State your your hypothesis, test statistics, p-value, and conclusion. Plot graphs and interpret them.
#Which predictors will cause you to reject the null hypothesis? (Give the interpretation of each coefficient in the model).
#Now, try a different model (e.g. include more predictors or use less predictors). Which model fits the data better? What is your selected model? How did you select the model. Explain your answers.
#Address any other concerns you might have.
library(corrplot)
library(tidyverse)
#import dataset
df=read.table(file.choose(),sep = "\t", header=T)
df1=df[,c(4:15)]
str(df)
df1_corr=cor(df1)
corrplot(df1_corr)
modelkenny=lm(Kenny~., data=df1)
summary(modelkenny)
modelbobath=lm(Bobath~., data=df1)
summary(modelbobath)
modelbarthel=lm(Barthel~., data=df1)
summary(modelbarthel)
#kenny and bobath seem to be highly correlated with each other
plot(df)
attach(df)
summary(df)
#the outcome we would like to predict is the kenny, bobath, and bathel scoring system of daily living which is out of 24
names(df)
model1=lm(Kenny~Age+Balance)
summary(model1)
#This model of Age + Balance gives a R squared of 0.7399 

model2=lm(Kenny~Balance+Bobath)
summary(model2)
#this second model is even less 

coefficients(model2) #model coefficients
confint(model2, level=0.95)#CIs for model parameters
fitted(model2)#predicted values
residuals(model2) 
anova(model2) #anova table
vcov(model2)#covariance matrix 
influence(model2)#regression diagnostics
ggplot(df, aes(y=Bobath, x=Balance, colour=Sex))+geom_point()

#Including more variables
model3=lm(Kenny~Balance+Bobath+Sensation+Age)
summary(model3)