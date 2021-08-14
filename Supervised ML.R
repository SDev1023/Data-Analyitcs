#--------------importing the libraries------------------------------------
library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)
library(ggplot2)

#----------------setting the directory and reading the file --------------------
setwd("D:/Internship_Sparks Foundation")
getwd()
data<-read.csv("Task1.csv",stringsAsFactors = FALSE, as.is = TRUE, header = TRUE)

#-----------------making a copy of the data------------------------------------
data1<-data

#-----------------Exploring the data-------------------------------------------
str(data1)
dim(data1)
summary(data1)
head(data1)

#----------------Renaming the independent variable-----------------------------
colnames(data1)[which(names(data)=="ï..Hours")]="hours"

#-------------------plotting the graph-----------------------------------------

ggplot(data=data1,aes(x=hours, y = Scores))+ 
       geom_point() + geom_smooth(method = "lm", se = FALSE)

#------------- Checking for outliers through quantile method-------------------
quantile(data1$Scores,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))

#We can see from the method that there are no oultiers in our data.So we can procced with our analysis

#-----------------Splitting the data into training and testing data set--------
set.seed(123)#This is used to produce reproducible results, every time we run the model

spl = sample.split(data1$Scores, 0.7)#Splits the overall data into train and test data in 70:30 ratio

train.data = subset(data1, spl == TRUE)
str(train.data)
dim(train.data)

test.data = subset(data1, spl == FALSE)
str(test.data)
dim(test.data)

#--------------Using the iteration in training data---------------------------

LinearModel0=lm(Scores~.,data=train.data)
summary(LinearModel0)
#We can see from the p value of our model and using the T test, that the independent variable hours 
#is significant at 9.99%. The value of the R-squared is also closer to 1 which
#shows that our model is a good fit and almost 95% of the variation in scores is 
# explained by the number of hours a student studies......


#----------------Using the iteration in Test Data------------------------------

Finalmodel=lm(Scores~.,data=test.data)
summary(Finalmodel)

#We can see form the p value of our model  and using the T test, that the independent variable hours 
#is significant at 99.99%.  However The value of the R-squared has decreased 
#slightly but is still almost closer to 1 which 
#shows that our model is a good fit and almost 91% of the variation in scores is 
#explained by the number of hours a student studies......

#------------------Using the predict command to predict the scores------------
Predicted_Scores<-predict(Finalmodel)
test.data$Predicted_Scores<-Predicted_Scores
Predicted_Scores

#---------Writing the predicted scores in CSV format---------------------------
write.csv(test.data, "scores_pred.csv", row.names = FALSE)

#----------Predicting the score if student studies for 9.25hours/day--------
a1<-data.frame(hours=9.25)
predicted_score<-predict(Finalmodel,a1)
predicted_score

#The predicted score is 93.77 if a student studies for 9.25hours/day






