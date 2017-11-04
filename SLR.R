
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL - ACTIVITY-------------------------------------##
##----------------------------------------------------------------------------------##


##--- Step 1: Clear environment variables ------------------------------------------##
rm(list=ls(all=TRUE))
##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##
setwd("~/Documents/Lakshmi_Work/ACADEMICS/Batch_32/CSE7202c/SimpleLinearRegression/20170902_Batch32_CSE7302c_Lab01_SimpleLinReg")
##__________________________________________________________________________________##


##--- Step 3: Read the data from the csv file --------------------------------------##
cars_data = read.csv(file="Toyota_SimpleReg.csv", header=T)
names(cars_data)
##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop the Id, Model attributes:
cars_data = cars_data[,-c(1,2)]

## Summary of the data and look for any missing values:
str(cars_data)
summary(cars_data)
#No missing values

## Correlation and Covariance between the attributes:
cov(cars_data)
#The covariance of the Age of car and Price is -59136.11. 
#It indicates a negative linear relationship between the two variables. 
#This relation could be observed from the scatter plot also.
plot(cars_data$Age_06_15, cars_data$Price)
plot(cars_data$Age_06_15, cars_data$Price, xlab = "Age of the car", ylab = "Price in ($)", pch = 18, col = "blue")

cor(cars_data)
cor(cars_data$Age_06_15, cars_data$Price)
#The correlation coefficient of the Age of car and Price is -0.8765905. 
#Since the value is close to 1 and has a -ve sign, we can conclude that the variables are strongly negatively correlated.
##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (70:30) ratio
rows = seq(1, nrow(cars_data),1)
set.seed(123)
trainRows = sample(rows,(70*nrow(cars_data))/100)
cars_train = cars_data[trainRows,] 
cars_test = cars_data[-trainRows,]
##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##
LinReg = lm(Price ~ Age_06_15, data = cars_train)
coefficients(LinReg)

## Summary of model:
summary(LinReg)

#To extract the coefficients:
coefficients(LinReg)
coefficients(LinReg)[1]
coefficients(LinReg)[2]
names(coefficients(LinReg))
#To extract the residuals:
LinReg$residuals
#To extract the train predictions:
LinReg$fitted.values
##__________________________________________________________________________________##


##--- Step 7: Check for validity of linear regression assumptions ------------------##
par(mfrow = c(2,2))
plot(LinReg)
par(mfrow = c(1,1))
##__________________________________________________________________________________##
# The plot of the Linear Regression model gives insight into the residuals,the normality of the errors(Q-Q plot)
# It also shows the homoscedasticity(homogeneity in variance) of the data and finally the cooks distance displays potential outliers

##--- Step 8: Predict on testdata --------------------------------------------------##
test_prediction = predict(LinReg, cars_test)
test_actual = cars_test$Price
##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##
library(DMwR)
#Error verification on train data
regr.eval(cars_train$Price, LinReg$fitted.values)

#Error verification on test data
regr.eval(test_actual, test_prediction)
##__________________________________________________________________________________##

#The evaluation function can be used to calculate the error metrics on the fitted data vs actual data as well as the 
# predictions made on the test vs actual values.
# mae : gives average error of model attaching equal weights to all errors
# mse : gves avg square error and penalizes larger errors
# rmse: gives the root mean square error and squaring penalizes larger errors
#  mape: gives error in terms of percentage and is generally more intuitive


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
# Confidence Intervals talk about the average values intervals
# Prediction Intervals talk about the all individual values intervals
Conf_Pred = data.frame(predict(LinReg, cars_test, interval="confidence",level=0.95))
Pred_Pred = data.frame(predict(LinReg, cars_test, interval="prediction",level=0.95))

names(Conf_Pred)

plot(cars_test$Age_06_15, cars_test$Price, xlab = "Age of the car", ylab = "Price in ($)")

points(cars_test$Age_06_15,Conf_Pred$fit,type="l", col="green", lwd=2)
points(cars_test$Age_06_15,Conf_Pred$lwr,pch="-", col="red", lwd=4)
points(cars_test$Age_06_15,Conf_Pred$upr,pch="-", col="red", lwd=4)
points(cars_test$Age_06_15,Pred_Pred$lwr,pch="-", col="blue", lwd=4)
points(cars_test$Age_06_15,Pred_Pred$upr,pch="-", col="blue", lwd=4)
##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##