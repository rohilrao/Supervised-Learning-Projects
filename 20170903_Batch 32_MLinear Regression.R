#Setting the working directory
setwd("D:\\Insofe Academics\\CPEE\\Batch 32\\CSE 7202c\\MLR")

getwd()


setwd("C:/Users/rohil/Downloads/INSOFE/Labs/CSE 7302 Decision modelling Stats and Prob/20170903_Batch_32_CSE7302c_MLinear_Regression/20170903_Batch 32_CSE7302c_MLinear Regression")
###Reading the train data and test data (evaluation)###
train_data<-read.csv("CustomerData.csv",header=T,sep=",")
test_data<-read.csv("Eval.csv",header=T)


library(car)
boxplot(train_data)


###Understanding the data### 
str(train_data)
summary(train_data)

###Are all the variables with appropriate data type. If not we need to convert them###
#Observe that the city variable is numeric type. Convert that to factor
train_data$City<-as.factor(as.character(train_data$City))

test_data$City<-as.factor(as.character(test_data$City))
###Looking at the summary of the train_train_data###
summary(train_data)

###Do you observe any anomalies###

#Observe that the maximum values for age is 113. Is it an outlier
#Lets draw boxplots for it
boxplot(train_data$MinAgeOfChild,train_data$MaxAgeOfChild)

###Remove the extreme points from the analysis###
train_data1<-train_data[!((train_data$MinAgeOfChild== max(train_data$MinAgeOfChild))|(train_data$MinAgeOfChild== max(train_data$MinAgeOfChild))),]

###Build the Linear Regression Model and obtain the summary###
mod_lm<-lm(TotalRevenueGenerated~.,data=train_data1[,-1])
summary(mod_lm)


###Interpretation of the output###
#Residuals
#Estimates, SE, t-values, p-values 
#F statistic and p-value


###Plot the model###
par(mfrow=c(2,2))
plot(mod_lm)

###What are your observations###

### Make Predictions using the model###
pred_1<-predict(mod_lm,newdata=train_data1)
regr.eval(train_data1$TotalRevenueGenerated,pred_1)
pred_2<-predict(mod_lm,newdata=test_data)

###Evaluation on metric###
library(DMwR)
regr.eval(test_data$TotalRevenueGenerated,pred_2)


###Are the the variables really significant in explaining the 
##dependent variable/target. Can we reduce the dimensions
##Lets try to identify important variables in explaining the y
library(car)
vif(mod_lm)

library(MASS)
stepAIC(mod_lm)
mod_lm_mass<-lm(formula = TotalRevenueGenerated ~ City + NoOfChildren + MinAgeOfChild + 
                  Tenure + FrquncyOfPurchase + NoOfUnitsPurchased + FrequencyOFPlay + 
                  NoOfGamesPlayed + NoOfGamesBought+FavoriteChannelOfTransaction+
                  FavoriteGame,data = train_data)
summary(mod_lm_mass)

####What to be done with extreme points observed in the plots###

#Remove the points that are extreme.. Do we need to remove the points at all in the first place?
#What you observe in the plots are the rownames.
ext=c(1764,974,667)
train_data2<-train_data1[!rownames(train_data1)%in%ext,]
mod_lm_2<-lm(TotalRevenueGenerated~.,data=train_data2[,-1])
summary(mod_lm_2)
plot(mod_lm_2)
pred_3<-predict(mod_lm_2,newdata=train_data2)
regr.eval(train_data2$TotalRevenueGenerated,pred_3)[4] #Removed extreme point
regr.eval(train_data1$TotalRevenueGenerated,pred_1)[4]

pred_4<-predict(mod_lm_2,newdata=test_data)
regr.eval(test_data$TotalRevenueGenerated,pred_4)[4] #Removed extreme points
regr.eval(test_data$TotalRevenueGenerated,pred_2)[4]



###############
#Can we improve the accuracies if transformations are done
#One of the transformations- target transformation-log transformation


train_data2$target<-log(train_data2$TotalRevenueGenerated)
test_data$target<-log(test_data$TotalRevenueGenerated)
train_data3<-train_data2[,-13]
test1<-test_data[,-13]
mod_lm_log<-lm(target~.,data=train_data3[,-1])
pred1_log<-exp(predict(mod_lm_log,newdata=train_data3))
regr.eval(train_data2$TotalRevenueGenerated,pred1_log)[4]
summary(mod_lm_log)

pred2_log<-exp(predict(mod_lm_log,newdata=test1))
regr.eval(test_data$TotalRevenueGenerated,pred2_log)[4]

#Perform StepAIC on the log transformed model and see if the result improves
stepAIC(mod_lm_log)
mod_log_aic<-lm(formula = target ~ City + NoOfChildren + MinAgeOfChild + FrquncyOfPurchase + 
                  NoOfUnitsPurchased + FrequencyOFPlay + NoOfGamesPlayed + 
                  NoOfGamesBought + FavoriteChannelOfTransaction + FavoriteGame, 
                data = train_data3)

pred3_log<-exp(predict(mod_log_aic,newdata=train_data3))
regr.eval(train_data2$TotalRevenueGenerated,pred3_log)[4]

pred4_log<-exp(predict(mod_log_aic,newdata=test1))
regr.eval(test$TotalRevenueGenerated,pred4_log)[4]

