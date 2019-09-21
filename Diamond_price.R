install.packages('caTools')
install.packages('tree')
library(tree) # For decision tree
library(caTools) # For splitting the data 
library(DataExplorer) # For the missing values
library(psych)  #describe() function
library(ggplot2)
library(lattice) # required for the xyplot() function
library(MASS) #StepAIC is function
install.packages('randomForest')
library(randomForest)
# Reading the file in Variable called df 
df = read.csv(file.choose(),header = T)

#######################################
#Data Quality Check
######################################
# examining the structure of the initial data frame
print(str(df))
#We can see there are 7 variables and 425 observations 
#Price being Dependent Variable and rest being the Dependent Variable
#Doing a high level data exploration
print(summary(df))
#The Value of carat varies from 0.2 to 2.48
#The Colour Varies from 1 to 9 and it takes only Integer Value
#Cut is Categorical variable with two states ideal and non Ideal
#Channel is Categorical variable having 3 levels
#Store is also Categorical Variable having 7 levels 
#Price is Continous Variable having minimum value as 497 and maximum value as 27575
describe(df)
#from this we infer that Price Variable has high range when conpared to other Variables present
#There is little bit of skewness and Kurtosis present in all the indeoendent variables but is 
#very high in the proce variable 
#######Checking for the missing value 
plot_missing(df)
sapply(df, function(df) sum(is.na(df)))
#As we can see there is no missing value in the dataset
#Graphical Data Exploration of the dependent Varaible Price
par(mfrow=c(1,2))
hist(df$price, col = "red", xlab = "Price", main = "Price_Histogram")
boxplot(df$price, col = "red", main = "Price BoxPlot")
#As can be seen from both histogram and Boxplot there is presence of fair amount of outlier in the price variable 

par(mfrow=c(2,2))
hist(df$carat, col = "red", xlab = "Carat", main = "Carat_Histogram")
boxplot(df$carat, col = "red", main = "Carat BoxPlot")
hist(df$color, col = "red", xlab = "COLOR", main = "COLOR_Histogram")
boxplot(df$color, col = "red", main = "COLOR BoxPlot")
#Carat is little bit skewed with presence of few outlier
g1=ggplot(df,aes(color))+geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)  
#Color distribution is more uniform with lot of data lying between 0-6 and there is no outlier and 10 of the datasets is at 8 & 9 
print(g1)

par(mfrow=c(1,2))
hist(df$clarity, col = "red", xlab = "Clarity", main = "Clarity_Histogram")
boxplot(df$clarity, col = "red", main = "Clarity BoxPlot")
#Clarity is normally distributed with small values of skew, there is also not outlier present.

g2=ggplot(df,aes(cut))+geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)  
print(g2)
#63.76% of data of diamond is not ideal cut
g3=ggplot(df,aes(channel))+geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)  
print(g3)
#Most of the diamonds are sold online i.e 74.82% of diamonds are sold online
g4=ggplot(df,aes(store))+geom_bar()+geom_text(stat='count', aes(label=..count..), vjust=-1)  
print(g4)
#Only Ashford and Blue Nile are best stores and are the outliers in terms of sales when compared to other stores
#########################################################################################
#Data Exploration
#########################################################################################
#As our predictor that is "Price" a continuous variable we can use EDA based on regression as we have to predict the 
#continous variable
ggplot(aes(x=carat, y=price), data=df) +
  geom_point(fill=I("#F79420"), color=I("black"), shape=21) +
  scale_x_continuous(lim = c(0, quantile(diamonds$carat, 0.99)) ) +
  scale_y_continuous(lim = c(0, quantile(diamonds$price, 0.99)) ) +
  stat_smooth(method="lm")+
  ggtitle("Diamonds: Price vs. Carat")
#Write something here
xyplot(jitter(price) ~ jitter(carat) | channel + cut, 
       data = df,
       aspect = 1, 
       layout = c(3, 2),
       strip=function(...) strip.default(..., style=1),
       xlab = "Size or Weight of Diamond (carats)", 
       ylab = "Price")
#As we see there is strong realtionship between price and carat no matter channel and cut 
xyplot(jitter(price) ~ jitter(color) | channel + cut, 
       data = df,
       aspect = 1, 
       layout = c(3, 2),
       strip=function(...) strip.default(..., style=1),
       xlab = "Color", 
       ylab = "Price")
#As we see there is strong realtionship between price and color no matter channel and cut 
xyplot(jitter(price) ~ jitter(clarity) | channel + cut, 
       data = df,
       aspect = 1, 
       layout = c(3, 2),
       strip=function(...) strip.default(..., style=1),
       xlab = "Clarity", 
       ylab = "Price")
#As we see there is strong relationship between price and clarity no matter channel and cut 

#####################################################################################################
#Model Based EDA 
#####################################################################################################
###Spliting the data into train and test 
set.seed(123)
sample = sample.split(df$carat,SplitRatio = 0.70)
train = subset(df,sample ==TRUE)
test = subset(df,sample==FALSE)
model1 =lm(price~.,data=train)
summary(model1)
#This is our base model wherein we have added all the variables now through backward selection we need to eliminate certain variables and keep only 
#those predictor variables which are significant 
#The important and significant predictors are carat which is postively related to price,color which is negatively related to price,clarity again is 
#negatively related to price 
#Ideal cut increases the price
#The Price of Diamond varies with the channel On internet the prices are less when compared to independent and in mall prices are high when 
#compared with Independent and Internet Channel 
#We also see that there are few of stores has significance and affect the prices of the Diamond 
#But when we combine Ashford and Blue Nile we discover they are nothing but represented by channel Internet 
#Channel Internet ------ is 318 
#Store Ashford and Blue Nile --------- is 318
#Store and Channel represent same information so we will eliminate store from further regression
par(mfrow=c(2,2))
plot(model1)
#we see the plot residual vs fitted is non-linear so we expect that there non linear relation between price and carat.
#Log-transformation to curtail the outliers:
model2 = lm(log(price)~carat+color+clarity+cut+channel,data = train)
summary(model2)
par(mfrow = c(2,2))
plot(model2)
#After log transformation we see that there is less effect of outlier on the model  

#Lets make a carat predictor tranformation
train$carat_square = (train$carat)^2
test$carat_square = (test$carat)^2
train$log_price_train = log(train$price)
test$log_price_test = log(test$price)
model3 = lm(log_price_train~carat+carat_square+color+clarity+cut+channel,data = train)
summary(model3)
par(mfrow = c(2,2))
plot(model3)
#After this response and predictor transformation we have all the variables and significant and adjust R-square of 94.79%
#Model3 is final user defined model.
#####################################
#StepAIC Model
####################################
step <- stepAIC(model3, direction="both")
#Step$anova # display results
coefficients(step) # Model coefficients
summary(step)
#The StepAIC model provides same coeffiecient and adjusted R-square as our generated Model (Model3).
#######################
#Decision Tree Model
######################
tree.diamond = tree(price~carat+carat_square+color+clarity+cut+channel,train)
summary(tree.diamond)
plot(tree.diamond)
text(tree.diamond,pretty = 0)
#As we see carat,clarity and color are important class seperators



#################################################
#Model Development
################################################
#Model1
Reg1 = lm(price~carat,data=train)
summary(Reg1)
sqrt(mean(sum(Reg1$residuals^2)))
#Model2 
Reg2 = lm(log_price_train~carat+carat_square+color+clarity+cut+channel,data = train)
summary(Reg2)
sqrt(mean(sum(Reg2$residuals^2)))
#Model 3
tree.diamond = tree(log_price_train~carat+color+clarity+cut+channel,data = train)
summary(tree.diamond)
#rpart.plot(tree.diamond)
plot(tree.diamond)
text(tree.diamond,pretty = 0)
y_predicted = predict(tree.diamond,data = train)
sqrt(mean(sum((y_predicted-train$log_price_train)^2)))
#Model 4 Random Forest
set.seed (1)
bag.diamond =randomForest(log_price_train~carat+color+clarity+cut+channel,data=train,mtry=5, importance =TRUE)
bag.diamond
plot(bag.diamond)
text(bag.diamond,pretty = 0)
#Checking the importance of each variable
importance(bag.diamond)
#########################
#Performance
########################
#Training data performance of Model 1 
summary(Reg1)
RMSE_Reg1_Train=sqrt(mean(sum(Reg1$residuals^2)))
print(RMSE_Reg1_Train)
#Test data performance of Model 1
y_predicted_reg1=predict(Reg1,test)
RMSE_Reg1_Test=sqrt(mean(sum((y_predicted_reg1-test$price)^2)))
print(RMSE_Reg1_Test)

#Training data performance of Model 2 
summary(Reg2)
RMSE_Reg2_Train=sqrt(mean(sum(Reg2$residuals^2)))
print(RMSE_Reg2_Train)
#Test data performance of Model 2
y_predicted_reg2=predict(Reg2,test)
RMSE_Reg2_Test=sqrt(mean(sum((y_predicted_reg2-test$log_price_test)^2)))
print(RMSE_Reg2_Test)

#Training data performance of Model 3
y_predicted_tree.diamond = predict(tree.diamond,data = train)
RMSE_tree.diamond_train=sqrt(mean(sum((y_predicted_tree.diamond-train$log_price_train)^2)))
print(RMSE_tree.diamond_train)
#Testing data performance of Model 3
y_predicted_tree.diamond_test = predict(tree.diamond,test)
RMSE_tree.diamond_test=sqrt(mean(sum((y_predicted_tree.diamond_test-test$log_price_test)^2)))
print(RMSE_tree.diamond_test)

#Training data performance of Model 
y_predicted_bag.diamond = predict(bag.diamond,data = train)
RMSE_bag.diamond_train=sqrt(mean(sum((y_predicted_bag.diamond-train$log_price_train)^2)))
print(RMSE_bag.diamond_train)
#Testing data performance of Model 3
y_predicted_bag.diamond_test = predict(bag.diamond,test)
RMSE_bag.diamond_test=sqrt(mean(sum((y_predicted_bag.diamond_test-test$log_price_test)^2)))
print(RMSE_bag.diamond_test)
