library(DataExplorer)
library(psych)  #describe() function
library(ggplot2)
library(RColorBrewer)
install.packages('gridBase')
library(gridBase)
library(grid)
library(MASS)
library(e1071)
install.packages('ROCR')
library(ROCR)
library(randomForest)
library(caret)
library(pROC)
library(InformationValue)
CUSTOM_COLORS_PLOT <- colorRampPalette(brewer.pal(10, "Set3"))

##################################################################################
# Loading and checking the data set;
##################################################################################
my.path <- 'C:\\Users\\Abhinav\\Desktop\\Fiverr\\Ali\\Advanced_Data_Analytics\\'; 
my.file <- paste(my.path,'uci_spambase.RData',sep='');
# Read the RData object using readRDS();
uci.spam <- readRDS(my.file)
head(uci.spam)

########################################################
#Data quality Check
#######################################################
str(uci.spam)
#There are 4601 observations and 61 variables out of 61 only 58 variables are relevant 
#57 Variables are numeric and Spam is also numeric but it should be factor variable
#Converting the spam from numeric to factor
uci.spam$spam = as.factor(uci.spam$spam)
summary(uci.spam)
describe(uci.spam)
plot_missing(uci.spam)
#we don't have any missing values in the dataset
sum(is.na(uci.spam))
#We don't have any NA value in the dataset
levels(uci.spam$spam)


###################################################
#EDA
###################################################
#Number of Hams in the dataset
result = table(uci.spam$spam)
num_ham = result[['0']]
num_spam = result[['1']]
print(num_ham)
print(paste0("Percentage of Hams ", round((num_ham/nrow(uci.spam)) * 100, 2), "%"))
print(paste0("Percentage of Hams ", round((num_spam/nrow(uci.spam)) * 100, 2), "%"))

ggplot(uci.spam, aes(x=spam)) + geom_bar(aes(group=spam, colour=spam, fill=spam), alpha=1)+geom_text(stat='count', aes(label=..count..), vjust=-1)
#Visual representation of the above fact

uci.spam_ham = sapply(uci.spam[which(uci.spam$spam=="0"),1:54],function(x) ifelse(is.numeric(x), round(mean(x), 2), NA))
uci.spam_spam = sapply(uci.spam[which(uci.spam$spam=="1"),1:54],function(x) ifelse(is.numeric(x), round(mean(x), 2), NA))

uci.spam_ham.order <- uci.spam_ham[order(-uci.spam_ham)[1:10]]
uci.spam_spam.order <- uci.spam_spam[order(-uci.spam_spam)[1:10]]


par(mfrow = c(1, 2))
par(mar = c(8, 4, 4, 2) + 0.1)  # increase y-axis margin.
plot <- barplot(uci.spam_ham.order, col = CUSTOM_COLORS_PLOT(10), main = "Ham: Average Percentage", 
                names.arg = "", ylab = "Percentage Relative (%)")
 text(x=plot,y=uci.spam_ham.order-0.1, labels=uci.spam_ham.order,cex=0.6)
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)
grid.text(names(uci.spam_ham.order), x = unit(plot, "native"), y = unit(-1, "lines"), just = "right", rot = 60)
popViewport(3)

plot <- barplot(uci.spam_spam.order, col = CUSTOM_COLORS_PLOT(10), main = "Spam: Average Percentage", 
                names.arg = "", ylab = "Percentage Relative (%)")
text(x=plot,y=uci.spam_spam.order-0.1, labels=uci.spam_spam.order,cex=0.6)
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)
grid.text(names(uci.spam_spam.order), x = unit(plot, "native"), y = unit(-1, "lines"), just = "right", rot = 60)
popViewport(3)
#There is Distortion of the text on the x-axis due to the package parameter used, now we see for the ham the relative percentage of words you and george is high
#For Spam email the relative percentage of the words you and your is high.
#There are also difference in the words which are frequently occuring in the spam and ham

library(corrgram)
numerical_subset = uci.spam[,-58]
corrgram(numerical_subset, order = TRUE, lower.panel = panel.shade,upper.panel = panel.pie, text.panel = panel.txt)
#There is some amount of correlation amoung the words. Indicating that two words occur together or in similar frequency


############################Spliting the data in train and test data set#################### 
train = uci.spam[uci.spam$train==1,]
test = uci.spam[uci.spam$test==1,]
#Table providing the number of data present in traint and test
nrow(train)
nrow(test)
train = train[,-c(59:61)]
test = test[,-c(59:61)]
#################################################################################################


############################################################
#Modelling 
############################################################

###################################
#Logistic Regression
##################################
set.seed(100)
model.lr = glm(spam ~ .,family = binomial(link="logit"),data = train)
summary(model.lr)
predictandevaluate = function(model,data){
  pred.lr = predict(model, newdata = data,type = "response")
  optCutOff = optimalCutoff(data$spam, pred.lr)[1]
  model.cf = confusionMatrix(data$spam, pred.lr, threshold = optCutOff)
  return(model.cf)
}
###########Performance###############
#Confusion matrix for the train data
predictandevaluate(model.lr,train)
print(paste0("Accuary (Precision) on Train dataset: ", (1990+1223)/(1990+1223+142+84)))
#Confusion matrix for the test data
predictandevaluate(model.lr,test)
print(paste0("Accuary (Precision) on Test dataset: ", (672+413)/(672+413+35+42)))

############################################
#Tree Model
############################################
set.seed(100)
library(rpart)
model.dt <- rpart(spam ~ ., method = "class", data = train)
summary(model.dt)
plot(model.dt, uniform = TRUE, main = "Classification (RPART). Classification Tree for SPAM")
text(model.dt, all = TRUE, cex = 0.75)
predictandevaluate = function(model,data){
  pred.dt <- predict(model, newdata = data, type = "class")
  cf = table(`Actual Class` = data$spam, `Predicted Class` = pred.dt)
  error.rate.rpart <- sum(data$spam != pred.dt)/nrow(data)
  print("Confusion Matrix :")
  print(cf)
  return(print(paste0("Accuary (Precision) dataset:  ", 1 - error.rate.rpart)))
}
#################Performance####################
#performance of training dataset
predictandevaluate(model.dt,train)
#Performance of testing dataset
predictandevaluate(model.dt,test)

##############################
#SVM
##############################
set.seed(100)
model.svm = svm(spam ~ ., method = "class", data = train)
summary(model.svm)
#plotting Svm
#No idea how to do it

#################Performance####################
#performance of training dataset
predictandevaluate(model.svm,train)
#Performance of testing dataset
predictandevaluate(model.svm,test)


##################################
#Random Forest 
#################################
set.seed(100)
train1 =train
test1 = test
#here we have changed the colnames due to naming issue which random forest is not able to rectify 
newColNames <- c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d", 
                 "word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet", 
                 "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will", 
                 "word_freq_people", "word_freq_report", "word_freq_addresses", "word_freq_free", 
                 "word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit", 
                 "word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money", 
                 "word_freq_hp", "word_freq_hpl", "word_freq_george", "word_freq_650", "word_freq_lab", 
                 "word_freq_labs", "word_freq_telnet", "word_freq_857", "word_freq_data", 
                 "word_freq_415", "word_freq_85", "word_freq_technology", "word_freq_1999", 
                 "word_freq_parts", "word_freq_pm", "word_freq_direct", "word_freq_cs", "word_freq_meeting", 
                 "word_freq_original", "word_freq_project", "word_freq_re", "word_freq_edu", 
                 "word_freq_table", "word_freq_conference", "char_freq_semicolon", "char_freq_leftopenbracket", 
                 "char_freq_leftsquarebracket", "char_freq_notequal", "char_freq_dollar", "char_freq_hash", "capital_run_length_average", 
                 "capital_run_length_longest", "capital_run_length_total", "spam")

#here "char_freq_;= char_freq_semicolon"                "char_freq_( = char_freq_leftopenbracket"               
#"char_freq_[= char_freq_leftsquarebracket"                "char_freq_!=char_freq_notequal" "char_freq_$=char_freq_dollar" "char_freq_# ="char_freq_hash" 

colnames(train1) <- newColNames
colnames(test1) <- newColNames
model.rf = randomForest(spam ~ .,data = train1)
model.rf
library(caret)
varImp(model.rf)
varImpPlot(model.rf,type=2)
#################Performance####################
#performance of training dataset
predictandevaluate(model.rf,train1)
#Performance of testing dataset
predictandevaluate(model.rf,test1)






######################################
#Naive Bayes with WOE Binning
######################################
install.packages('woeBinning')
library(woeBinning)
df = train[,c(1:5,58)]
# Bin all variables of the data frame (apart from the target variable)
# with default parameter settings
# Plot the binned variables
woe.binning.plot(binning)
binning = woe.binning(df,'spam',df)
df.with.binned.vars.added <- woe.binning.deploy(df, binning,add.woe.or.dum.var='woe')

# Tabulate the binned variables
tabulate.binning <- woe.binning.table(binning)
tabulate.binning

# Deploy the binning solution to the data frame
# (i.e. add binned variables and corresponding WOE variables)
df.with.binned.vars.added <- woe.binning.deploy(df, binning,add.woe.or.dum.var='woe')		
head(df.with.binned.vars.added)
#https://cran.r-project.org/web/packages/woeBinning/woeBinning.pdf for the reference

