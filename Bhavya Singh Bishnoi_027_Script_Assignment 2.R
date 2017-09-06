# STEP - 1 START #

install.packages("rpart.plot")
install.packages("randomForest")
install.packages("DAAG")
library(rpart.plot)
library(gmodels)
library(Hmisc)
library(pROC)
library(ResourceSelection)
library(car)
library(caret)
library(dplyr)
library(InformationValue)
library(rpart)
library(randomForest)
library("DAAG")

cat("\014") # Clearing the screen

getwd()
setwd("C:/MICA/Term 4/AMMA 2017/Assignment/Assignment 2") #This working directory is the folder where all the bank data is stored

titanic_train_1<-read.csv('train.csv')
titanic_train<-titanic_train_1
titanic_train_2 <- read.csv('train.csv')

#titanic test
titanic_test_const <-read.csv('test-3.csv')

#splitting titanic train into 80,20
set.seed(1234) # for reproducibility
titanic_train$rand <- runif(nrow(titanic_train))
titanic_train_start <- titanic_train[titanic_train$rand <= 0.8,]
titanic_test_start <- titanic_train[titanic_train$rand > 0.8,]


# number of survived vs number of dead
CrossTable(titanic_train$Survived)

# removing NA row entries
#titanic_train <- titanic_train_start
titanic_train <- titanic_train[!apply(titanic_train[,c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Age")], 1, anyNA),]
titanic_train_NA_columns <- titanic_train_1[!apply(titanic_train_1[,c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Age")], 1, anyNA),]
nrow(titanic_train_1)
nrow(titanic_train_NA_columns)

# replacing NA by mean
mean_age = mean(titanic_train_1$Age)
titanic_train_mean1 <- titanic_train_start
titanic_test_mean <- titanic_test_start
titanic_train_mean1$Age[is.na(titanic_train_mean1$Age)] = mean(titanic_train_mean1$Age, na.rm = TRUE)
titanic_test_mean$Age[is.na(titanic_test_mean$Age)] = mean(titanic_test_mean$Age, na.rm = TRUE)

# STEP - 1 END #

# STEP - 2 START #

########## Build model from mean imputed into the data set ##########

fullmod.titanic_mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age,
                            data=titanic_train_mean1, family = binomial) #family = binomial implies that the type of regression is logistic

#lm
fittrain_mean <- lm(formula = Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age,
                    data=titanic_test_mean) #family = binomial implies that the type of regression is logistic
summary(fittrain_mean)

#vif - remove those variables which have high vif >5
vif(fittrain_mean) 

#removing insignificant variables
titanic_train_mean1$Fare<-NULL
fullmod.titanic_mean <- glm(formula = Survived ~ Pclass + Sex + SibSp +Parch + Age,
                            data=titanic_train_mean1, family = binomial) #family = binomial implies that the type of regression is logistic
summary(fullmod.titanic_mean)

titanic_train_mean1$Parch<-NULL
fullmod.titanic_mean <- glm(formula = Survived ~ Pclass + Sex + SibSp + + Age,
                            data=titanic_train_mean1, family = binomial) #family = binomial implies that the type of regression is logistic
summary(fullmod.titanic_mean)

#Testing performance on Train set

titanic_train_mean1$prob = predict(fullmod.titanic_mean, type=c("response"))
titanic_train_mean1$Survived.pred = ifelse(titanic_train_mean1$prob>=.5,'pred_yes','pred_no')
table(titanic_train_mean1$Survived.pred,titanic_train_mean1$Survived)

#Testing performance on test set
nrow(titanic_test_start)
titanic_test2_mean1 <- titanic_test_start
nrow(titanic_test2_mean1)

#imputation by replacing NAs by means in the test set
titanic_test2_mean1$Age[is.na(titanic_test2_mean1$Age)] = mean(titanic_test2_mean1$Age, na.rm = TRUE)

titanic_test2_mean1$prob = predict(fullmod.titanic_mean, newdata=titanic_test2_mean1, type=c("response"))
titanic_test2_mean1$Survived.pred = ifelse(titanic_test2_mean1$prob>=.5,'pred_yes','pred_no')
table(titanic_test2_mean1$Survived.pred,titanic_test2_mean1$Survived)

########## END - Model with mean included instead of NA #########

# STEP - 2 END #

# STEP - 3 START #

### Testing for Jack n Rose's survival ###
df.jackrose <- read.csv('Book1.csv')
df.jackrose$prob = predict(fullmod.titanic_mean, newdata=df.jackrose, type=c("response"))
df.jackrose$Survived.pred = ifelse(df.jackrose$prob>=.5,'pred_yes','pred_no')
head(df.jackrose)

# Jack dies, Rose survives

### END - Testing on Jack n Rose ###

# STEP - 3 END #

# STEP - 4 START #

## START  K-fold cross validation ##

# Defining the K Fold CV function here
Kfold_function <- function(dataset,formula,family,k)
{
  object <- glm(formula=formula, data=dataset, family = family)
  CVbinary(object, nfolds= k, print.details=TRUE)
}

#Defining the function to calculate Mean Squared Error here
MeanSquareError_function <- function(dataset,formula)
{
  LM_Object <- lm(formula=formula, data=dataset)
  LM_Object_sum <-summary(LM_Object)
  MSE <- mean(LM_Object_sum$residuals^2)
  print("Mean squared error")
  print(MSE)
}

#Performing KFold CV on Training set by calling the KFOLD CV function here
Kfoldobject <- Kfold_function(titanic_train_mean1,Survived ~ Pclass + Sex + SibSp + Age,binomial,10)

#Calling the Mean Squared Error function on the training set here
MSqErr_Train <-MeanSquareError_function(titanic_train_mean1,Survived ~ Pclass + Sex + SibSp + Age)

#confusion matrix on training set
table(titanic_train_mean1$Survived,round(Kfoldobject$cvhat))
print("Estimate of Accuracy")
print(Kfoldobject$acc.cv)

#Performing KFold CV on test set by calling the KFOLD CV function here
Kfoldobject.test <- Kfold_function(titanic_test2_mean1,Survived ~ Pclass + Sex + SibSp + Age,binomial,10)

#Calling the Mean Squared Error function on the test set here
MSqErr_Test <-MeanSquareError_function(titanic_test2_mean1,Survived ~ Pclass + Sex + SibSp + Age)

#Confusion matrix on test set
table(titanic_test2_mean1$Survived,round(Kfoldobject.test$cvhat))
print("Estimate of Accuracy")
print(Kfoldobject.test$acc.cv)

## END K-FOLD CROSS VALIDATION ##

# STEP - 4 END #
