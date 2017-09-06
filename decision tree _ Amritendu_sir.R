setwd("C:/Users/Trisha Dutta/Downloads")
titanic_data= read.csv("train.csv")

###understanding data
View(titanic_data)
str(titanic_data)
head(titanic_data)

###selecting necessary variables
titanic_final= titanic_data[c("Pclass" ,"Sex" ,"Age" ,"SibSp", "Parch","Survived")]
View(titanic_final)
str(titanic_final)
summary(titanic_final)

###Missing value imputation
titanic_final$Age[is.na(titanic_final$Age)]= mean(titanic_final$Age[!is.na(titanic_final$Age)])
summary(titanic_final)

###Splitting data 
set.seed(1234)
df.titanic <- runif(nrow(titanic_final))
titanic_train <- titanic_final[df.titanic <= 0.7,]
titanic_test<- titanic_final[df.titanic> 0.7,]


###call rpart package
library("rpart")

fit = rpart(Survived ~ Pclass+Sex+Age ,titanic_train, method = "class") 

plot_fit= plot(fit,uniform = TRUE , main = "Classification Tree For Titanic")

text(fit,use.n = TRUE, all = TRUE , cex=.8)


#########____________###############
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(fit)

###############___________########
install.packages("randomForest")
library(randomForest)


rf= randomForest(Survived ~ Pclass+Sex+Age ,data =  titanic_train, mtry = 2,importance= TRUE)
print(rf)
summary(rf)
varImpPlot(rf)

titanic_train$randomprob = predict(rf,type=c("response"))
titanic_train$randompred = ifelse(titanic_train$randomprob>=.5,'pred_yes','pred_no')
View(titanic_train)

### confusion matrix of training data 
table(titanic_train$randompred,titanic_train$Survived)


### index for measuring trainning data
accuracy_random = (345+157)/(345+157+94+41)
print(accuracy_random)

########_____________########
install.packages("e1071")
library(e1071)


svm= svm(Survived ~ Pclass+Sex+Age ,data =  titanic_train, mtry = 2,importance= TRUE)
print(svm)


titanic_train$svm_prob = predict(svm,type=c("response"))
titanic_train$svm_pred = ifelse(titanic_train$svm_prob>=.5,'pred_yes','pred_no')
View(titanic_train)

### confusion matrix of training data 
table(titanic_train$svm_pred,titanic_train$Survived)


### index for measuring trainning data
accuracy_svm = (358+144)/(358+144+28+107)
print(accuracy_svm)
