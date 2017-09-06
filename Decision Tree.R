#Package necessary for Random Forrest 
install.packages("rpart.plot")
library(rpart)
library("rpart.plot")


getwd()
setwd("C:/YYYYYY/AMMA 2017/Data/Amritendu")

#Decision Tree on Titanic dataset
View(titanic_final)
str(titanic_final) 
summary(titanic_final) 
 
###Splitting data 
set.seed(1234)
df.titanic <- runif(nrow(titanic_final))
titanic_train <- titanic_final[df.titanic <= 0.7,]
titanic_test<- titanic_final[df.titanic> 0.7,]

#Decision Tree on Titanic
fit = rpart(Survived ~ Pclass+Sex+Age,titanic_train,method="class")
rpart.plot_fit= plot(fit,uniform =TRUE, main = "Classification tree for Titanic ")
rpart.plot(fit)
text(fit,use.n=TRUE,all = TRUE, cex= .8)
