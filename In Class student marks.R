Name <- c(head(LETTERS,5)) 
Age <- c(23,22,21,25,20)
Math_marks <- c(87,86,89,88,79)
Science_marks <- c(65,87,78,55,93)
student.df <- data.frame(Name,Age,Math_marks,Science_marks)

student.df$Total_marks<-student.df$Math_marks+student.df$Science_marks
student.df$Percent_Math <-(student.df$Math_marks/student.df$Total_marks)*100

student.df$Percent_Science <-(student.df$Science_marks/student.df$Total_marks)*100
View(student.df)
#######  Removing a column  ########
student.new <- student.df[c(2:7)] 
student.new<- student.df[,-1]
View(student.new)
## adding some transformed columns to student.df
student.df$log_age<-log(student.df$Age)
student.df$exp_age<-exp(student.df$Age/mean(student.df$Age))
student.df$inv_age<-1/(student.df$Age)
student.df$sqr_age<-(student.df$Age)*(student.df$Age)
student.df$sqrt_age<-sqrt(student.df$Age)
View(student.df)
##changing from numeric to character
student.df$log_age<-as.character(student.df$log_age)
class(student.df$log_age)

View(student.df)

s1<-student.df[student.df$Age>=23,]
View(s1)

