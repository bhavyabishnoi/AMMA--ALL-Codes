####### Part 1 ###########
getwd()
setwd("C:/MICA/Term 4/AMMA 2017/data_2017")
mortality_table<-read.csv("Infant Mortality.csv",header = T)
View(mortality_table)
#### Removing the Last Variable ########
#Since the last variable is the 8th coumn therefore#

mortality_table<-mortality_table[,-8]
View(mortality_table)


##### Part 1 End  ########

#########  Part 2 ##########
install.packages("rvest")
install.packages("xml2")
install.packages("XML")
library("xml2")
library("rvest")
library("XML")

link = "https://en.wikipedia.org/wiki/India%E2%80%93Pakistan_cricket_rivalry"
file=read_html(link)
table=html_nodes(file,"table")
cricket_table<-html_table(table[2],fill=TRUE)
View(cricket_table)
cricket_table.final<-as.data.frame(cricket_table)
India_Odi<-cricket_table.final[3,3]
print("Number of ODIs Won by India")
print(India_Odi)
############## END Part 2 ###############