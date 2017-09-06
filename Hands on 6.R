library("xml2")
library("rvest")
link = "http://stats.espncricinfo.com/ci/content/records/284248.html"
file=read_html(link)
cricket=html_nodes(file,"table")
cricket_table<-html_table(cricket[1],fill=TRUE)
View(cricket_table)
cricket.df <- as.data.frame(cricket_table)

View(cricket.df)

TopFive<-c(cricket.df$Player[1], cricket.df$Player[2], cricket.df$Player[3], cricket.df$Player[4], cricket.df$Player[5])
TopFive

Top.Five.df <- NULL
Top.Five.df$Runs<- cricket.df$Runs[1:5]
Top.Five.df$Year<- cricket.df$Year[1:5]

plot(Top.Five.df$Year, Top.Five.df$Runs)

###### Top Five Scorers End ######

######## Highest Run Getter #####

yearsort<- cricket.df[with(cricket.df, order(Year, Player)), ]
View(yearsort)

yearscore.df<- as.data.frame(table(cricket.df$Year))
yearscore.df$MaxRuns <- rep(0,nrow(yearscore.df))
yearscore.df$MaxRunsPlayer <- rep("",nrow(yearscore.df))
for(i3 in 1:nrow(yearscore.df))
{
  
  maxRuns<- subset(cricket.df,cricket.df$Year==yearscore.df$Var1[i3])
  yearscore.df$MaxRuns[i3] <- max(maxRuns$Runs)
}

for (i4 in 1:nrow(yearscore.df))
{
  for (i5 in 1:nrow(cricket.df))
  {
    if(cricket.df$Year[i5]==yearscore.df$Var1[i4] && cricket.df$Runs[i5]==yearscore.df$MaxRuns[i4])
      yearscore.df$MaxRunsPlayer[i4] <- cricket.df$Player[i5]
  }  
  
}

playerMaxRunsTable <- as.data.frame(table(yearscore.df$MaxRunsPlayer))
max(playerMaxRunsTable$Freq)

print(playerMaxRunsTable)
print("Maximum number of times a player has got the highest number of runs in a year")
print(max(playerMaxRunsTable$Freq))

##### Highest Run Getter ####
###### Contribution ofplayer country #####
PAK=0
AUS=0
IND=0
SL=0
ENG=0
WI=0
SA=0

for(i in 1:length(cricket.df$Player))
{
  if(grepl("PAK",cricket.df$Player[i]))
    PAK=PAK+1
}

PAK

for(i in 1:length(cricket.df$Player))
{
  if(grepl("AUS",cricket.df$Player[i]))
    AUS=AUS+1
}

AUS
for(i in 1:length(cricket.df$Player))
{
  if(grepl("INDIA",cricket.df$Player[i]))
    IND=IND+1
}

IND
for(i in 1:length(cricket.df$Player))
{
  if(grepl("SL",cricket.df$Player[i]))
    SL=SL+1
}

SL
for(i in 1:length(cricket.df$Player))
{
  if(grepl("ENG",cricket.df$Player[i]))
    ENG=ENG+1
}

ENG
for(i in 1:length(cricket.df$Player))
{
  if(grepl("WI",cricket.df$Player[i]))
    WI=WI+1
}

WI
for(i in 1:length(cricket.df$Player))
{
  if(grepl("SA",cricket.df$Player[i]))
    SA=SA+1
}

SA

Total_appearances = PAK + AUS + IND + WI + ENG + SA + SL

PAK_ratio = round(100*PAK/Total_appearances,2)
AUS_ratio = round(100*AUS/Total_appearances,2)
IND_ratio = round(100*IND/Total_appearances,2)
SL_ratio = round(100*SL/Total_appearances,2)
WI_ratio = round(100*WI/Total_appearances,2)
ENG_ratio = round(100*ENG/Total_appearances,2)
SA_ratio = round(100*SA/Total_appearances,2)

Country_Percent<- NULL
Country_Percent$Country <- as.factor(c("PAK","AUS","IND","SL","WI","ENG","SA"))
Country_Percent$Percentage <- c(PAK_ratio,AUS_ratio,IND_ratio,SL_ratio,WI_ratio,ENG_ratio,SA_ratio)
View(Country_Percent)
plot(Country_Percent$Country,Country_Percent$Percentage)
