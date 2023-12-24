library(readr)
all_ball_data <- read_csv("IPL data analysis/IPL_Ball_by_Ball_2008_2022.csv", 
                          col_types = cols(ID = col_number(), innings = col_number(), 
                                           overs = col_number(), ballnumber = col_number(), 
                                           batsman_run = col_number(), extras_run = col_number(), 
                                           total_run = col_number(), isWicketDelivery = col_number()))
all_match_data <- read_csv("IPL data analysis/IPL_Matches_2008_2022.csv", 
                           col_types = cols(ID = col_integer(), 
                                            Season = col_integer(), Margin = col_number()))
for_bollimg_merge<-all_match_data[,c("ID","Team1","Team2")]
all_ball_data<-merge(all_ball_data,for_bollimg_merge,by.x = "ID",
                     by.y = "ID",all.x = TRUE)
bowler_team<-NULL
for(i in 1:nrow(all_ball_data)){
  if(all_ball_data$BattingTeam[i]==all_ball_data$Team1[i]){
    bowler_team[i]<-all_ball_data$Team2[i]
  }
  else{bowler_team[i]<-all_ball_data$Team1[i]}
}
all_ball_data<-all_ball_data[,1:17]
all_ball_data<-cbind(all_ball_data,bowler_team)
for_merge<-all_match_data[,c("ID","Date","City","Venue","Player_of_Match")]
all_ball_data<-merge(all_ball_data,for_merge,by.x = "ID",
                     by.y = "ID",all.x = TRUE)
season <- as.POSIXct(all_ball_data$Date, format = "%m/%d/%Y")
all_ball_data<-cbind(all_ball_data,"Season"=format(season, format="%Y"))
all_ball_data$Season<-as.numeric(all_ball_data$Season)
over_interval<-NULL
for (i in 1:nrow(all_ball_data)){
  if(all_ball_data$over[i] < 6){over_interval[i]="01-06"}
  else if(all_ball_data$over[i]>5 & all_ball_data$over[i]<15){
    over_interval[i]="07-15"
  }
  else{over_interval[i]="16-20"}
}
x<-ave(x=all_ball_data$isWicketDelivery,all_ball_data$ID,all_ball_data$innings,FUN=cumsum)
all_ball_data<-cbind(all_ball_data,over_interval,"Position"=x)
all_ball_data<-all_ball_data[all_ball_data$innings %in% c(1,2),]
all_ball_data[all_ball_data$Venue=="Arun Jaitley Stadium, Delhi","Venue"]<-"Arun Jaitley Stadium"
all_ball_data[all_ball_data$Venue=="Dr DY Patil Sports Academy, Mumbai","Venue"]<-"Dr DY Patil Sports Academy"
all_ball_data[all_ball_data$Venue=="Eden Gardens, Kolkata","Venue"]<-"Eden Gardens"
all_ball_data[all_ball_data$Venue=="M Chinnaswamy Stadium","Venue"]<-"M.Chinnaswamy Stadium"
all_ball_data[all_ball_data$Venue=="MA Chidambaram Stadium, Chepauk","Venue"]<-"MA Chidambaram Stadium"
all_ball_data[all_ball_data$Venue=="MA Chidambaram Stadium, Chepauk, Chennai","Venue"]<-"MA Chidambaram Stadium"
all_ball_data[all_ball_data$Venue=="Punjab Cricket Association IS Bindra Stadium, Mohali","Venue"]<-"Punjab Cricket Association IS Bindra Stadium"
all_ball_data[all_ball_data$Venue=="Punjab Cricket Association Stadium, Mohali","Venue"]<-"Punjab Cricket Association IS Bindra Stadium"
all_ball_data[all_ball_data$Venue=="Rajiv Gandhi International Stadium, Uppal","Venue"]<-"Rajiv Gandhi International Stadium"
all_ball_data[all_ball_data$Venue=="Wankhede Stadium, Mumbai","Venue"]<-"Wankhede Stadium"
all_ball_data[all_ball_data$Venue=="Brabourne Stadium, Mumbai","Venue"]<-"Brabourne Stadium"
all_ball_data[all_ball_data$Venue=="Maharashtra Cricket Association Stadium, Pune","Venue"]<-"Maharashtra Cricket Association Stadium"
all_ball_data[all_ball_data$Venue=="Feroz Shah Kotla","Venue"]<-"Arun Jaitley Stadium"
all_ball_data[all_ball_data$Venue=="Maharashtra Cricket Association Stadium","Venue"]<-"Subrata Roy Sahara Stadium"
all_ball_data[all_ball_data$Venue=="Sardar Patel Stadium, Motera","Venue"]<-"Narendra Modi Stadium, Ahmedabad"
all_ball_data[all_ball_data$City %in% "Navi Mumbai","City"]<-"Mumbai"
all_ball_data[all_ball_data$City %in% "Bengaluru","City"]<-"Bangalore"

all_ball_data[all_ball_data$BattingTeam=="Delhi Daredevils","BattingTeam"]<-"Delhi Capitals"
all_ball_data[all_ball_data$BattingTeam=="Kings XI Punjab","BattingTeam"]<-"Punjab Kings"
all_ball_data[all_ball_data$BattingTeam=="Rising Pune Supergiants","BattingTeam"]<-"Rising Pune Supergiant"
all_ball_data[all_ball_data$bowler_team=="Delhi Daredevils","bowler_team"]<-"Delhi Capitals"
all_ball_data[all_ball_data$bowler_team=="Kings XI Punjab","bowler_team"]<-"Punjab Kings"
all_ball_data[all_ball_data$bowler_team=="Rising Pune Supergiants","bowler_team"]<-"Rising Pune Supergiant"
all_ball_data[all_ball_data$kind %in% "retired hurt","kind"]<-"retired out"

rm(for_bollimg_merge,for_merge,bowler_team,i,over_interval,season,x)
