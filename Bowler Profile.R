Bowler_Profile<-function(Bowler_Name,Start_Season=2008,End_Season=2022){
  if(Bowler_Name %in% all_ball_data$bowler ==FALSE){
    if(length(unique(all_ball_data$bowler[grepl(Bowler_Name,all_ball_data$bowler)]))==0){
      cat("Please correctly mention the player name\nhint:- Write surname of the player with first capital letter\n")
      stop()
    }
    print("Possible player name that you are looking --->")
    print(data.frame("No."=1:length(unique(all_ball_data$bowler[grepl(Bowler_Name,all_ball_data$bowler)])),
                     "Player"=unique(all_ball_data$bowler[grepl(Bowler_Name,all_ball_data$bowler)])))
    print("Select the number corresponding to your player")
    a<-readline()
    a<-as.numeric(a)
    Bowler_Name<-unique(all_ball_data$bowler[grepl(Bowler_Name,all_ball_data$bowler)])[a]
  }
  subdata1<-subset(all_ball_data,bowler==Bowler_Name & Season>=Start_Season & Season<=End_Season)
  if(nrow(subdata1)==0){cat("Your Player Doesn't play in the given season\n");stop()}
  subdata<-subset(subdata1,extra_type %in% c("legbyes",NA,"wides","byes","noballs"))
  subdata<-subset(subdata,subdata$innings %in% c(1,2))
  Inn<-length(unique(subdata$ID))
  BD<-nrow(subdata[subdata$extra_type %in% c( NA,"legbyes","byes"),])
  Runs_concede<-sum(subdata$total_run[subdata$extra_type %in% c(NA,"wides","noballs")])
  Wkt<-length(subdata$isWicketDelivery[! subdata$kind %in% c("run out",NA,"retired out","obstructing the field")])
  match_run<-aggregate(total_run~ID,subdata[subdata$extra_type %in% c(NA,"wides","noballs"),],sum)
  match_wkt<-aggregate(isWicketDelivery~ID,subdata[! subdata$kind %in% c("run out","retired out","obstructing the field"),],sum)
  best_data<-merge(match_run,match_wkt,all.x = TRUE,by = "ID")
  best_data[is.na(best_data$isWicketDelivery),"isWicketDelivery"]<-0
  best_run<-min(best_data$total_run[best_data$isWicketDelivery==max(best_data$isWicketDelivery)])
  BBM<-noquote(capture.output(cat(max(best_data$isWicketDelivery),"/",best_run,sep = "")))
  Economy<-round(Runs_concede/(BD/6),2)
  if(Wkt==0){Avg<-Runs_concede;SR<-BD}
  else{
  Avg<-round(Runs_concede/Wkt,2)
  SR<-round(BD/Wkt,2)}
  five_wkt<-nrow(best_data[best_data$isWicketDelivery>4,])
  four_wkt<-nrow(best_data[best_data$isWicketDelivery>3,])-five_wkt
  cat("_______________________",Bowler_Name,"(",min(subdata$Season),",",max(subdata$Season),")","_________________________________
Performance :-
      
      Innings      -   ",Inn,"
      Bowl Bowled  -   ",BD,"
      Runs Concede -   ",Runs_concede,"
      Wickets      -   ",Wkt,"
      Best Bowling -   ",BBM,"
      Economy      -   ",Economy,"
      Strike Rate  -   ",SR,"
      Average      -   ",Avg,"
      4W           -   ",four_wkt,"
      5W           -   ",five_wkt,"\n",sep="")
  
  #Over Interval Data
  over_interval_wise<-data.frame()
  for(i in 1:length(unique(subdata$over_interval))){
    if(is.na(unique(subdata$over_interval)[i])){next}
    ubdata<-subset(subdata,subdata$over_interval==unique(subdata$over_interval)[i])
    BD<-nrow(ubdata[ubdata$extra_type %in% c( NA,"legbyes","byes"),])
    Runs_concede<-sum(ubdata$total_run[ubdata$extra_type %in% c(NA,"wides","noballs")])
    Wkt<-length(ubdata$isWicketDelivery[! ubdata$kind %in% c("run out",NA,"retired out","obstructing the field")])
    match_run<-aggregate(total_run~ID,ubdata[ubdata$extra_type %in% c(NA,"wides","noballs"),],sum)
    match_wkt<-aggregate(isWicketDelivery~ID,ubdata[! ubdata$kind %in% c("run out","retired out","obstructing the field"),],sum)
    best_data<-merge(match_run,match_wkt,all.x = TRUE,by = "ID")
    best_data[is.na(best_data$isWicketDelivery),"isWicketDelivery"]<-0
    best_run<-min(best_data$total_run[best_data$isWicketDelivery==max(best_data$isWicketDelivery)])
    BBM<-noquote(capture.output(cat(max(best_data$isWicketDelivery),"/",best_run,sep = "")))
    Economy<-round(Runs_concede/(BD/6),2)
    if(Wkt==0){Avg<-Runs_concede;SR<-BD}
    else{
      Avg<-round(Runs_concede/Wkt,2)
      SR<-round(BD/Wkt,2)}
    over_interval_wise[i,c("over_interval","Bowl Bowled","Runs Concede","Wickets","Best Bowling","Economy","Average","Strike Rate")]<-
      c(unique(ubdata$over_interval),BD,Runs_concede,Wkt,BBM,Economy,Avg,SR)
    over_interval_wise<-over_interval_wise[is.na(over_interval_wise$over_interval)!=TRUE,]
  }
  over_interval_wise[,c(2:4,6:8)]<-apply(over_interval_wise[ ,c(2:4,6:8)], 2,
                                  function(x) as.numeric(x))
  over_interval_wise<-over_interval_wise[order(match(over_interval_wise$over_interval,c("01-06","07-15","16-20"))),]
  cat("__________________________________________________________________________________")
  cat("\nOver Interval Performance\n\n")
  print(over_interval_wise)
  
  #Inning Wise Data
  inning_data<-data.frame()
  for(i in 1:length(unique(subdata$innings))){
    if(is.na(unique(subdata$innings)[i])){next}
    ubdata<-subset(subdata,subdata$innings==unique(subdata$innings)[i])
    Inn<-length(unique(ubdata$ID))
    BD<-nrow(ubdata[ubdata$extra_type %in% c( NA,"legbyes","byes"),])
    Runs_concede<-sum(ubdata$total_run[ubdata$extra_type %in% c(NA,"wides","noballs")])
    Wkt<-length(ubdata$isWicketDelivery[! ubdata$kind %in% c("run out",NA,"retired out","obstructing the field")])
    match_run<-aggregate(total_run~ID,ubdata[ubdata$extra_type %in% c(NA,"wides","noballs"),],sum)
    match_wkt<-aggregate(isWicketDelivery~ID,ubdata[! ubdata$kind %in% c("run out","retired out","obstructing the field"),],sum)
    best_data<-merge(match_run,match_wkt,all.x = TRUE,by = "ID")
    best_data[is.na(best_data$isWicketDelivery),"isWicketDelivery"]<-0
    best_run<-min(best_data$total_run[best_data$isWicketDelivery==max(best_data$isWicketDelivery)])
    BBM<-noquote(capture.output(cat(max(best_data$isWicketDelivery),"/",best_run,sep = "")))
    Economy<-round(Runs_concede/(BD/6),2)
    if(Wkt==0){Avg<-Runs_concede;SR<-BD}
    else{
      Avg<-round(Runs_concede/Wkt,2)
      SR<-round(BD/Wkt,2)}
    five_wkt<-nrow(best_data[best_data$isWicketDelivery>4,])
    four_wkt<-nrow(best_data[best_data$isWicketDelivery>3,])-five_wkt
    inning_data[i,c("Inning","Inn","Bowl Bowled","Runs Concede","Wickets",
                    "Best Bowling","Economy","Average","Strike Rate","Four Wicket",
                    "Five Wicket")]<-
      c(unique(ubdata$innings),Inn,BD,Runs_concede,Wkt,BBM,Economy,Avg,SR,
        four_wkt,five_wkt)
    inning_data<-inning_data[is.na(inning_data$Inning)!=TRUE,]
  }
  inning_data[,c(2:5,7:11)]<-apply(inning_data[ ,c(2:5,7:11)], 2,
                            function(x) as.numeric(x))
  cat("__________________________________________________________________________________")
  cat("\nInning wise Performance\n\n")
  print(inning_data)
  
  # SEASON Wise DATA
  season_data<-data.frame()
  for(i in 1:length(unique(subdata$Season))){
    ubdata<-subdata[subdata$Season==unique(subdata$Season)[i],]
    Inn<-length(unique(ubdata$ID))
    BD<-nrow(ubdata[ubdata$extra_type %in% c( NA,"legbyes","byes"),])
    Runs_concede<-sum(ubdata$total_run[ubdata$extra_type %in% c(NA,"wides","noballs")])
    Wkt<-length(ubdata$isWicketDelivery[! ubdata$kind %in% c("run out",NA,"retired out","obstructing the field")])
    match_run<-aggregate(total_run~ID,ubdata[ubdata$extra_type %in% c(NA,"wides","noballs"),],sum)
    match_wkt<-aggregate(isWicketDelivery~ID,ubdata[! ubdata$kind %in% c("run out","retired out","obstructing the field"),],sum)
    best_data<-merge(match_run,match_wkt,all.x = TRUE,by = "ID")
    best_data[is.na(best_data$isWicketDelivery),"isWicketDelivery"]<-0
    best_run<-min(best_data$total_run[best_data$isWicketDelivery==max(best_data$isWicketDelivery)])
    BBM<-noquote(capture.output(cat(max(best_data$isWicketDelivery),"/",best_run,sep = "")))
    Economy<-round(Runs_concede/(BD/6),2)
    if(Wkt==0){Avg<-Runs_concede;SR<-BD}
    else{
      Avg<-round(Runs_concede/Wkt,2)
      SR<-round(BD/Wkt,2)}
    five_wkt<-nrow(best_data[best_data$isWicketDelivery>4,])
    four_wkt<-nrow(best_data[best_data$isWicketDelivery>3,])-five_wkt
    season_data[i,c("Season","Inn","Bowl Bowled","Runs Concede","Wickets",
                    "Best Bowling","Economy","Average","Strike Rate","Four Wicket",
                    "Five Wicket")]<-c(unique(ubdata$Season),Inn,BD,Runs_concede,Wkt,BBM,Economy,Avg,SR,
                                       four_wkt,five_wkt)
  }
  season_data[,c(2:5,7:11)]<-apply(season_data[ ,c(2:5,7:11)], 2,
                            function(x) as.numeric(x))
  plot(x=season_data$Average,y=season_data$`Strike Rate`,xlab = "Average",type="n",
       ylab = "Strike Rate",lwd=2,panel.first=grid(),
       xlim<-c(min(season_data$Average)-5,max(season_data$Average)+5),
       ylim<-c(min(season_data$`Strike Rate`)-10,max(season_data$`Strike Rate`)+10),
       main=paste("Average v/s Strike Rate"))
  text(season_data$Average,season_data$`Strike Rate`+1,labels=season_data$Season)

  cat("__________________________________________________________________________________")
  cat("\nYear on Year Performance\n\n")
  print(season_data)
  
  # Team Wise DATA
  team_data<-data.frame()
  for(i in 1:length(unique(subdata$BattingTeam))){
    if(is.na(unique(subdata$BattingTeam)[i])){next}
    ubdata<-subset(subdata,subdata$BattingTeam==unique(subdata$BattingTeam)[i])
    Inn<-length(unique(ubdata$ID))
    BD<-nrow(ubdata[ubdata$extra_type %in% c( NA,"legbyes","byes"),])
    Runs_concede<-sum(ubdata$total_run[ubdata$extra_type %in% c(NA,"wides","noballs")])
    Wkt<-length(ubdata$isWicketDelivery[! ubdata$kind %in% c("run out",NA,"retired out","obstructing the field")])
    match_run<-aggregate(total_run~ID,ubdata[ubdata$extra_type %in% c(NA,"wides","noballs"),],sum)
    match_wkt<-aggregate(isWicketDelivery~ID,ubdata[! ubdata$kind %in% c("run out","retired out","obstructing the field"),],sum)
    best_data<-merge(match_run,match_wkt,all.x = TRUE,by = "ID")
    best_data[is.na(best_data$isWicketDelivery),"isWicketDelivery"]<-0
    best_run<-min(best_data$total_run[best_data$isWicketDelivery==max(best_data$isWicketDelivery)])
    BBM<-noquote(capture.output(cat(max(best_data$isWicketDelivery),"/",best_run,sep = "")))
    Economy<-round(Runs_concede/(BD/6),2)
    if(Wkt==0){Avg<-Runs_concede;SR<-BD}
    else{
      Avg<-round(Runs_concede/Wkt,2)
      SR<-round(BD/Wkt,2)}
    five_wkt<-nrow(best_data[best_data$isWicketDelivery>4,])
    four_wkt<-nrow(best_data[best_data$isWicketDelivery>3,])-five_wkt
    team_data[i,c("Team","Inn","Bowl Bowled","Runs Concede","Wickets",
                  "Best Bowling","Economy","Average","Strike Rate","Four Wicket",
                  "Five Wicket")]<-
      c(unique(ubdata$BattingTeam),Inn,BD,Runs_concede,Wkt,BBM,Economy,Avg,SR,
        four_wkt,five_wkt)
    team_data<-team_data[is.na(team_data$Team)!=TRUE,]
  }
  team_data[,c(2:5,7:11)]<-apply(team_data[ ,c(2:5,7:11)], 2,
                          function(x) as.numeric(x))
  cat("__________________________________________________________________________________")
  cat("\nTeam Against Performance\n\n")
  team_data<-team_data[order(team_data$Inn,decreasing = TRUE),]
  rownames(team_data)<-1:nrow(team_data)
  print(team_data)
  
  #Batsman Wise distribution
  batsman_wise<-data.frame()
  for(i in 1:length(unique(subdata$batter))){
    if(is.na(unique(subdata$batter)[i])){next}
    ubdata<-subset(subdata,subdata$batter==unique(subdata$batter)[i])
    Inn<-length(unique(ubdata$ID))
    BD<-nrow(ubdata[ubdata$extra_type %in% c( NA,"legbyes","byes"),])
    Runs_concede<-sum(ubdata$total_run[ubdata$extra_type %in% c(NA,"wides","noballs")])
    Wkt<-length(ubdata$isWicketDelivery[! ubdata$kind %in% c("run out",NA,"retired out","obstructing the field")])
    Economy<-round(Runs_concede/(BD/6),2)
    if(Wkt==0){Avg<-Runs_concede;SR<-BD}
    else{
      Avg<-round(Runs_concede/Wkt,2)
      SR<-round(BD/Wkt,2)}
    batsman_wise[i,c("Batter","Inn","Bowl Bowled","Runs Concede","Wickets",
                     "Economy","Average","Strike Rate")]<-
      c(unique(ubdata$batter),Inn,BD,Runs_concede,Wkt,Economy,Avg,SR)
    batsman_wise<-batsman_wise[is.na(batsman_wise$Batter)!=TRUE,]
  }
  batsman_wise[,2:8]<-apply(batsman_wise[ ,2:8], 2,
                            function(x) as.numeric(x))
  if(nrow(batsman_wise)>10){batsman_wise<-subset(batsman_wise,`Bowl Bowled`>12)
  if(nrow(batsman_wise)==0){
    cat("__________________________________________________________________________________")
    cat("\n",Bowler_Name,"not bowl atleast 12 bowl to any batsman\n\n")
    
  }
  else{
  batsman_wise<-batsman_wise[order(batsman_wise$Economy,decreasing = TRUE),]
  rownames(batsman_wise)<-1:nrow(batsman_wise)
  cat("__________________________________________________________________________________")
  cat("\n",Bowler_Name,"don't want to bowl, these batsman\n\n")
  print(head(batsman_wise,10))
  cat("__________________________________________________________________________________")
  cat("\n",Bowler_Name,"has an upper hand on these batsman ->\n\n")
  print(tail(batsman_wise,10))}}
  else{batsman_wise<-batsman_wise
  cat("__________________________________________________________________________________")
  cat("\n",Bowler_Name,"not bowl many batsman\n\n")
  }
  
  #Venue wise distribution
  venue_data<-data.frame()
  for(i in 1:length(unique(subdata$Venue))){
    if(is.na(unique(subdata$Venue)[i])){next}
    ubdata<-subset(subdata,subdata$Venue==unique(subdata$Venue)[i])
    Inn<-length(unique(ubdata$ID))
    BD<-nrow(ubdata[ubdata$extra_type %in% c( NA,"legbyes","byes"),])
    Runs_concede<-sum(ubdata$total_run[ubdata$extra_type %in% c(NA,"wides","noballs")])
    Wkt<-length(ubdata$isWicketDelivery[! ubdata$kind %in% c("run out",NA,"retired out","obstructing the field")])
    match_run<-aggregate(total_run~ID,ubdata[ubdata$extra_type %in% c(NA,"wides","noballs"),],sum)
    match_wkt<-aggregate(isWicketDelivery~ID,ubdata[! ubdata$kind %in% c("run out","retired out","obstructing the field"),],sum)
    best_data<-merge(match_run,match_wkt,all.x = TRUE,by = "ID")
    best_data[is.na(best_data$isWicketDelivery),"isWicketDelivery"]<-0
    best_run<-min(best_data$total_run[best_data$isWicketDelivery==max(best_data$isWicketDelivery)])
    BBM<-noquote(capture.output(cat(max(best_data$isWicketDelivery),"/",best_run,sep = "")))
    Economy<-round(Runs_concede/(BD/6),2)
    if(Wkt==0){Avg<-Runs_concede;SR<-BD}
    else{
      Avg<-round(Runs_concede/Wkt,2)
      SR<-round(BD/Wkt,2)}
    five_wkt<-nrow(best_data[best_data$isWicketDelivery>4,])
    four_wkt<-nrow(best_data[best_data$isWicketDelivery>3,])-five_wkt
    venue_data[i,c("Venue","City","Inn","Bowl Bowled","Runs Concede","Wickets",
                   "Best Bowling","Economy","Average","Strike Rate","Four Wicket",
                   "Five Wicket")]<-
      c(unique(ubdata$Venue),unique(ubdata$City)[1],Inn,BD,Runs_concede,Wkt,BBM,Economy,Avg,SR,
        four_wkt,five_wkt)
    venue_data<-venue_data[is.na(venue_data$Venue)!=TRUE,]
  }
  venue_data[,c(3:6,8:12)]<-apply(venue_data[ ,c(3:6,8:12)], 2,
                           function(x) as.numeric(x))
  venue_data<-venue_data[order(venue_data$Inn,decreasing = TRUE),]
  cat("__________________________________________________________________________________")
  cat("\nPerformance on Venue\n\n")
  rownames(venue_data)<-1:nrow(venue_data)
  print(head(venue_data,10))
  
  final<-list("Bowler_name"=Bowler_Name,"Player_Data"=subdata,
              "Over_interval"=over_interval_wise,"inning_data"=inning_data,
              "Season_wise"=season_data,"Team_wise"=team_data,
              "Venue_wise"=venue_data,"Batsman_wise"=batsman_wise)
  return(final)
}
