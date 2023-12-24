Player_Profile<-function(Player_Name,Start_Season=2008,End_Season=2022){
  if(Player_Name %in% all_ball_data$batter ==FALSE){
    if(length(unique(all_ball_data$batter[grepl(Player_Name,all_ball_data$batter)]))==0){
      cat("Please correctly mention the player name\nhint:- Write surname of the player with capital first letter\n")
      stop()
    }
    print("Possible player name that you are looking --->")
    print(data.frame("No."=1:length(unique(all_ball_data$batter[grepl(Player_Name,all_ball_data$batter)])),
                     "Player"=unique(all_ball_data$batter[grepl(Player_Name,all_ball_data$batter)])))
    print("Select the number corresponding to your player")
    a<-readline()
    a<-as.numeric(a)
    Player_Name<-unique(all_ball_data$batter[grepl(Player_Name,all_ball_data$batter)])[a]
  }
  subdata1<-subset(all_ball_data,batter==Player_Name & Season>=Start_Season & Season<=End_Season)
  if(nrow(subdata1)==0){cat("Your Player Doesn't play in the given season\n");stop()}
  subdata<-subset(subdata1,extra_type %in% c("legbyes",NA,"byes","noballs"))
  `%notin%`<-Negate(`%in%`)
  Inn<-length(unique(subdata$ID))
  Out<-nrow(subset(all_ball_data,all_ball_data$player_out==Player_Name & Season>=Start_Season & Season<=End_Season))
  NO<-Inn-Out
  Runs<-sum(subdata$batsman_run,na.rm = TRUE)
  table1<-aggregate(batsman_run~ID,subdata,
                    function(x) sum(x,na.rm = TRUE))
  status<-NULL
  for(i in 1:length(unique(subdata$ID))){
    if(nrow(subset(all_ball_data,ID==unique(subdata$ID)[i] & player_out==Player_Name))>0){
      status[i]<-""
    }
    else{status[i]<-"*"}
  }
  table<-data.frame("ID"=unique(subdata$ID),"Status"=status,"batsman_run"=table1$batsman_run)
  table<-table[order(table$batsman_run,decreasing = TRUE),]
  ducks<-nrow(table[table$batsman_run==0,])
  if(Out==0){Avg<-Runs}
  else{Avg<-round(Runs/Out,2)}
  BF<-nrow(subdata)
  if(Runs<11 | BF<11){
    cat(Player_Name,"played less then 10 balls or score less then 10 runs in his IPL carrier.\n",
    "So, we don't have much to show\n")
    stop()
  }
  SR<-round((Runs/BF)*100,1)
  fifty<-nrow(table[table$batsman_run>=50,])
  hundred<-nrow(table[table$batsman_run>=100,])
  fours<-nrow(subdata[subdata$batsman_run==4,])
  six<-nrow(subdata[subdata$batsman_run==6,])
  running<-round((sum(subdata$batsman_run[subdata$batsman_run!=4 & subdata$batsman_run!=6])*1/Runs)*100,2)
  four_runs<-round((4*fours/Runs)*100,2)
  six_runs<-round((6*six/Runs)*100,2)
  MoM<-unique(subdata[subdata$Player_of_Match==Player_Name,c("ID","Player_of_Match")])
  MoM<-nrow(na.omit(MoM))
  Dot_per<-round((nrow(subset(subdata,extra_type %notin% "wides" & batsman_run==0))/
                    nrow(subset(subdata,extra_type %notin% "wides")))*100,2)
  boundary_per<-round(nrow(subset(subdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                        nrow(subset(subdata,extra_type %notin% "wides"))*100,2)  
  par(mfrow=c(1,2))
  pie(c(running,four_runs,six_runs),
      labels = paste(c(running,four_runs,six_runs),"%",sep=""),
      col=c("#DF536B","#61D04F","#2297E6"),
      main=paste("Run Distribution"))
  legend("bottom", c("Running","Four","Six"),cex=0.8,
         fill = c("#DF536B","#61D04F","#2297E6"),horiz = TRUE)
  cat("_______________________",Player_Name,"(",min(subdata$Season),",",max(subdata$Season),")","_________________________________
Performance :-
      
      Innings      -   ",Inn,"
      Not Outs     -   ",NO,"
      Runs         -   ",Runs,"
      Ball Faced   -   ",BF,"
      High Score   -   ",table[1,3],table[1,2],"
      Average      -   ",Avg,"
      Strike Rate  -   ",SR,"
      50*          -   ",fifty-hundred,"
      100*         -   ",hundred,"
      4s           -   ",fours,"
      6s           -   ",six,"
      Man Of Match -   ",MoM,"
      Dot Percent  -   ",Dot_per,"%
      Boundary Percent -   ",boundary_per,"%
      Ducks        -   ",ducks,"\n",sep="")
  
  poistion_data<-data.frame()
  temp3<-aggregate(Position~ID,subdata,min)
  for(i in 1:length(unique(temp3$Position))){
    if(is.na(unique(temp3$Position)[i])){next}
    matchid<-temp3$ID[temp3$Position==unique(temp3$Position)[i]]
    ubdata<-subset(subdata,subdata$ID %in% matchid)
    Inn<-length(unique(ubdata$ID))
    Out<-nrow(subset(all_ball_data,all_ball_data$player_out==Player_Name & all_ball_data$ID %in% matchid & 
                       Season>=Start_Season & Season<=End_Season))
    NO<-Inn-Out
    Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
    table1<-aggregate(batsman_run~ID,ubdata,
                      function(x) sum(x,na.rm = TRUE))
    temp001<-subset(all_ball_data,ID %in% matchid & Season>=Start_Season & Season<=End_Season)
    status<-NULL
    for(j in 1:length(unique(ubdata$ID))){
      if(nrow(subset(temp001,ID==unique(ubdata$ID)[j] & player_out==Player_Name))>0){
        status[j]<-""
      }
      else{status[j]<-"*"}
    }
    table<-data.frame("ID"=unique(ubdata$ID),"Status"=status,"batsman_run"=table1$batsman_run)
    table<-table[order(table$batsman_run,decreasing = TRUE),]
    table$High_Score<-paste(table$batsman_run,table$Status,sep = "")
    High_Score<-table$High_Score[1]
    ducks<-nrow(table[table$batsman_run==0,])
    {if(Out==0){Avg<-Runs}
      else{Avg<-round(Runs/Out,2)}}
    BF<-nrow(ubdata)
    SR<-round((Runs/BF)*100,1)
    hundred<-nrow(table[table$batsman_run>=100,])
    fifty<-nrow(table[table$batsman_run>=50,])-hundred
    fours<-nrow(ubdata[ubdata$batsman_run==4,])
    six<-nrow(ubdata[ubdata$batsman_run==6,])
    Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                      nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
    boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                          nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
    MoM<-unique(ubdata[ubdata$Player_of_Match==Player_Name,c("ID","Player_of_Match")])
    MoM<-nrow(na.omit(MoM))
    poistion_data[i,c("Play_Position","Inn","NO","Runs","Ball Faced","High_Score","Avg","SR",
                      "fifty","hundred","fours","six","Dot_Percent","Boundary_Percent","Man of Match","Ducks")]<-
      c(unique(temp3$Position)[i]+2,Inn,NO,Runs,BF,High_Score,Avg,SR,
        fifty,hundred,fours,six,Dot_per,boundary_per,MoM,ducks)
    poistion_data<-poistion_data[is.na(poistion_data$Play_Position)!=TRUE,]
  }
  poistion_data[,c(2:5,7:14)]<-apply(poistion_data[,c(2:5,7:14)], 2,
                                     function(x) as.numeric(x))
  poistion_data<-poistion_data[order(poistion_data$Play_Position,decreasing = FALSE),]
  rownames(poistion_data)<-1:nrow(poistion_data)
  cat("__________________________________________________________________________________")
  cat("\nPosition wise Performance\n\n")
  print(poistion_data)
  
  
  #BEST Performance
  table1<-aggregate(batsman_run~ID,subdata,
                    function(x) sum(x,na.rm = TRUE))
  status<-NULL
  for(i in 1:length(unique(subdata$ID))){
    if(nrow(subset(all_ball_data,ID==unique(subdata$ID)[i] & player_out==Player_Name))>0){
      status[i]<-""
    }
    else{status[i]<-"*"}
  }
  table<-data.frame("ID"=unique(subdata$ID),"Status"=status,"batsman_run"=table1$batsman_run)
  table<-table[order(table$batsman_run,decreasing = TRUE),]
  table$High_Score<-paste(table$batsman_run,table$Status,sep = "")
  best_performance<-data.frame()
  for(i in 1:nrow(table)){
    ubdata<-subset(subdata,subdata$ID %in% table$ID[i])
    Season<-unique(ubdata$Season)[1]
    venue<-unique(ubdata$Venue)[1]
    Batting_Team<-unique(ubdata$BattingTeam)[1]
    Bowling_Team<-unique(ubdata$bowler_team)[1]
    inning<-unique(ubdata$innings)[1]
    Runs<-table$High_Score[i]
    BF<-nrow(ubdata)
    SR<-round((table$batsman_run[i]/BF)*100,1)
    fours<-nrow(ubdata[ubdata$batsman_run==4,])
    six<-nrow(ubdata[ubdata$batsman_run==6,])
    Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                      nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
    boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                          nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
    MoM<-unique(ubdata[ubdata$Player_of_Match==Player_Name,c("ID","Player_of_Match")])
    MoM<-nrow(na.omit(MoM))
    best_performance[i,c("Season","Venue","Batting_Team","Bowling_Team","Inning","Runs",
                         "Ball Faced","SR","fours","Six","Dot_Percent","Boundary_Percent","Man of Match")]<-
      c(Season,venue,Batting_Team,Bowling_Team,inning,Runs,BF,SR,fours,six,Dot_per,boundary_per,MoM)
  }
  best_performance[,c(5,7:13)]<-apply(best_performance[,c(5,7:13)], 2,
                                 function(x) as.numeric(x))
  cat("__________________________________________________________________________________")
  cat("\nBest Performances of",Player_Name,"\n\n")
  print(head(best_performance,10))
  
  #Over_interval data
  over_interval_wise<-data.frame()
  for(i in 1:length(unique(subdata$over_interval))){
    if(is.na(unique(subdata$over_interval)[i])){next}
    ubdata<-subset(subdata,subdata$over_interval==unique(subdata$over_interval)[i])
    Inn<-length(unique(ubdata$ID))
    Out<-nrow(subset(all_ball_data,all_ball_data$player_out==Player_Name & all_ball_data$over_interval==unique(subdata$over_interval)[i] & 
                       Season>=Start_Season & Season<=End_Season))
    NO<-Inn-Out
    Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
    table<-aggregate(batsman_run~ID,ubdata,
                     function(x) sum(x,na.rm = TRUE))
    High_Score<-max(table[,2])
    {if(Out==0){Avg<-Runs}
      else{Avg<-round(Runs/Out,2)}}
    BF<-nrow(ubdata)
    SR<-round((Runs/BF)*100,1)
    fours<-nrow(ubdata[ubdata$batsman_run==4,])
    six<-nrow(ubdata[ubdata$batsman_run==6,])
    Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                      nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
    boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                          nrow(subset(ubdata,extra_type %notin% "wides"))*100,2)
    over_interval_wise[i,c("over_interval","Out","Runs","Ball Faced","Avg","SR","fours","six","Dot_Percent","Boundary_Percent")]<-
      c(unique(ubdata$over_interval),Out,Runs,BF,Avg,SR,fours,six,Dot_per,boundary_per)
    over_interval_wise<-over_interval_wise[is.na(over_interval_wise$over_interval)!=TRUE,]
  }
  over_interval_wise[,2:10]<-apply(over_interval_wise[ ,2:10], 2,
                                  function(x) as.numeric(x))
  over_interval_wise<-over_interval_wise[order(match(over_interval_wise$over_interval,c("01-06","07-15","16-20"))),]
  cat("__________________________________________________________________________________")
  cat("\nOver Interval Performance\n\n")
  print(over_interval_wise)
  
  # Inning Wise DATA
  inning_data<-data.frame()
  for(i in 1:length(unique(subdata$innings))){
    if(is.na(unique(subdata$innings)[i])){next}
    ubdata<-subset(subdata,subdata$innings==unique(subdata$innings)[i])
    Inn<-length(unique(ubdata$ID))
    Out<-nrow(subset(all_ball_data,all_ball_data$player_out==Player_Name & all_ball_data$innings==unique(subdata$innings)[i] & 
                       Season>=Start_Season & Season<=End_Season))
    NO<-Inn-Out
    Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
    table1<-aggregate(batsman_run~ID,ubdata,
                      function(x) sum(x,na.rm = TRUE))
    temp001<-subset(all_ball_data,innings==unique(subdata$innings)[i] & Season>=Start_Season & Season<=End_Season)
    status<-NULL
    for(j in 1:length(unique(ubdata$ID))){
      if(nrow(subset(temp001,ID==unique(ubdata$ID)[j] & player_out==Player_Name))>0){
        status[j]<-""
      }
      else{status[j]<-"*"}
    }
    table<-data.frame("ID"=unique(ubdata$ID),"Status"=status,"batsman_run"=table1$batsman_run)
    table<-table[order(table$batsman_run,decreasing = TRUE),]
    table$High_Score<-paste(table$batsman_run,table$Status,sep = "")
    High_Score<-table$High_Score[1]
    ducks<-nrow(table[table$batsman_run==0,])
    {if(Out==0){Avg<-Runs}
      else{Avg<-round(Runs/Out,2)}}
    BF<-nrow(ubdata)
    SR<-round((Runs/BF)*100,1)
    hundred<-nrow(table[table$batsman_run>=100,])
    fifty<-nrow(table[table$batsman_run>=50,])-hundred
    fours<-nrow(ubdata[ubdata$batsman_run==4,])
    six<-nrow(ubdata[ubdata$batsman_run==6,])
    Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                      nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
    boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                          nrow(subset(ubdata,extra_type %notin% "wides"))*100,2)
    MoM<-unique(ubdata[ubdata$Player_of_Match==Player_Name,c("ID","Player_of_Match")])
    MoM<-nrow(na.omit(MoM))
    inning_data[i,c("Inning","Inn","NO","Runs","Ball Faced","High_Score","Avg","SR",
                    "fifty","hundred","fours","six","Dot_Percent","Boundary_Percent","Man of Match","Ducks")]<-
      c(unique(ubdata$innings),Inn,NO,Runs,BF,High_Score,Avg,SR,
        fifty,hundred,fours,six,Dot_per,boundary_per,MoM,ducks)
    inning_data<-inning_data[is.na(inning_data$Inning)!=TRUE,]
  }
  inning_data[,c(2:5,7:14)]<-apply(inning_data[ ,c(2:5,7:14)], 2,
                                   function(x) as.numeric(x))
  cat("__________________________________________________________________________________")
  cat("\nInning wise Performance\n\n")
  print(inning_data)
  
  #Partnerships
  partnership_data<-data.frame()
  temp01<-aggregate(batsman_run~`non-striker`+ID,data = subdata,sum)
  temp01<-subset(temp01,batsman_run>20)
  if(nrow(temp01)==0){cat("\n",Player_Name,"doesn't have any partnership greater than 20 runs\n")}
  if(nrow(temp01)>0){
    temp<-aggregate(total_run~`non-striker`+ID,data = subdata,sum)
    temp<-temp[rownames(temp01),]
    temp0<-aggregate(total_run~`non-striker`+ID,data = subdata,length)
    temp0<-temp0[rownames(temp),]
    for(i in 1:nrow(temp)){
      temp1<-subset(all_ball_data,batter==temp[i,1] & ID==temp[i,2] & `non-striker`==Player_Name)
      if(nrow(temp1)==0){next}
      Player1<-Player_Name
      Player2<-temp[i,1]
      Player1_Run<-temp01[i,3]
      Player1_BF<-temp0[i,3]
      Player2_Run<-sum(temp1$batsman_run)
      Player2_BF<-nrow(temp1)
      total<-temp[i,3]+sum(temp1$total_run)
      Extra<-total-(Player1_Run + Player2_Run)
      partnership_data[i,c("Season","Venue","Inning","Batting_team","Bowling_Team","Player1","Player2",
                           "Player1_Run","Player2_Run","Player1_BAllFaced","Player2_BAllFaced",
                           "Partnership","Extra")]<-c(unique(temp1$Season),unique(temp1$Venue),unique(temp1$innings)[1],
                                                      unique(temp1$BattingTeam),unique(temp1$bowler_team),Player1,
                                                      Player2,Player1_Run,Player2_Run,Player1_BF,Player2_BF,total,Extra)
      #partnership_data<-partnership_data[is.na(partnership_data$Partnership)!=TRUE,]
    }
    partnership_data[,8:13]<-apply(partnership_data[ ,8:13], 2,
                                   function(x) as.numeric(x))
    partnership_data<-na.omit(partnership_data)
    partnership_data<-partnership_data[order(partnership_data$Partnership,decreasing = TRUE),]
    rownames(partnership_data)<-1:nrow(partnership_data)
    cat("__________________________________________________________________________________")
    cat("\nPartnership of",Player_Name,"\n\n")
    print(head(partnership_data,10))}
  
  # SEASON Wise DATA
  season_data<-data.frame()
  for(i in 1:length(unique(subdata$Season))){
    ubdata<-subdata[subdata$Season==unique(subdata$Season)[i],]
    Inn<-length(unique(ubdata$ID))
    Out<-nrow(subset(all_ball_data,all_ball_data$player_out==Player_Name & all_ball_data$Season==unique(subdata$Season)[i] & 
                       Season>=Start_Season & Season<=End_Season))
    NO<-Inn-Out
    Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
    table1<-aggregate(batsman_run~ID,ubdata,
                      function(x) sum(x,na.rm = TRUE))
    temp001<-subset(all_ball_data,Season==unique(subdata$Season)[i] & Season>=Start_Season & Season<=End_Season)
    status<-NULL
    for(j in 1:length(unique(ubdata$ID))){
      if(nrow(subset(temp001,ID==unique(ubdata$ID)[j] & player_out==Player_Name))>0){
        status[j]<-""
      }
      else{status[j]<-"*"}
    }
    table<-data.frame("ID"=unique(ubdata$ID),"Status"=status,"batsman_run"=table1$batsman_run)
    table<-table[order(table$batsman_run,decreasing = TRUE),]
    table$High_Score<-paste(table$batsman_run,table$Status,sep = "")
    High_Score<-table$High_Score[1]
    ducks<-nrow(table[table$batsman_run==0,])
    {if(Out==0){Avg<-Runs}
      else{Avg<-round(Runs/Out,2)}}
    BF<-nrow(ubdata)
    SR<-round((Runs/BF)*100,1)
    hundred<-nrow(table[table$batsman_run>=100,])
    fifty<-nrow(table[table$batsman_run>=50,])-hundred
    fours<-nrow(ubdata[ubdata$batsman_run==4,])
    six<-nrow(ubdata[ubdata$batsman_run==6,])
    Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                      nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
    boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                          nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
    MoM<-unique(ubdata[ubdata$Player_of_Match==Player_Name,c("ID","Player_of_Match")])
    MoM<-nrow(na.omit(MoM))
    season_data[i,c("Season","Inn","NO","Runs","Ball Faced",
                    "High_Score","Avg","SR","fifty",
                    "hundred","fours","six","Dot_Percent","Boundary_Percent","Man of Match","Ducks")]<-c(unique(ubdata$Season),Inn,NO,Runs,BF,
                                                                        High_Score,Avg,SR,fifty,hundred,fours,six,Dot_per,boundary_per,MoM,ducks)
  }
  season_data[,c(1:5,7:14)]<-apply(season_data[ ,c(1:5,7:14)], 2,
                                   function(x) as.numeric(x))
  plot(x=season_data$Avg,y=season_data$SR,xlab = "Average",type="n",
       ylab = "Strike Rate",lwd=2,panel.first=grid(),
       xlim<-c(min(season_data$Avg)-5,max(season_data$Avg)+5),
       ylim<-c(min(season_data$SR)-10,max(season_data$SR)+10),
       main=paste("Average v/s Strike Rate"))
  text(season_data$Avg,season_data$SR+1,labels=season_data$Season)
  par(mfrow=c(1,1))
  mtext(paste(Player_Name),side=3,line=-2,outer=TRUE,cex=1.5)
  cat("__________________________________________________________________________________")
  cat("\nYear on Year Performance\n\n")
  print(season_data)
  
  # Team Wise DATA
  team_data<-data.frame()
  for(i in 1:length(unique(subdata$bowler_team))){
    if(is.na(unique(subdata$bowler_team)[i])){next}
    ubdata<-subset(subdata,subdata$bowler_team==unique(subdata$bowler_team)[i])
    Inn<-length(unique(ubdata$ID))
    Out<-nrow(subset(all_ball_data,all_ball_data$player_out==Player_Name & all_ball_data$bowler_team==unique(subdata$bowler_team)[i] & 
                       Season>=Start_Season & Season<=End_Season))
    NO<-Inn-Out
    Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
    table1<-aggregate(batsman_run~ID,ubdata,
                      function(x) sum(x,na.rm = TRUE))
    temp001<-subset(all_ball_data,bowler_team==unique(subdata$bowler_team)[i] & Season>=Start_Season & Season<=End_Season)
    status<-NULL
    for(j in 1:length(unique(ubdata$ID))){
      if(nrow(subset(temp001,ID==unique(ubdata$ID)[j] & player_out==Player_Name))>0){
        status[j]<-""
      }
      else{status[j]<-"*"}
    }
    table<-data.frame("ID"=unique(ubdata$ID),"Status"=status,"batsman_run"=table1$batsman_run)
    table<-table[order(table$batsman_run,decreasing = TRUE),]
    table$High_Score<-paste(table$batsman_run,table$Status,sep = "")
    High_Score<-table$High_Score[1]
    ducks<-nrow(table[table$batsman_run==0,])
    {if(Out==0){Avg<-Runs}
      else{Avg<-round(Runs/Out,2)}}
    BF<-nrow(ubdata)
    SR<-round((Runs/BF)*100,1)
    hundred<-nrow(table[table$batsman_run>=100,])
    fifty<-nrow(table[table$batsman_run>=50,])-hundred
    fours<-nrow(ubdata[ubdata$batsman_run==4,])
    six<-nrow(ubdata[ubdata$batsman_run==6,])
    Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                      nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
    boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                          nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
    MoM<-unique(ubdata[ubdata$Player_of_Match==Player_Name,c("ID","Player_of_Match")])
    MoM<-nrow(na.omit(MoM))
    team_data[i,c("Team","Inn","NO","Runs","Ball Faced","High_Score","Avg","SR",
                  "fifty","hundred","fours","six","Dot_Percent","Boundary_Percent","Man of Match","Ducks")]<-
      c(unique(ubdata$bowler_team),Inn,NO,Runs,BF,High_Score,Avg,SR,
        fifty,hundred,fours,six,Dot_per,boundary_per,MoM,ducks)
    team_data<-team_data[is.na(team_data$Team)!=TRUE,]
  }
  team_data[,c(2:5,7:14)]<-apply(team_data[ ,c(2:5,7:14)], 2,
                                 function(x) as.numeric(x))
  cat("__________________________________________________________________________________")
  cat("\nTeam Against Performance\n\n")
  team_data<-team_data[order(team_data$Avg,decreasing = TRUE),]
  rownames(team_data)<-1:nrow(team_data)
  print(team_data)
  
  #Bowler Wise distribution
  bowler_wise<-data.frame()
  for(i in 1:length(unique(subdata$bowler))){
    if(is.na(unique(subdata$bowler)[i])){next}
    ubdata<-subset(subdata,subdata$bowler==unique(subdata$bowler)[i])
    Inn<-length(unique(ubdata$ID))
    Out<-nrow(subset(ubdata,player_out==Player_Name & kind!="run out"))
    NO<-Inn-Out
    Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
    table<-aggregate(batsman_run~ID,ubdata,
                     function(x) sum(x,na.rm = TRUE))
    High_Score<-max(table[,2])
    {if(Out==0){Avg<-Runs}
      else{Avg<-round(Runs/Out,2)}}
    BF<-nrow(ubdata)
    SR<-round((Runs/BF)*100,1)
    fours<-nrow(ubdata[ubdata$batsman_run==4,])
    six<-nrow(ubdata[ubdata$batsman_run==6,])
    Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                      nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
    boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                          nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
    bowler_wise[i,c("Bowler","Inn","Out","Runs","Ball Deliver","Avg","SR","fours","six","Dot_Percent","Boundary_Percent")]<-
      c(unique(ubdata$bowler),Inn,Out,Runs,BF,Avg,SR,fours,six,Dot_per,boundary_per)
    bowler_wise<-bowler_wise[is.na(bowler_wise$Bowler)!=TRUE,]
  }
  bowler_wise[,2:11]<-apply(bowler_wise[ ,2:11], 2,
                           function(x) as.numeric(x))
  bowler_wise_out<-bowler_wise[order(bowler_wise$Out,decreasing = TRUE),]
  bowler_wise_SR<-bowler_wise[order(bowler_wise$Avg,decreasing = TRUE),]
  cat("__________________________________________________________________________________")
  cat("\nHigh Risk Bowler to",Player_Name,"\n\n")
  rownames(bowler_wise_out)<-1:nrow(bowler_wise_out)
  print(head(bowler_wise_out,10))
  cat("__________________________________________________________________________________")
  cat("\n",Player_Name,"enjoy to play these bowlers ->\n\n")
  rownames(bowler_wise_SR)<-1:nrow(bowler_wise_SR)
  print(head(bowler_wise_SR,10))
  
  #Venue wise distribution
  venue_data<-data.frame()
  for(i in 1:length(unique(subdata$Venue))){
    if(is.na(unique(subdata$Venue)[i])){next}
    ubdata<-subset(subdata,subdata$Venue==unique(subdata$Venue)[i])
    Inn<-length(unique(ubdata$ID))
    Out<-nrow(subset(all_ball_data,all_ball_data$player_out==Player_Name & all_ball_data$Venue==unique(subdata$Venue)[i] & 
                       Season>=Start_Season & Season<=End_Season))
    NO<-Inn-Out
    Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
    table1<-aggregate(batsman_run~ID,ubdata,
                      function(x) sum(x,na.rm = TRUE))
    temp001<-subset(all_ball_data,Venue==unique(subdata$Venue)[i] & Season>=Start_Season & Season<=End_Season)
    status<-NULL
    for(j in 1:length(unique(ubdata$ID))){
      if(nrow(subset(temp001,ID==unique(ubdata$ID)[j] & player_out==Player_Name))>0){
        status[j]<-""
      }
      else{status[j]<-"*"}
    }
    table<-data.frame("ID"=unique(ubdata$ID),"Status"=status,"batsman_run"=table1$batsman_run)
    table<-table[order(table$batsman_run,decreasing = TRUE),]
    table$High_Score<-paste(table$batsman_run,table$Status,sep = "")
    High_Score<-table$High_Score[1]
    ducks<-nrow(table[table$batsman_run==0,])
    {if(Out==0){Avg<-Runs}
      else{Avg<-round(Runs/Out,2)}}
    BF<-nrow(ubdata)
    SR<-round((Runs/BF)*100,1)
    hundred<-nrow(table[table$batsman_run>=100,])
    fifty<-nrow(table[table$batsman_run>=50,])-hundred
    fours<-nrow(ubdata[ubdata$batsman_run==4,])
    six<-nrow(ubdata[ubdata$batsman_run==6,])
    Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                      nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
    boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                          nrow(subset(ubdata,extra_type %notin% "wides"))*100,2)
    MoM<-unique(ubdata[ubdata$Player_of_Match==Player_Name,c("ID","Player_of_Match")])
    MoM<-nrow(na.omit(MoM))
    venue_data[i,c("Venue","City","Inn","NO","Runs","Ball Faced","High_Score","Avg","SR",
                   "fifty","hundred","fours","six","Dot_Percent","Boundary_Percent","Man of Match","Ducks")]<-
      c(unique(ubdata$Venue),unique(ubdata$City)[1],Inn,NO,Runs,BF,High_Score,Avg,SR,
        fifty,hundred,fours,six,Dot_per,boundary_per,MoM,ducks)
    venue_data<-venue_data[is.na(venue_data$Venue)!=TRUE,]
  }
  venue_data[,c(3:6,8:16)]<-apply(venue_data[ ,c(3:6,8:16)], 2,
                                  function(x) as.numeric(x))
  venue_data<-venue_data[order(venue_data$Inn,decreasing = TRUE),]
  cat("__________________________________________________________________________________")
  cat("\nPerformance on Venue\n\n")
  rownames(venue_data)<-1:nrow(venue_data)
  print(head(venue_data,10))
  
  final<-list("Player_name"=Player_Name,"Player_Data"=subdata,
              "Over_interval"=over_interval_wise,"Season_wise"=season_data,
              "Team_wise"=team_data,"Bowler_wise"=bowler_wise,
              "Venue_wise"=venue_data,"Playing_Position"=poistion_data,
              "Partnership_data"=partnership_data,"inning_data"=inning_data,
              "Best_Performances"=best_performance)
  return(final)
}
dev.off()


#___________________________________________________________________________
subdata<-all_ball_data[all_ball_data$batter=="SK Raina",]
test<-Player_Profile("SR Tendulkar")
test<-Player_Profile("MS Dhoni")
test<-Player_Profile("CH Gayle")
test<-Player_Profile("V Kohli")
test<-Player_Profile("V Sehwag",Start_Season = 2022)
test<-Player_Profile("AB de Villiers")
test<-Player_Profile("Yuvraj Singh")





