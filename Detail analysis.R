Player_Performance<-function(data,Bowler_name=NULL,stadium=FALSE,Inning=3,Over_interval="01-20"){
  if(stadium != TRUE & stadium != FALSE){
    cat("Your input in stadium attribute is wrong\nStadium only take TRUE or FALSE as a value\n")
    stop()
    }
  if(Inning != "n" & Inning != 1 & Inning != 2 & Inning != 3){
    cat("Your input in Inning attribute is wrong\nInning only take n,1,2,3 as a value\nn=NO inning\n1=1st inning\n2=2nd inning\n3=All Inning\n")
    stop()
  }
  if(Over_interval != "01-06" & Over_interval != "07-15" & Over_interval != "16-20" & Over_interval != "01-20" & Over_interval != "n"){
    cat("Your input in Over_interval attribute is wrong\nOver_interval only take 'n','01-06','07-15','16-20','01-20' as a value\n")
    stop()
  }
  if(is.null(Bowler_name)==FALSE)  #Bowler NAME
  if(Bowler_name %in% data$Player_Data$bowler ==FALSE){
    if(length(unique(data$Player_Data$bowler[grepl(Bowler_name,data$Player_Data$bowler)]))==0){
      cat("Please correctly mention the bowler name\nhint:- Write surname of the player with capital first letter\n")
      stop()
    }
    cat("Possible bowler that you are looking --->\n")
    print(data.frame("No."=1:length(unique(data$Player_Data$bowler[grepl(Bowler_name,data$Player_Data$bowler)])),
                     "Player"=unique(data$Player_Data$bowler[grepl(Bowler_name,data$Player_Data$bowler)])))
    cat("Select the number corresponding to your player\n")
    a<-readline()
    a<-as.numeric(a)
    Bowler_name<-unique(data$Player_Data$bowler[grepl(Bowler_name,data$Player_Data$bowler)])[a]
  }
  `%notin%`<-Negate('%in%')
  result<-data.frame()
  #Only Bowler name
  if(is.null(Bowler_name)==FALSE & stadium==FALSE & Inning=="n" & Over_interval=="n"){
    result<-data$Bowler_wise[data$Bowler_wise$Bowler==Bowler_name,]
  }
  #Only Stadium 
  if(is.null(Bowler_name)==TRUE & stadium==TRUE & Inning=="n" & Over_interval=="n"){
          cat("Select the stadium for analysis\n")
      print(data.frame("No."=0:length(unique(data$Player_Data$Venue)),
                       "Player"=c("ALL",unique(data$Player_Data$Venue[order(data$Player_Data$Venue)]))))
      b<-scan(what = integer())
      ifelse(b==0,venue<-unique(data$Player_Data$Venue[order(data$Player_Data$Venue)]),
             venue<-unique(data$Player_Data$Venue[order(data$Player_Data$Venue)])[b])
    result<-data$Venue_wise[data$Venue_wise$Venue %in% venue,]
  }
  #Only Inning
  if(is.null(Bowler_name)==TRUE & stadium==FALSE & Inning!="n" & Over_interval=="n"){
    if(Inning %in% c(1,2)){
      result<-data$inning_data[data$inning_data$Inning %in% Inning,]
    }
    if(Inning==3){result<-data$inning_data[data$inning_data$Inning %in% c(1,2),]}
  }
  #Only Over Interval
  if(is.null(Bowler_name)==TRUE & stadium==FALSE & Inning=="n" & Over_interval!="n"){
    if(Over_interval %in% c("01-06","07-15","16-20")){
      result<-data$Over_interval[data$Over_interval$over_interval %in% Over_interval,]
    }
    if(Over_interval=="01-20"){
      result<-data$Over_interval
    }
  }

  #Bowler name and stadium
  if(is.null(Bowler_name)==FALSE & stadium==TRUE & Inning=="n" & Over_interval=="n"){
    cat("Select the stadium for analysis\n")
    temp0<-unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name])
    print(data.frame("No."=0:length(temp0),
                     "Venue"=c("ALL",temp0[order(temp0)])))
    b<-scan(what = integer())
    ifelse(b==0,venue<-temp0,
           venue<-temp0[order(temp0)][b])
    subdata<-data$Player_Data[data$Player_Data$Venue %in% venue & data$Player_Data$bowler==Bowler_name,]
    for(i in 1:length(unique(subdata$Venue))){
      if(is.na(unique(subdata$Venue)[i])){next}
      ubdata<-subset(subdata,subdata$Venue==unique(subdata$Venue)[i])
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(ubdata,player_out==data$Player_name & kind!="run out"))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      High_Score<-max(table[,2])
      (if(Out==0){Avg<-Runs}
        else{Avg<-round(Runs/Out,2)})
      BF<-nrow(ubdata)
      SR<-round((Runs/BF)*100,1)
      fours<-nrow(ubdata[ubdata$batsman_run==4,])
      six<-nrow(ubdata[ubdata$batsman_run==6,])
      Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                        nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
      boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                            nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
      result[i,c("Venue","City","Inn","OUT","Runs","Ball Faced","Avg","SR",
                     "fours","six","Dot_Percent","Boundary_Percent")]<-
        c(unique(ubdata$Venue),unique(ubdata$City)[1],Inn,Out,Runs,BF,Avg,SR,
          fours,six,Dot_per,boundary_per)
      result<-result[is.na(result$Venue)!=TRUE,]
    }
    result[,3:12]<-apply(result[ ,3:12], 2,
                             function(x) as.numeric(x))
    result<-result[order(result$Venue),]
    cat(data$Player_name,"Vs",Bowler_name,"\n")
  }
  
  # Bowler and inning
  if(is.null(Bowler_name)==FALSE & stadium==FALSE & Inning!="n" & Over_interval=="n"){
    subdata<-data$Player_Data[data$Player_Data$bowler==Bowler_name,]
    if(Inning %in% c(1,2)){
      subdata<-subdata[subdata$inning %in% Inning,]
    }
    if(Inning==3){subdata<-subdata[subdata$inning %in% c(1,2),]}
    if(nrow(subdata)==0){cat("Don't have any data for the",Bowler_name,"in the input inning against",data$Player_name,"\n")
      stop()}
    cat(data$Player_name,"Vs",Bowler_name,"\n")
    for(i in 1:length(unique(subdata$innings))){
      ubdata<-subset(subdata,subdata$innings==unique(subdata$innings)[i])
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(ubdata,player_out==data$Player_name & kind!="run out"))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      High_Score<-max(table[,2])
      if(Out==0){Avg<-Runs}
      else{Avg<-round(Runs/Out,2)}
      BF<-nrow(ubdata)
      SR<-round((Runs/BF)*100,1)
      fours<-nrow(ubdata[ubdata$batsman_run==4,])
      six<-nrow(ubdata[ubdata$batsman_run==6,])
      Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                        nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
      boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                            nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
      result[i,c("Inning","Inn","OUT","Runs","Ball Faced","Avg","SR",
                      "fours","six","Dot_Percent","Boundary_Percent")]<-
        c(unique(ubdata$innings),Inn,Out,Runs,BF,Avg,SR,
          fours,six,Dot_per,boundary_per)
      result<-result[is.na(result$Inning)!=TRUE,]
    }
    result[,2:11]<-apply(result[ ,2:11], 2,
                              function(x) as.numeric(x))
  }
  
  #Bowler and Over interval
  if(is.null(Bowler_name)==FALSE & stadium==FALSE & Inning=="n" & Over_interval!="n"){
    subdata<-data$Player_Data[data$Player_Data$bowler==Bowler_name,]
    if(Over_interval %in% c("01-06","07-15","16-20")){
      subdata<-subdata[subdata$over_interval %in% Over_interval,]
    }
    if(Over_interval=="01-20"){
      subdata<-subdata
    }
    if(nrow(subdata)==0){cat("Don't have any data for the",Bowler_name,"in the input overinterval against",data$Player_name,"\n")
      stop()}
    cat(data$Player_name,"Vs",Bowler_name,"\n")
    for(i in 1:length(unique(subdata$over_interval))){
      if(is.na(unique(subdata$over_interval)[i])){next}
      ubdata<-subset(subdata,subdata$over_interval==unique(subdata$over_interval)[i])
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(ubdata,player_out==data$Player_name & kind!="run out"))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      if(Out==0){Avg<-Runs}
      else{Avg<-round(Runs/Out,2)}
      BF<-nrow(ubdata)
      SR<-round((Runs/BF)*100,1)
      fours<-nrow(ubdata[ubdata$batsman_run==4,])
      six<-nrow(ubdata[ubdata$batsman_run==6,])
      Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                        nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
      boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                            nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
      result[i,c("Over_Interval","OUT","Runs","Ball Faced","Avg","SR",
                 "fours","six","Dot_Percent","Boundary_Percent")]<-c(unique(ubdata$over_interval),Out,Runs,BF,Avg,SR,
                                  fours,six,Dot_per,boundary_per)
    }
    result[,2:10]<-apply(result[ ,2:10], 2,
                         function(x) as.numeric(x))
    result<-result[order(result$Over_Interval),]
  }
  
  # Stadium and Inning
  if(is.null(Bowler_name)==TRUE & stadium==TRUE & Inning!="n" & Over_interval=="n"){
      #Stadium Name when bowler name is NULL
    if(Inning %in% c(1,2)){
      cat("Select the stadium for analysis\n")
      temp0<-unique(data$Player_Data$Venue[data$Player_Data$inning == Inning])
      print(data.frame("No."=0:length(temp0),
                       "Player"=c("ALL",temp0[order(temp0)])))
      b<-scan(what = integer())
      ifelse(b==0,venue<-temp0[order(temp0)],
             venue<-temp0[order(temp0)][b])
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue,]
      subdata<-subdata[subdata$inning %in% Inning,]
    }
    if(Inning==3){
      cat("Select the stadium for analysis\n")
      print(data.frame("No."=0:length(unique(data$Player_Data$Venue)),
                       "Player"=c("ALL",unique(data$Player_Data$Venue[order(data$Player_Data$Venue)]))))
      b<-scan(what = integer())
      ifelse(b==0,venue<-unique(data$Player_Data$Venue[order(data$Player_Data$Venue)]),
             venue<-unique(data$Player_Data$Venue[order(data$Player_Data$Venue)])[b])
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue,]
      subdata<-subdata[subdata$inning %in% c(1,2),]}
    ref_data<-aggregate(batsman_run~Venue+innings,subdata,sum)
    for(i in 1:nrow(ref_data)){
      ubdata<-subset(subdata,ref_data$Venue[i]==subdata$Venue & ref_data$innings[i]==subdata$innings)
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(all_ball_data,all_ball_data$player_out==data$Player_name & 
                         ref_data$Venue[i]==all_ball_data$Venue & ref_data$innings[i]==all_ball_data$innings))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      High_Score<-max(table[,2])
      ducks<-nrow(table[table$batsman_run==0,])
      (if(Out==0){Avg<-Runs}
        else{Avg<-round(Runs/Out,2)})
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
      MoM<-unique(ubdata[ubdata$Player_of_Match==data$Player_name,c("ID","Player_of_Match")])
      MoM<-nrow(na.omit(MoM))
      result[i,c("Venue","Inning","Played_inning","Out","Runs","Ball Faced","High_Score","Avg","SR",
                 "Fifty","Hundred","fours","six","Dot_Percent","Boundary_Percent","Man Of Match","Duck")]<-
        c(ref_data$Venue[i],ref_data$innings[i],Inn,Out,Runs,BF,High_Score,Avg,SR,
          fifty,hundred,fours,six,Dot_per,boundary_per,MoM,ducks)
    }
    result[,3:17]<-apply(result[ ,3:17], 2,
                         function(x) as.numeric(x))
    result<-result[order(result$Venue,result$Inning,decreasing = FALSE),]
    rownames(result)<-1:nrow(result)
  }
  
  #Stadium and Over_interval
  if(is.null(Bowler_name)==TRUE & stadium==TRUE & Inning=="n" & Over_interval!="n"){
    #Stadium Name when bowler name is NULL
    if(Over_interval %in% c("01-06","07-15","16-20")){
      cat("Select the stadium for analysis\n")
      temp0<-unique(data$Player_Data$Venue[data$Player_Data$over_interval == Over_interval])
      print(data.frame("No."=0:length(temp0),
                       "Player"=c("ALL",temp0[order(temp0)])))
      b<-scan(what = integer())
      ifelse(b==0,venue<-temp0[order(temp0)],
             venue<-temp0[order(temp0)][b])
      
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue,]
      subdata<-subdata[subdata$over_interval %in% Over_interval,]
    }
    if(Over_interval=="01-20"){
      cat("Select the stadium for analysis\n")
      print(data.frame("No."=0:length(unique(data$Player_Data$Venue)),
                       "Player"=c("ALL",unique(data$Player_Data$Venue[order(data$Player_Data$Venue)]))))
      b<-scan(what = integer())
      ifelse(b==0,venue<-unique(data$Player_Data$Venue[order(data$Player_Data$Venue)]),
             venue<-unique(data$Player_Data$Venue[order(data$Player_Data$Venue)])[b])
      
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue,]
    }
    ref_data<-aggregate(batsman_run~Venue+over_interval,subdata,sum)
    for(i in 1:nrow(ref_data)){
      ubdata<-subset(subdata,ref_data$Venue[i]==subdata$Venue & ref_data$over_interval[i]==subdata$over_interval)
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(all_ball_data,all_ball_data$player_out==data$Player_name &
                         ref_data$Venue[i]==all_ball_data$Venue & ref_data$over_interval[i]==all_ball_data$over_interval))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      High_Score<-max(table[,2])
      (if(Out==0){Avg<-Runs}
        else{Avg<-round(Runs/Out,2)})
      BF<-nrow(ubdata)
      SR<-round((Runs/BF)*100,1)
      fours<-nrow(ubdata[ubdata$batsman_run==4,])
      six<-nrow(ubdata[ubdata$batsman_run==6,])
      Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                        nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
      boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                            nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
      result[i,c("Venue","Over_Interval","Out","Runs","Ball Faced","High_Score","Avg","SR",
                 "fours","six","Dot_Percent","Boundary_Percent")]<-
        c(ref_data$Venue[i],ref_data$over_interval[i],Out,Runs,BF,High_Score,Avg,SR,
          fours,six,Dot_per,boundary_per)
    }
    result[,3:12]<-apply(result[ ,3:12], 2,
                         function(x) as.numeric(x))
    result<-result[order(result$Venue,result$Over_Interval,decreasing = FALSE),]
    rownames(result)<-1:nrow(result)
  }
  
  #Inning and Over_interval
  if(is.null(Bowler_name)==TRUE & stadium==FALSE & Inning!="n" & Over_interval!="n"){
    if(Inning %in% c(1,2)){
      subdata<-data$Player_Data[data$Player_Data$inning %in% Inning,]
    }
    if(Inning==3){subdata<-data$Player_Data[data$Player_Data$inning %in% c(1,2),]}
    if(Over_interval %in% c("01-06","07-15","16-20")){
      subdata<-subdata[subdata$over_interval %in% Over_interval,]
    }
    if(Over_interval=="01-20"){subdata<-subdata}
    ref_data<-aggregate(batsman_run~innings+over_interval,subdata,sum)
    for(i in 1:nrow(ref_data)){
      ubdata<-subset(subdata,ref_data$innings[i]==subdata$innings & ref_data$over_interval[i]==subdata$over_interval)
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(all_ball_data,all_ball_data$player_out==data$Player_name & 
                         ref_data$innings[i]==all_ball_data$innings & ref_data$over_interval[i]==all_ball_data$over_interval))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      High_Score<-max(table[,2])
      (if(Out==0){Avg<-Runs}
        else{Avg<-round(Runs/Out,2)})
      BF<-nrow(ubdata)
      SR<-round((Runs/BF)*100,1)
      fours<-nrow(ubdata[ubdata$batsman_run==4,])
      six<-nrow(ubdata[ubdata$batsman_run==6,])
      Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                        nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
      boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                            nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
      result[i,c("Inning","Over_Interval","Out","Runs","Ball Faced","High_Score","Avg","SR",
                 "fours","six","Dot_Percent","Boundary_Percent")]<-
        c(ref_data$innings[i],ref_data$over_interval[i],Out,Runs,BF,High_Score,Avg,SR,
          fours,six,Dot_per,boundary_per)
    }
    result[,3:12]<-apply(result[ ,3:12], 2,
                         function(x) as.numeric(x))
    result<-result[order(result$Inning,result$Over_Interval,decreasing = FALSE),]
    rownames(result)<-1:nrow(result)
  }
  
  #Bowler, Venue and Inning
  if(is.null(Bowler_name)==FALSE & stadium==TRUE & Inning!="n" & Over_interval=="n"){
    if(Inning %in% c(1,2)){
      cat("Select the stadium for analysis\n")
      print(data.frame("No."=0:length(unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name & data$Player_Data$innings==Inning])),
                       "Venue"=c("ALL",unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name & data$Player_Data$innings==Inning]))))
      b<-scan(what = integer())
      ifelse(b==0,venue<-unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name & data$Player_Data$innings==Inning]),
             venue<-unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name & data$Player_Data$innings==Inning])[b])
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue & data$Player_Data$bowler==Bowler_name & data$Player_Data$innings==Inning,]
    }
    if(Inning==3){
    cat("Select the stadium for analysis\n")
    print(data.frame("No."=0:length(unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name])),
                     "Venue"=c("ALL",unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name]))))
    b<-scan(what = integer())
    ifelse(b==0,venue<-unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name]),
           venue<-unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name])[b])
    subdata<-data$Player_Data[data$Player_Data$Venue %in% venue & data$Player_Data$bowler==Bowler_name,]
    subdata<-subdata[subdata$inning %in% c(1,2),]
    }
    ref_data<-aggregate(batsman_run~Venue+innings,subdata,sum)
    for(i in 1:nrow(ref_data)){
      ubdata<-subset(subdata,ref_data$Venue[i]==subdata$Venue & ref_data$innings[i]==subdata$innings)
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(ubdata,ubdata$player_out==data$Player_name & ubdata$kind!="run out"))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      (if(Out==0){Avg<-Runs}
        else{Avg<-round(Runs/Out,2)})
      BF<-nrow(ubdata)
      SR<-round((Runs/BF)*100,1)
      fours<-nrow(ubdata[ubdata$batsman_run==4,])
      six<-nrow(ubdata[ubdata$batsman_run==6,])
      Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                        nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
      boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                            nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
      result[i,c("Venue","Inning","Played_inning","Out","Runs","Ball Faced","Avg","SR",
                 "fours","six","Dot_Percent","Boundary_Percent")]<-
        c(ref_data$Venue[i],ref_data$innings[i],Inn,Out,Runs,BF,Avg,SR,
          fours,six,Dot_per,boundary_per)
    }
    result[,3:12]<-apply(result[ ,3:12], 2,
                         function(x) as.numeric(x))
    result<-result[order(result$Venue,result$Inning,decreasing = FALSE),]
    rownames(result)<-1:nrow(result)
    cat("\n",data$Player_name,"Vs",Bowler_name,"\n")
  }
  
  #Bowler, Stadium and Over interval
  if(is.null(Bowler_name)==FALSE & stadium==TRUE & Inning=="n" & Over_interval!="n"){
    if(Over_interval %in% c("01-06","07-15","16-20")){
      if(nrow(data$Player_Data[data$Player_Data$bowler==Bowler_name & data$Player_Data$over_interval==Over_interval,])<2){
        cat(Bowler_name,"doesn't bowl to",data$Player_name,"in over interval",Over_interval,"\n");stop()}
      
      cat("Select the stadium for analysis\n")
      print(data.frame("No."=0:length(unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name & data$Player_Data$over_interval==Over_interval])),
                       "Venue"=c("ALL",unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name & data$Player_Data$over_interval==Over_interval]))))
      b<-scan(what = integer())
      ifelse(b==0,venue<-unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name & data$Player_Data$over_interval==Over_interval]),
             venue<-unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name & data$Player_Data$over_interval==Over_interval])[b])
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue & data$Player_Data$bowler==Bowler_name & data$Player_Data$over_interval==Over_interval,]
    }
    if(Over_interval=="01-20"){
      cat("Select the stadium for analysis\n")
      print(data.frame("No."=0:length(unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name])),
                       "Venue"=c("ALL",unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name]))))
      b<-scan(what = integer())
      ifelse(b==0,venue<-unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name]),
             venue<-unique(data$Player_Data$Venue[data$Player_Data$bowler==Bowler_name])[b])
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue & data$Player_Data$bowler==Bowler_name,]
      subdata<-subdata[subdata$inning %in% c(1,2),]
    }
    ref_data<-aggregate(batsman_run~Venue+over_interval,subdata,sum)
    for(i in 1:nrow(ref_data)){
      ubdata<-subset(subdata,ref_data$Venue[i]==subdata$Venue & ref_data$over_interval[i]==subdata$over_interval)
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(ubdata,player_out==data$Player_name & kind!="run out"))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      (if(Out==0){Avg<-Runs}
        else{Avg<-round(Runs/Out,2)})
      BF<-nrow(ubdata)
      SR<-round((Runs/BF)*100,1)
      fours<-nrow(ubdata[ubdata$batsman_run==4,])
      six<-nrow(ubdata[ubdata$batsman_run==6,])
      Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                        nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
      boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                            nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
      result[i,c("Venue","Over_interval","Out","Runs","Ball Faced","Avg","SR",
                 "fours","six","Dot_Percent","Boundary_Percent")]<-
        c(ref_data$Venue[i],ref_data$over_interval[i],Out,Runs,BF,Avg,SR,
          fours,six,Dot_per,boundary_per)
    }
    result[,3:11]<-apply(result[ ,3:11], 2,
                         function(x) as.numeric(x))
    result<-result[order(result$Venue,result$Over_interval,decreasing = FALSE),]
    rownames(result)<-1:nrow(result)
    cat("\n",data$Player_name,"Vs",Bowler_name,"\n")
  }
  
  #Bowler, Inning and Over_interval
  if(is.null(Bowler_name)==FALSE & stadium==FALSE & Inning!="n" & Over_interval!="n"){
    if(Inning %in% c(1,2)){
      subdata<-data$Player_Data[data$Player_Data$inning %in% Inning & data$Player_Data$bowler==Bowler_name,]
    }
    if(Inning==3){subdata<-data$Player_Data[data$Player_Data$inning %in% c(1,2) & data$Player_Data$bowler==Bowler_name,]}
    if(Over_interval %in% c("01-06","07-15","16-20")){
      subdata<-subdata[subdata$over_interval %in% Over_interval,]
    }
    if(Over_interval=="01-20"){subdata<-subdata}
    if(nrow(subdata)==0){cat("\n",Bowler_name,"doesn't bowl any ball to",data$Player_name,"in",Over_interval,"over of given inning\n")
      stop()}
    cat("\n",data$Player_name,"Vs",Bowler_name,"\n")
    ref_data<-aggregate(batsman_run~innings+over_interval,subdata,sum)
    for(i in 1:nrow(ref_data)){
      ubdata<-subset(subdata,ref_data$innings[i]==subdata$innings & ref_data$over_interval[i]==subdata$over_interval)
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(ubdata,ubdata$player_out==data$Player_name & ubdata$kind!="run out"))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      High_Score<-max(table[,2])
      (if(Out==0){Avg<-Runs}
        else{Avg<-round(Runs/Out,2)})
      BF<-nrow(ubdata)
      SR<-round((Runs/BF)*100,1)
      fours<-nrow(ubdata[ubdata$batsman_run==4,])
      six<-nrow(ubdata[ubdata$batsman_run==6,])
      Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                        nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
      boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                            nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
      result[i,c("Inning","Over_Interval","Out","Runs","Ball Faced","High_Score","Avg","SR",
                 "fours","six","Dot_Percent","Boundary_Percent")]<-
        c(ref_data$innings[i],ref_data$over_interval[i],Out,Runs,BF,High_Score,Avg,SR,
          fours,six,Dot_per,boundary_per)
    }
    result[,3:12]<-apply(result[ ,3:12], 2,
                         function(x) as.numeric(x))
    result<-result[order(result$Inning,result$Over_Interval,decreasing = FALSE),]
    rownames(result)<-1:nrow(result)
  }
  
  #Stadium, Inning and Over interval
  if(is.null(Bowler_name)==TRUE & stadium==TRUE & Inning!="n" & Over_interval!="n"){
    if(Inning==3){demo_inn<-c(1,2)}
    if(Inning==1 | Inning==2){demo_inn<-Inning}
    if(Over_interval %in% c("01-06","07-15","16-20")){
      cat("Select the stadium for analysis\n")
      temp0<-unique(data$Player_Data$Venue[data$Player_Data$innings %in% demo_inn & data$Player_Data$over_interval==Over_interval])
      print(data.frame("No."=0:length(temp0),
                       "Venue"=c("ALL",temp0[order(temp0)])))
      b<-scan(what = integer())
      ifelse(b==0,venue<-temp0,
             venue<-temp0[order(temp0)][b])
      if(length(venue)<2){cat("For input inning and over_interval",data$Player_name,"Doesn't play any match\n")
        stop()}
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue & data$Player_Data$innings %in% demo_inn & data$Player_Data$over_interval==Over_interval,]
    }
    if(Over_interval=="01-20"){
      cat("Select the stadium for analysis\n")
      temp0<-unique(data$Player_Data$Venue[data$Player_Data$innings %in% demo_inn])
      print(data.frame("No."=0:length(temp0),
                       "Venue"=c("ALL",temp0[order(temp0)])))
      b<-scan(what = integer())
      ifelse(b==0,venue<-temp0,
             venue<-temp0[order(temp0)][b])
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue & data$Player_Data$innings %in% demo_inn,]
    }
    ref_data<-aggregate(batsman_run~Venue+innings+over_interval,subdata,sum)
    for(i in 1:nrow(ref_data)){
      ubdata<-subset(subdata,ref_data$Venue[i]==subdata$Venue & ref_data$innings[i]==subdata$innings & ref_data$over_interval[i]==subdata$over_interval)
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(all_ball_data,all_ball_data$player_out==data$Player_name & 
                         ref_data$Venue[i]==all_ball_data$Venue & ref_data$innings[i]==all_ball_data$innings & ref_data$over_interval[i]==all_ball_data$over_interval))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      (if(Out==0){Avg<-Runs}
        else{Avg<-round(Runs/Out,2)})
      BF<-nrow(ubdata)
      SR<-round((Runs/BF)*100,1)
      fours<-nrow(ubdata[ubdata$batsman_run==4,])
      six<-nrow(ubdata[ubdata$batsman_run==6,])
      Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                        nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
      boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                            nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
      result[i,c("Venue","Inning","Over_interval","Out","Runs","Ball Faced","Avg","SR",
                 "fours","six","Dot_Percent","Boundary_Percent")]<-
        c(ref_data$Venue[i],ref_data$innings[i],ref_data$over_interval[i],Out,Runs,BF,Avg,SR,
          fours,six,Dot_per,boundary_per)
    }
    result[,4:11]<-apply(result[ ,4:11], 2,
                        function(x) as.numeric(x))
    result<-result[order(result$Venue,result$Inning,result$Over_interval,decreasing = FALSE),]
    rownames(result)<-1:nrow(result)
  }
  
  #Bowler, Venue, Inning, and Over_interval
  if(is.null(Bowler_name)==FALSE & stadium==TRUE & Inning!="n" & Over_interval!="n"){
    if(Inning==3){demo_inn<-c(1,2)}
    if(Inning==1 | Inning==2){demo_inn<-Inning}
    if(Over_interval %in% c("01-06","07-15","16-20")){
      cat("Select the stadium for analysis\n")
      print(data.frame("No."=0:length(unique(data$Player_Data$Venue[data$Player_Data$innings %in% demo_inn & data$Player_Data$over_interval==Over_interval & data$Player_Data$bowler==Bowler_name])),
                       "Venue"=c("ALL",unique(data$Player_Data$Venue[data$Player_Data$innings %in% demo_inn & data$Player_Data$over_interval==Over_interval & data$Player_Data$bowler==Bowler_name]))))
      b<-scan(what = integer())
      ifelse(b==0,venue<-unique(data$Player_Data$Venue[data$Player_Data$innings %in% demo_inn & data$Player_Data$over_interval==Over_interval & data$Player_Data$bowler==Bowler_name]),
             venue<-unique(data$Player_Data$Venue[data$Player_Data$innings %in% demo_inn & data$Player_Data$over_interval==Over_interval & data$Player_Data$bowler==Bowler_name])[b])
      if(length(venue)<1){cat("For input inning and over_interval",data$Player_name,"Doesn't play any match\n")
        stop()}
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue & data$Player_Data$innings %in% demo_inn & data$Player_Data$over_interval==Over_interval & data$Player_Data$bowler==Bowler_name,]
    }
    if(Over_interval=="01-20"){
      cat("Select the stadium for analysis\n")
      print(data.frame("No."=0:length(unique(data$Player_Data$Venue[data$Player_Data$innings %in% demo_inn & data$Player_Data$bowler==Bowler_name])),
                       "Venue"=c("ALL",unique(data$Player_Data$Venue[data$Player_Data$innings %in% demo_inn & data$Player_Data$bowler==Bowler_name]))))
      b<-scan(what = integer())
      ifelse(b==0,venue<-unique(data$Player_Data$Venue[data$Player_Data$innings %in% demo_inn & data$Player_Data$bowler==Bowler_name]),
             venue<-unique(data$Player_Data$Venue[data$Player_Data$innings %in% demo_inn & data$Player_Data$bowler==Bowler_name])[b])
      if(length(venue)<1){cat("For input inning and over_interval",data$Player_name,"Doesn't play any match\n")
        stop()}
      subdata<-data$Player_Data[data$Player_Data$Venue %in% venue & data$Player_Data$innings %in% demo_inn & data$Player_Data$bowler==Bowler_name,]
    }
    ref_data<-aggregate(batsman_run~Venue+innings+over_interval,subdata,sum)
    for(i in 1:nrow(ref_data)){
      ubdata<-subset(subdata,ref_data$Venue[i]==subdata$Venue & ref_data$innings[i]==subdata$innings & ref_data$over_interval[i]==subdata$over_interval)
      Inn<-length(unique(ubdata$ID))
      Out<-nrow(subset(ubdata,ubdata$player_out==data$Player_name & ubdata$kind!="run out"))
      NO<-Inn-Out
      Runs<-sum(ubdata$batsman_run,na.rm = TRUE)
      table<-aggregate(batsman_run~ID,ubdata,
                       function(x) sum(x,na.rm = TRUE))
      (if(Out==0){Avg<-Runs}
        else{Avg<-round(Runs/Out,2)})
      BF<-nrow(ubdata)
      SR<-round((Runs/BF)*100,1)
      fours<-nrow(ubdata[ubdata$batsman_run==4,])
      six<-nrow(ubdata[ubdata$batsman_run==6,])
      Dot_per<-round((nrow(subset(ubdata,extra_type %notin% "wides" & batsman_run==0))/
                        nrow(subset(ubdata,extra_type %notin% "wides")))*100,2)
      boundary_per<-round(nrow(subset(ubdata,batsman_run %in% c(4,6) & extra_type %notin% "wides"))/
                            nrow(subset(ubdata,extra_type %notin% "wides"))*100,2) 
      result[i,c("Venue","Inning","Over_interval","Out","Runs","Ball Faced","Avg","SR",
                 "fours","six","Dot_Percent","Boundary_Percent")]<-
        c(ref_data$Venue[i],ref_data$innings[i],ref_data$over_interval[i],Out,Runs,BF,Avg,SR,
          fours,six,Dot_per,boundary_per)
    }
    result[,4:11]<-apply(result[ ,4:11], 2,
                        function(x) as.numeric(x))
    result<-result[order(result$Venue,result$Inning,result$Over_interval,decreasing = FALSE),]
    rownames(result)<-1:nrow(result)
    cat("\n",data$Player_name,"Vs",Bowler_name,"\n")
  }
  if(is.null(Bowler_name)==TRUE & stadium==FALSE & Inning=="n" & Over_interval=="n"){
   cat("\nAccording to your input we don't have anything to show\n")
    stop()
  }
  return(result)
}

https://chat.openai.com/share/8ec4f26b-ffee-4a03-a7cb-1d6b7812c5ab