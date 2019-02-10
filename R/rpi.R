# team_list <- read.csv('C:/Users/bhnoreen/Documents/Data Projects/ncaa baseball/team_list.csv')
# team_list = team_list[which(team_list$division==1 & team_list$year==2018),]
# 
# for(i in 91:nrow(team_list)){
#   if(i==1){
#     complete_list = game_stats(team_list[i,]$school_id,2018)
#   } else {
#     complete_list = rbind(complete_list,game_stats(team_list[i,]$school_id,2018))
#     print(paste0(i,' /297'))
#   }
#   
# }
# complete_list$Opponent = as.character(NA)
# backup_complete_list = complete_list
# 
# complete_list = complete_list[!duplicated(complete_list[,c('Team','GameCode')]),]
# 
# complete_list$Team = as.character(complete_list$Team)
# for (i in unique(complete_list$GameCode)){
#   temp=complete_list[which(complete_list$GameCode==i),]$Team
#   complete_list[which(complete_list$GameCode==i),]$Opponent = rev(temp)
# }
# complete_list = complete_list[which(complete_list$Team %in% team_list$school & complete_list$Opponent %in% team_list$school),]
# 
# for(i in unique(complete_list$GameCode)){
#   temp = complete_list[which(complete_list$GameCode == i),]$R
#   temp = as.numeric(as.character(temp))
#   if(length(temp)==2){
#     if(temp[1]>temp[2]){
#       complete_list[which(complete_list$GameCode == i),]$Result = c('Win','Loss')
#     } else if (temp[1]<temp[2]){
#       complete_list[which(complete_list$GameCode == i),]$Result = c('Loss','Win')
#     } else {
#       complete_list[which(complete_list$GameCode == i),]$Result = c('Tie','Tie')
#     }
#   }
# }
#enter dataframe of box_stats
rpi = function(game_stats_frame){
  for(i in unique(game_stats_frame$Team)){
    temp = as.data.frame(table(game_stats_frame[which(game_stats_frame$Team == i),]$Location))
    temp = temp[which(temp$Freq<15),]$Var1
    if(length(game_stats_frame[which(game_stats_frame$Team == i & game_stats_frame$HomeAway == 'Home' & game_stats_frame$Location %in% temp),]$HomeAway)>0){
      game_stats_frame[which(game_stats_frame$Team == i & game_stats_frame$HomeAway == 'Home' & game_stats_frame$Location %in% temp),]$HomeAway = 'Neutral'
    }
  }
  temp=game_stats_frame[which(game_stats_frame$HomeAway=='Neutral'),]$GameCode
  game_stats_frame[which(game_stats_frame$GameCode %in% temp),]$HomeAway = 'Neutral'
 View(game_stats_frame[which(game_stats_frame$Team == 'North Carolina'),])
  
df = game_stats_frame[,c('Team','Result','GameCode','Date','HomeAway','Opponent')]
rpi_stats = data.frame('Team'=c(),'RoadWin'=c(),'RoadLoss'=c(),'HomeWin'=c(),'HomeLoss'=c(),'NeutralWin'=c(),'NeutralLoss'=c(),'Ties'=c())
for(i in unique(df$Team)){
  temp = data.frame('Team'=c(i))
  temp$RoadWin = nrow(df[which(df$Team == i & df$Result == 'Win' & df$HomeAway == 'Away'),])
  temp$RoadLoss = nrow(df[which(df$Team == i & df$Result == 'Loss' & df$HomeAway == 'Away'),])
  temp$HomeWin = nrow(df[which(df$Team == i & df$Result == 'Win' & df$HomeAway == 'Home'),])
  temp$HomeLoss = nrow(df[which(df$Team == i & df$Result == 'Loss' & df$HomeAway == 'Home'),])
  temp$NeutralWin = nrow(df[which(df$Team == i & df$Result == 'Win' & df$HomeAway == 'Neutral'),])
  temp$NeutralLoss = nrow(df[which(df$Team == i & df$Result == 'Loss' & df$HomeAway == 'Neutral'),])
  temp$Ties = nrow(df[which(df$Team == i & df$Result == 'Tie'),])
  rpi_stats = rbind(rpi_stats,temp)
}
rpi_stats$win_perc = (rpi_stats$HomeWin + rpi_stats$RoadWin+rpi_stats$NeutralWin)/
  (rpi_stats[,2] + rpi_stats[,3] +rpi_stats[,4] +rpi_stats[,5]+rpi_stats[,6]+rpi_stats[,7]+rpi_stats[,8])
rpi_stats$weighted_win_perc = (rpi_stats$HomeWin*.7 + rpi_stats$RoadWin*1.3 + rpi_stats$NeutralWin)/
  (rpi_stats[,2] + rpi_stats[,3] +rpi_stats[,4] +rpi_stats[,5]+rpi_stats[,6]+rpi_stats[,7]+rpi_stats[,8])

rpi_stats$opp_win_perc = NA
rpi_stats$opp_win = NA
rpi_stats$opp_loss = NA
rpi_stats$opp_tie = NA
for(i in 1:nrow(rpi_stats)){
temp = df[which(df$Team == rpi_stats[i,]$Team),]$Opponent
temp = rpi_stats[which(rpi_stats$Team %in% temp),]
rpi_stats[i,]$opp_win_perc = (sum(temp$HomeWin) + sum(temp$RoadWin))/
  (sum(temp[,2])+sum(temp[,3])+sum(temp[,4])+sum(temp[,5]) + sum(temp[,6]))
rpi_stats[i,]$opp_win = (sum(temp$HomeWin) + sum(temp$RoadWin) + sum(temp$NeutralWin))
rpi_stats[i,]$opp_loss = (sum(temp$HomeLoss) + sum(temp$RoadLoss) + sum(temp$NeutralLoss))
rpi_stats[i,]$opp_tie = (sum(temp$Ties))
}
rpi_stats$opp_opp_win_perc = NA
for(i in 1:nrow(rpi_stats)){
  temp = df[which(df$Team == rpi_stats[i,]$Team),]$Opponent
  temp = rpi_stats[which(rpi_stats$Team %in% temp),]
  rpi_stats[i,]$opp_opp_win_perc = sum(temp$opp_win)/(sum(temp$opp_win)+sum(temp$opp_loss)+sum(temp$opp_tie))
}
rpi_stats$RPI = .25*rpi_stats$weighted_win_perc+.5*rpi_stats$opp_win_perc +.25*rpi_stats$opp_opp_win_perc
rpi_stats$RPI_rank = rank(-rpi_stats$RPI,'min')

return(rpi_stats)
}
