#' RPI Function
#'
#' This function returns box stats dataframe with RPI and SOS column added. If RPI exists to some point it will continue.
#' It also includes a dataframe of the most recent rpi and sos rankings
#' @param box_stats box stat dataframe
#' #' @keywords ncaa, baseball, college, rpi
#' @export
#' @examples rpi(box_stats_dataframe)
#' rpi()
rpi = function(game_stats_frame){

  if(inherits(game_stats_frame$Date,'Date')){
    data(team_list)
    team_list1=team_list[which(team_list$year == substr(game_stats_frame$Date,1,4)[1] & team_list$division==1),]
    game_stats_frame = game_stats_frame[which(game_stats_frame$Team %in% team_list1$school & game_stats_frame$Opponent %in% team_list1$school),]
    #add rpi and sos if they do not exist
    if(!('RPI' %in% colnames(game_stats_frame))){
      game_stats_frame$RPI = NA
    }
    if(!('SOS' %in% colnames(game_stats_frame))){
      game_stats_frame$SOS = NA
    }
    if(!('RPIrank' %in% colnames(game_stats_frame))){
      game_stats_frame$RPIrank = NA
    }
    if(!('SOSrank' %in% colnames(game_stats_frame))){
      game_stats_frame$SOSrank = NA
    }

    #getting the dates we need...if none then we start a week in, if some then find out what dates are missing
    if(all(is.na(game_stats_frame$RPI))){
      dates = unique(game_stats_frame$Date)[7:length(unique(game_stats_frame$Date))]
    } else {
      dates = max(game_stats_frame[!is.na(game_stats_frame$RPI),]$Date)
      dates = unique(game_stats_frame[which(game_stats_frame$Date>dates),]$Date)

      comebacktothis = setDT(game_stats_frame)[,.SD[which.max(Date)],keyby=Team]

    }
    pb <- txtProgressBar(min = 0, max = length(dates), style = 3)
    for(jj in 1:length(dates)){
      df = game_stats_frame[which(game_stats_frame$Date<=dates[jj]),
                            c('Team','Result','Gamecode','Date','HomeAway','Opponent')]
      rpi_stats = data.frame('Team'=c(),'RoadWin'=c(),'RoadLoss'=c(),'HomeWin'=c(),'HomeLoss'=c(),'NeutralWin'=c(),'NeutralLoss'=c(),'Ties'=c())
      for(i in unique(df$Team)){
        temp = data.frame('Team'=c(i))
        temp$RoadWin = nrow(df[which(df$Team == i & df$Result == 'W' & df$HomeAway == 'Away'),])
        temp$RoadLoss = nrow(df[which(df$Team == i & df$Result == 'L' & df$HomeAway == 'Away'),])
        temp$HomeWin = nrow(df[which(df$Team == i & df$Result == 'W' & df$HomeAway == 'Home'),])
        temp$HomeLoss = nrow(df[which(df$Team == i & df$Result == 'L' & df$HomeAway == 'Home'),])
        temp$NeutralWin = nrow(df[which(df$Team == i & df$Result == 'W' & df$HomeAway == 'Neutral'),])
        temp$NeutralLoss = nrow(df[which(df$Team == i & df$Result == 'L' & df$HomeAway == 'Neutral'),])
        temp$Ties = nrow(df[which(df$Team == i & df$Result == 'T'),])
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
        rpi_stats[i,]$opp_win = (sum(temp$HomeWin) + sum(temp$RoadWin) + sum(temp$NeutralWin))
        rpi_stats[i,]$opp_loss = (sum(temp$HomeLoss) + sum(temp$RoadLoss) + sum(temp$NeutralLoss))
        rpi_stats[i,]$opp_tie = (sum(temp$Ties))
        rpi_stats[i,]$opp_win_perc = rpi_stats[i,]$opp_win/(rpi_stats[i,]$opp_win + rpi_stats[i,]$opp_loss + rpi_stats[i,]$opp_tie)
      }
      rpi_stats$opp_opp_win_perc = NA
      for(i in 1:nrow(rpi_stats)){
        temp = df[which(df$Team == rpi_stats[i,]$Team),]$Opponent
        temp = rpi_stats[which(rpi_stats$Team %in% temp),]
        rpi_stats[i,]$opp_opp_win_perc = sum(temp$opp_win)/(sum(temp$opp_win)+sum(temp$opp_loss)+sum(temp$opp_tie))
      }
      rpi_stats$RPI = .25*rpi_stats$weighted_win_perc+.5*rpi_stats$opp_win_perc +.25*rpi_stats$opp_opp_win_perc
      rpi_stats$SOS = .6666*rpi_stats$opp_win_perc +.3333*rpi_stats$opp_opp_win_perc
      if(!exists('rpi_rank_table')){
        rpi_rank_table = data.frame('Team'=team_list1$school)
        rpi_rank_table = merge(rpi_rank_table,rpi_stats[,c('Team','RPI','SOS')],by.x='Team',
                               by.y='Team',all.x=T)
        rpi_rank_table$RPIrank = rank(-rpi_rank_table$RPI,'min',na.last=T)
        rpi_rank_table$SOSrank = rank(-rpi_rank_table$SOS,'min',na.last=T)
      } else {
        rpi_rank_table_temp = data.frame('Team'=team_list1$school)
        rpi_rank_table_temp = merge(rpi_rank_table_temp,rpi_stats[,c('Team','RPI','SOS')],
                                    by.x='Team',by.y='Team',all.x=T)
        for(i in 1:nrow(rpi_rank_table)){
          if(!is.na(rpi_rank_table_temp[i,]$RPI)){
            rpi_rank_table[i,]$RPI = rpi_rank_table_temp[i,]$RPI
          }
          if(!is.na(rpi_rank_table_temp[i,]$SOS)){
            rpi_rank_table[i,]$SOS= rpi_rank_table_temp[i,]$SOS
          }
          rpi_rank_table$RPIrank = rank(-rpi_rank_table$RPI,'min',na.last=T)
          rpi_rank_table$SOSrank = rank(-rpi_rank_table$SOS,'min',na.last=T)
        }
      }

      for(i in game_stats_frame[which(game_stats_frame$Date == dates[jj]),]$Team){
        game_stats_frame[which(game_stats_frame$Date == dates[jj] & game_stats_frame$Team == i),]$RPI = rpi_rank_table[which(rpi_rank_table$Team==i),]$RPI
        game_stats_frame[which(game_stats_frame$Date == dates[jj] & game_stats_frame$Team == i),]$RPIrank = rpi_rank_table[which(rpi_rank_table$Team==i),]$RPIrank
        game_stats_frame[which(game_stats_frame$Date == dates[jj] & game_stats_frame$Team == i),]$SOS = rpi_rank_table[which(rpi_rank_table$Team==i),]$SOS
        game_stats_frame[which(game_stats_frame$Date == dates[jj] & game_stats_frame$Team == i),]$SOSrank = rpi_rank_table[which(rpi_rank_table$Team==i),]$SOSrank
      }

      setTxtProgressBar(pb, jj)
    }
    rpi_rank_table$Date = max(dates)
    close(pb)
    return(list(game_stats_frame,rpi_rank_table))

  } else {
    return("Date column must be of type 'Date'")
  }
}

