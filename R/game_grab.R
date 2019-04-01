#' Game Grab Function
#'
#' This function returns a data.frame of game statistics for every game played on a particular day. If the game is in the future it will return matchups if available. It retunrs a list of three dataframes with hitting, pitching, and fielding stats.
#' @param date The date that you want to grab in date format
#' @keywords ncaa, baseball, college
#' @export
#' @examples game_grab_by_date(Sys.Date())
#' game_grab_by_date()
game_grab_by_date = function(date=Sys.Date()-1){

  date = as.character(date)
  year = substr(date,1,4)
  month =substr(date,6,7)
  day = substr(date,9,10)
  codes <- ncaaYearCodes(year)
  webpage = paste0("https://stats.ncaa.org/contests/scoreboards?utf8=%E2%9C%93&game_sport_year_ctl_id=",ncaaYearCodes(year)$YearId[1],"&conference_id=0&conference_id=0&division=1&game_date=",month,"%2F",day,"%2F",year,"&commit=Submit")
  webpage = read_html(webpage)

  if(as.Date(date,'%Y-%m-%d')<=Sys.Date()){
    game_codes = str_match_all(webpage, "(?<=contests/)(.*)(?=/box_score)")[[1]][,2]
    if(length(game_codes)>0){
  neutral = webpage %>% html_nodes(".totalcol+td")
  neutral = gsub(' ','',neutral)
  neutral =ifelse(neutral == '<tdrowspan="2">\n</td>',0,1)

  bscores = webpage %>% html_nodes("table") %>% html_table(fill=TRUE)
  bscores = bscores[[1]]
  bscores = bscores[seq(5,nrow(bscores),5),1]
  neutral = neutral[which(bscores=='Box Score')]
  webpage = as.character(webpage)


  print(paste0('Downloading ',length(game_codes),' games from ', date,'...'))
  pb <- txtProgressBar(min = 0, max = length(game_codes), style = 3)
  for(i in 1:length(game_codes)){
    tryCatch(html_code = (read_html(paste0("https://stats.ncaa.org/contests/",game_codes[i],"/box_score"))),
             error=function(e) html_code <<- (read_html(paste0("https://stats.ncaa.org/contests/",game_codes[i],"/box_score"))))
    #html_code = read_html(paste0("https://stats.ncaa.org/contests/",game_codes[i],"/box_score"))
    Sys.sleep(0.1)

    tables = html_code %>% html_nodes("table")%>%
      html_table(fill=TRUE)
    inningscores = tables[[1]]
    box_score = matrix(c('Team',paste0('In',seq(1,18,1)),'R','H','E',rep(NA,44)),nrow=3,byrow=T)
    box_score[2,1:(ncol(inningscores)-3)] = unlist(inningscores[2,1:(ncol(inningscores)-3)])
    box_score[3,1:(ncol(inningscores)-3)] = unlist(inningscores[3,1:(ncol(inningscores)-3)])
    box_score[2,20:22] = unlist(inningscores[2,(ncol(inningscores)-2):ncol(inningscores)])
    box_score[3,20:22] = unlist(inningscores[3,(ncol(inningscores)-2):ncol(inningscores)])
    box_score = as.data.frame(box_score,stringsAsFactors=F); names(box_score)=box_score[1,]; box_score=box_score[2:3,]
    box_score$Weather = substr(tables[[2]][1][1,],9,nchar(tables[[2]][1][1,]))
    box_score$Date = tables[[3]][2][1,]
    box_score$Date = as.Date(box_score$Date,tryFormat='%m/%d/%Y')
    box_score$Location = tables[[3]][2][2,]
    box_score$Attendance = tables[[3]][2][3,]
    box_score[,2:22] =  lapply(box_score[,2:22], function(x) as.numeric(as.character(gsub("/", "", x))))
    box_score$ScoreDiff = c(box_score[1,]$R - box_score[2,]$R,box_score[2,]$R - box_score[1,]$R)
    box_score$Result = as.character(ifelse(box_score$ScoreDiff>0,'W','L'))
    box_score$Result = as.character(ifelse(box_score$ScoreDiff==0,'T',as.character(box_score$Result)))
    box_score$HomeAway = c('Away','Home')
      if(neutral[i]==1){
        box_score$HomeAway = 'Neutral'
    }

    box_score$HomeAway = ifelse(neutral[i]==1,'Neutral',box_score$HomeAway)
    box_score$Opponent = c(box_score$Team[2],box_score$Team[1])
    box_score$Gamecode = game_codes[i]

    #removing NA columns
    info1 <- tables[[6]]
    info2 <- tables[[7]]
    team1 = info1[1,2]
    team2 = info2[1,2]
    info = rbind(info1,info2)
    names(info) = info[2,]
    info <- info[3:nrow(info),]
    info[info==""] = "0"
    info[] <- lapply(info, gsub, pattern="'", replacement="")
    info[] <- lapply(info, gsub, pattern="\t", replacement="")
    info[] <- lapply(info, gsub, pattern="\t", replacement="")
    info[,3:ncol(info)] <- lapply(info[,3:ncol(info)], function(x) as.numeric(as.character(gsub("/", "", x))))
    info$Slugging <- round(info$TB/info$AB,3)

    info$OBP = round((info$H + info$BB + info$HBP)/
                       (info$AB + info$BB + info$HBP + info$SF),3)
    info$OPS = round(info$Slugging + info$OBP,3)

    info$Slugging <- ifelse(info$AB == 0, NA,info$Slugging)
    info$OBP <- ifelse(info$AB == 0, NA,info$OBP)
    info$OPS <- ifelse(info$AB == 0, NA,info$OPS)

    info$Team = NA
    info$Opponent = NA
    info$Team[1:(nrow(info1)-3)] = team1
    info$Team[(nrow(info1)-2):nrow(info)] = team2
    info$Opponent[1:(nrow(info1)-3)] = team2
    info$Opponent[(nrow(info1)-2):nrow(info)] = team1
    info <- info[c(1:(nrow(info1)-3), (nrow(info1)+1):(nrow(info)-1)),]
    info$Starter = NA
    htmltext <- paste(html_code)
    for(ii in 1:nrow(info)){
      temp <- info$Player[ii]
      location <- gregexpr(temp,htmltext)
      temp <- substr(htmltext,location[[1]][1]-1,
                     location[[1]][1]-1)
      starter <- ifelse(temp %in% c('\n','>'),1,0)
      temp <- unlist(strsplit(info$Player[ii],','))
      temp <- paste(trimws(temp[2]),temp[1])
      info$Player[ii]=temp
      info$Starter[ii] = starter
    }#close 10 rows up
      info$game_number = i
      date_temp = as.character(str_match_all(html_code, "(?s)Game Date:</td>\n      <td>(.*?)</td>\n   </tr>")[[1]][,2])
      date_temp = unlist(strsplit(date_temp,'/'))
      info$date = as.Date(paste0(str_sub(date_temp[1],-2,-1),'/',date_temp[2],'/',str_sub(date_temp[3],0,4)),format='%m/%d/%Y')
      info$Gamecode = game_codes[i]
      info$pitchers_game_score = 40 +
        floor(info$IP)*6 +
        (info$IP - floor(info$IP))*20 +
        info$SO -
        info$H*2 -
        info$BB*2 -
        info$R*3 -
        info$`HR-A`*6
      info$pitchers_game_score = ifelse(info$Pos=='P',info$pitchers_game_score,NA)
   setTxtProgressBar(pb, i)
    if(i==1){complete_table=info} else {complete_table=rbind(complete_table,info)}
   if(i==1){complete_box_scores=box_score} else {complete_box_scores=rbind(complete_box_scores,box_score)}

}
  # game_temp <- data.frame('gameID'=c(game_codes))
  # game_temp$YearId = codes$YearId
  # game_codes <- merge(game_temp,codes,by='YearId',all.x=T)
  # return(game_grab(game_codes, as.numeric(year),type, situational))
  complete_table$G = 1
    # hitting_stats = complete_table[,c('Player','Pos','G',7,8,10,13,15,16,17,19,21,23,26,30,32,34,35,41,52,68,69,70,71,72,73,74,75,76,77)]
    # pitching_stats = complete_table[,c(1,2,3,4,5,6,9,11,12,14,18,20,22,24,28,29,31,33,36,38,40,42,43,44,45,46,47,48,49,51,53,54,55,56,78,72,73,74,75,76,77)]
    # fielding_stats = complete_table[,c(1,2,3,25,27,37,39,50,57,58,59,60,62,63,72,73,74,75,76,77)]
    names(complete_table) = gsub('.1','',names(complete_table))
    names(complete_table) = gsub('-','.',names(complete_table))
    names(complete_table) = gsub(' ','.',names(complete_table))
    names(complete_table) = gsub('2B','X2B',names(complete_table))
    names(complete_table) = gsub('3B','X3B',names(complete_table))
    names(complete_table)[which(names(complete_table)=='BB')[1]] = 'BB.P'
    hitting_stats = complete_table[,c('Player','Pos','G','R','AB','H','X2B','PO','X3B','TB','HR','RBI','BB','HBP','SF','SH','K','DP','SB',
                                      'RBI2out','LOB','Slugging','OBP','OPS','Team','Opponent','Starter','game_number','date','Gamecode')]
    names(complete_table)[which(names(complete_table)=='H')[1]] = 'H.H'
    names(complete_table)[which(names(complete_table)=='R')[1]] = 'R.H'
    pitching_stats = complete_table[,c('Player','Pos','G','App','GS','IP','CG','H','R','ER','BB.P','SO','SHO','BF','P.OAB','X2B.A','X3B.A','Bk','HR.A','WP','HB','IBB','Inh.Run','Inh.Run.Score',
                                       'SHA','SFA','Pitches','GO','FO','W','L','SV','OrdAppeared','KL','pitchers_game_score',
                                       'Team','Opponent','Starter','game_number','date','Gamecode')]
    if(year <= 2016){complete_table$TC = NA}
    fielding_stats = complete_table[,c('Player','Pos','G','TC','A','CS','Picked','E','CI','PB','SBA',
                                       'CSB','IDP','TP','Team','Opponent','Starter','game_number','date','Gamecode')]
    names(pitching_stats)[which(names(pitching_stats)=='BB.P')[1]] = 'BB'
    wrapup = list(hitting_stats,pitching_stats,fielding_stats,complete_box_scores)

  return(wrapup)

  } else {
    print(paste0('No games available for download on ', date,'...'))
    wrapup = NA
    return(wrapup)
  }
  } else {
    teams = str_match_all(webpage, '(?<=TEAMS_WIN\">)(.*)(?=<)')[[1]][,1]
    teams = gsub('&amp;',"&",teams)
    future_games = data.frame('AwayTeam'=c(teams[seq(1,length(teams),2)]),
                              'HomeTeam'=c(teams[seq(2,length(teams),2)]))
    return(future_games)

  }
}


game_grab = function(game_codes,year=as.numeric(substr(Sys.Date(),1,4)), type='hitting',situational=F,bothteams=T){
  codes <- ncaaYearCodes(year)
  # Gamecode loop to get each game
  if(situational==T){
    for(i in 1:nrow(game_codes)){
      print(paste0('Grabbing game ',i,' of ',nrow(game_codes),'...'))
      if(type=='hitting'){
        #HITTING TABLE
        html_code <- paste0("http://stats.ncaa.org/game/situational_stats/",as.character(game_codes$gameID[i]),
                            '?year_stat_category_id=',game_codes$Hitting[i])
        columns_to_keep=14
      } else if (type=='pitching'){
        html_code <- paste0("http://stats.ncaa.org/game/situational_stats/",as.character(game_codes$gameID[i]),
                            '?year_stat_category_id=',game_codes$Pitching[i])
        columns_to_keep=11
      } else if (type=='fielding'){
        html_code <- paste0("http://stats.ncaa.org/game/situational_stats/",as.character(game_codes$gameID[i]),
                            '?year_stat_category_id=',game_codes$Fielding[i])
        columns_to_keep=2
      }
      try(html_code <- read_html(html_code))
      if(typeof(html_code)=='list'){
        #grabbing all play by play
        info <- html_code %>%
          html_nodes("table+ .mytable") %>%
          html_table(fill=TRUE)

        #removing NA columns
        info <- info[[1]][,1:columns_to_keep]
        #setting names
        names(info) = info[2,]
        info <- info[3:nrow(info),]
        info[info==""] = "0-0"
        info$Starter = NA
        htmltext <- paste(html_code)
        for(ii in 1:nrow(info)){
          temp <- info$Player[ii]
          location <- gregexpr(temp,htmltext)
          temp <- substr(htmltext,location[[1]][1]-1,
                         location[[1]][1]-1)
          starter <- ifelse(temp %in% c('\n','>'),1,0)
          info$Starter[ii] = starter
        }#close 10 rows up
        hit_table <- data.frame('Player'=info$Player,stringsAsFactors = F)
        for(col in 2:(ncol(info)-1)){
          out <- strsplit(as.character(info[,col]),'-')
          out <- data.frame(do.call(rbind, out))
          names(out) <- c(paste0( names(info[col]),'_converted'),paste0(names(info[col]),'_attempted'))
          out[,1] <- as.numeric(as.character(out[,1]));out[,2] <- as.numeric(as.character(out[,2]));
          hit_table <- cbind(hit_table,out)
        }#close 5 up
        hit_table$Team <- NA
        hit_table$Opponent <- NA

        if(length(which(grepl(" Total", hit_table$Player)))==2){
          team1 <- hit_table[which(grepl(" Total", hit_table$Player))[1],]$Player
          team1 <- gsub(" Totals","",team1)
          team2 <- hit_table[which(grepl(" Total", hit_table$Player))[2],]$Player
          team2 <- gsub(" Totals","",team2)
          hit_table[1:which(grepl(" Total", hit_table$Player))[1],]$Team <- team1
          hit_table[which(grepl(" Total", hit_table$Player))[1]:nrow(hit_table),]$Team <- team2
          hit_table[1:which(grepl(" Total", hit_table$Player))[1],]$Opponent <- team2
          hit_table[which(grepl(" Total", hit_table$Player))[1]:nrow(hit_table),]$Opponent <- team1
          hit_table <- hit_table[-which(grepl(" Totals", hit_table$Player)),]


          for(ii in 1:nrow(hit_table)){
            temp <- unlist(strsplit(hit_table$Player[ii],','))
            temp <- temp[1:length(temp)-1]
            temp <- paste(trimws(temp[2]),temp[1])
            hit_table$Player[ii]=temp
          }#close 5 up

          hit_table$TeamId=team_num
          hit_table$game_number = i
          hit_table$Date <- as.Date(as.character(str_match_all(html_code, "(?s)Game Date:</td>\n      <td>(.*?)</td>\n   </tr>")[[1]][,2]),"%m/%d/%Y")
          hit_table$GameCode = game_codes$gameID[i]

          if(i==1){
            complete_table <- hit_table
          } else {
            complete_table <- rbind(complete_table,hit_table)
          }


        }#close if not 2 teams
      }#close if html not list
    }

    if(bothteams==FALSE){
      complete_table = complete_table[which(complete_table$Team==as.character(names(sort(table(complete_table$Team),decreasing=TRUE))[1])),]
    }
    return(complete_table)

} else if (situational==F){
  for(i in 1:nrow(game_codes)){
    print(paste0('Grabbing game ',i, ' of ',nrow(game_codes),'...'))
    if(type=='hitting'){
      html_code <- paste0("http://stats.ncaa.org/game/box_score/",as.character(game_codes$gameID[i]),
                          '?year_stat_category_id=',game_codes$Hitting[i])
    } else if(type=='pitching') {
      html_code <- paste0("http://stats.ncaa.org/game/box_score/",as.character(game_codes$gameID[i]),
                          '?year_stat_category_id=',game_codes$Pitching[i])
    } else if(type=='fielding'){
      html_code <- paste0("http://stats.ncaa.org/game/box_score/",as.character(game_codes$gameID[i]),
                          '?year_stat_category_id=',game_codes$Fielding[i])
    }
    tryCatch(html_code <<- read_html(html_code),
             error=function(e) html_code <<- read_html(html_code))
    if(typeof(html_code)=='list'){
      #grabbing all play by play

      info1 <- html_code %>%
        html_nodes("table+ .mytable") %>%
        html_table(fill=TRUE)
      info2 <- html_code %>%
        html_nodes("br+ .mytable") %>%
        html_table(fill=TRUE)

      #removing NA columns
      info1 <- info1[[1]]
      info2 <- info2[[2]]
      team1 = info1[1,2]
      team2 = info2[1,2]
      info = rbind(info1,info2)
      names(info) = info[2,]
      info <- info[3:nrow(info),]
      info[info==""] = "0"
      info[] <- lapply(info, gsub, pattern="'", replacement="")
      info[] <- lapply(info, gsub, pattern="\t", replacement="")
      info[] <- lapply(info, gsub, pattern="\t", replacement="")

      if(type=='hitting'){
        info[,3:ncol(info)] <- lapply(info[,3:ncol(info)], function(x) as.numeric(as.character(gsub("/", "", x))))
        info$Slugging <- round(info$TB/info$AB,3)

        info$OBP = round((info$H + info$BB + info$HBP)/
                           (info$AB + info$BB + info$HBP + info$SF),3)
        info$OPS = round(info$Slugging + info$OBP,3)

        info$Slugging <- ifelse(info$AB == 0, NA,info$Slugging)
        info$OBP <- ifelse(info$AB == 0, NA,info$OBP)
        info$OPS <- ifelse(info$AB == 0, NA,info$OPS)
      }
      info$Team = NA
      info$Opponent = NA
      info$Team[1:(nrow(info1)-3)] = team1
      info$Team[(nrow(info1)-2):nrow(info)] = team2
      info$Opponent[1:(nrow(info1)-3)] = team2
      info$Opponent[(nrow(info1)-2):nrow(info)] = team1
      info <- info[c(1:(nrow(info1)-3), (nrow(info1)+1):(nrow(info)-1)),]
      info$Starter = NA
      htmltext <- paste(html_code)
      for(ii in 1:nrow(info)){
        temp <- info$Player[ii]
        location <- gregexpr(temp,htmltext)
        temp <- substr(htmltext,location[[1]][1]-1,
                       location[[1]][1]-1)
        starter <- ifelse(temp %in% c('\n','>'),1,0)
        temp <- unlist(strsplit(info$Player[ii],','))
        temp <- paste(trimws(temp[2]),temp[1])
        info$Player[ii]=temp
        info$Starter[ii] = starter
      }#close 10 rows up
      info$game_number = i
      date_temp = as.character(str_match_all(html_code, "(?s)Game Date:</td>\n      <td>(.*?)</td>\n   </tr>")[[1]][,2])
      date_temp = unlist(strsplit(date_temp,'/'))
      info$date = as.Date(paste0(str_sub(date_temp[1],-2,-1),'/',date_temp[2],'/',str_sub(date_temp[3],0,4)),format='%m/%d/%Y')
      info$Gamecode = game_codes$gameID[i]


      if(type=='pitching'){
        info <- info[which(info$Pos == 'P'),]
      }
      if(i==1){
        complete_table <- info
      } else {
        complete_table <- rbind(complete_table,info)
      }

    }#if less than 2 teams close
  }

  if(bothteams==F){
    complete_table = complete_table[which(complete_table$Team == names(sort(table(complete_table$Team),decreasing=TRUE)[1])),]
  }
  if(type=='pitching'){
    complete_table[,3:34] <- lapply(complete_table[,3:34], function(x) as.numeric(gsub("/", "", x)))

    complete_table$pitchers_game_score = 40 +
      floor(complete_table$IP)*6 +
      (complete_table$IP - floor(complete_table$IP))*20 +
      complete_table$SO -
      complete_table$H*2 -
      complete_table$BB*2 -
      complete_table$R*3 -
      complete_table$`HR-A`*6
  }
  return(complete_table)
}
}
