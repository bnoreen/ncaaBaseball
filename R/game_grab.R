#' Game Grab Function
#'
#' This function returns a data.frame of game statistics for every game played on a particular day.
#' @param date The date that you want to grab in date format
#' @param type hitting, pitching, or fielding
#' @param situational A true/false if you want situational or box stats
#' @keywords ncaa, baseball, college
#' @export
#' @examples game_grab_by_date(Sys.Date(),'hitting',TRUE)
#' game_grab_by_date()
game_grab_by_date = function(date=Sys.Date(), type='hitting',situational=F){
  date = as.character(date)
  year = substr(date,1,4)
  month =substr(date,6,7)
  day = substr(date,9,10)
  codes <- ncaaYearCodes(year)
  webpage = paste0("https://stats.ncaa.org/contests/scoreboards?utf8=%E2%9C%93&game_sport_year_ctl_id=",ncaaYearCodes(year)$YearId[1],"&conference_id=0&conference_id=0&division=1&game_date=",month,"%2F",day,"%2F",year,"&commit=Submit")
  webpage = read_html(webpage)
  webpage = as.character(webpage)
  game_codes = str_match_all(webpage, "(?<=contests/)(.*)(?=/box_score)")[[1]][,2]
  for(i in 1:length(game_codes)){
    webpage = as.character(read_html(paste0("https://stats.ncaa.org/contests/",game_codes[i],"/box_score")))
    game_codes[i] = str_match_all(webpage, "(?<=box_score/)(.*)(?=\\?year_stat)")[[1]][,2][1]
  }
  
  game_temp <- data.frame('gameID'=c(game_codes))
  game_temp$YearId = codes$YearId
  game_codes <- merge(game_temp,codes,by='YearId',all.x=T)
  return(game_grab(game_codes, type, situational))
}


game_grab = function(game_codes,type='hitting',situational=F,bothteams=T){
  # Gamecode loop to get each game
  if(situational==T){
    for(i in 1:nrow(game_codes)){
      
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
    
    try(html_code <- read_html(html_code))
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
        info[,3:34] <- lapply(info[,3:ncol(info)], function(x) as.numeric(as.character(gsub("/", "", x))))
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
      complete_table$HR.A*6
  }
  return(complete_table)
}
}