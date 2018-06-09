#' Advanced Stats Function
#'
#' This function returns a data.frame of advanced stats with each player and game for the entire season
#' @param team_num The number of the team. You can use the baseballr package to look up team_num.
#' @param year The year of the season you want.
#' @param type hitting, pitching, or fielding depending on what stats you want.
#' @param bothteams By default, bothteams is true and stats for your selected team and each opponent will be included. To only get the team you selected set to false.
#' @keywords ncaa, baseball, college
#' @export
#' @examples advanced_stats(457,2018,'pitching')
#' advanced_stats()
advanced_stats <- function(team_num,year,type,game_count=NULL,bothteams = TRUE){
#type can be hitting, pitching, or fielding

  codes <- ncaaYearCodes(year)

  game_codes <- game_codes(team_num,year)
  if(!is.null(game_count)){
    game_codes <- game_codes[(nrow(game_codes)-game_count+1):nrow(game_codes),]
  }

  pb <- txtProgressBar(min = 0, max = nrow(game_codes), style = 3)

  # Gamecode loop to get each game
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

      setTxtProgressBar(pb, i)
        }#close if not 2 teams
      }#close if html not list
      }
  close(pb)
  if(bothteams==FALSE){
    complete_table = complete_table[which(complete_table$Team==as.character(names(sort(table(complete_table$Team),decreasing=TRUE))[1])),]
  }
  return(complete_table)

}

#' Box Stats Function
#'
#' This function returns a data.frame of box stats with each player and game for the entire season
#' @param team_num The number of the team. You can use the baseballr package to look up team_num.
#' @param year The year of the season you want.
#' @param type hitting, pitching, or fielding depending on what stats you want.
#' @param bothteams By default, bothteams is true and stats for your selected team and each opponent will be included. To only get the team you selected set to false.
#' @keywords ncaa, baseball, college
#' @export
#' @examples box_stats(457,2018,'hitting')
#' box_stats()
box_stats <- function(team_num,year,type,game_count=NULL,bothteams = TRUE){
  codes <- ncaaYearCodes(year)

  game_codes <- game_codes(team_num,year)
  if(!is.null(game_count)){
    game_codes <- game_codes[(nrow(game_codes)-game_count+1):nrow(game_codes),]
  }
  pb <- txtProgressBar(min = 0, max = nrow(game_codes), style = 3)

  # Gamecode loop to get each game
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
    suppressWarnings(info[,3:ncol(info)] <- lapply(info[,3:ncol(info)], function(x) as.numeric(as.character(x))))
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
    info$Date <- as.Date(as.character(str_match_all(html_code, "(?s)Game Date:</td>\n      <td>(.*?)</td>\n   </tr>")[[1]][,2]),"%m/%d/%Y")
    info$Gamecode = game_codes$gameID[i]


    if(type=='pitching'){
      info <- info[which(info$Pos == 'P'),]
    }
    if(i==1){
      complete_table <- info
    } else {
      complete_table <- rbind(complete_table,info)
    }
    setTxtProgressBar(pb, i)
  }#if less than 2 teams close
  }
  close(pb)
  return(complete_table)
}

#' Game Stats Function
#'
#' This function returns a data.frame of game statistics for a team in a given season
#' @param team_num The number of the team. You can use the baseballr package to look up team_num.
#' @param year The year of the season you want.
#' @param game_count The number of games back from latest point in pull. For example, 5 would give you last 5 games of the season. If null, the entire season is grabbed.
#' @param bothteams By default, bothteams is true and stats for your selected team and each opponent will be included. To only get the team you selected set to false.
#' @keywords ncaa, baseball, college
#' @export
#' @examples game_stats(457,2018)
#' game_stats()
game_stats <- function(team_num,year,game_count=NULL,bothteams = TRUE){
  codes <- ncaaYearCodes(year)

  game_codes <- game_codes(team_num,year)
  if(!is.null(game_count)){
    game_codes <- game_codes[(nrow(game_codes)-game_count+1):nrow(game_codes),]
  }
  pb <- txtProgressBar(min = 0, max = nrow(game_codes), style = 3)
  innings = c('In1','In2','In3','In4','In5','In6','In7','In8','In9',
              'In10','In11','In12','In13','In14','In15','In16','In17','In18')
  # Gamecode loop to get each game
  for(i in 1:nrow(game_codes)){

      html_code <- paste0("http://stats.ncaa.org/game/box_score/",as.character(game_codes$gameID[i]))
      try(html_code <- read_html(html_code))
      if(typeof(html_code)=='list'){
        info <- html_code %>%
          html_nodes("br+ .mytable") %>%
          html_table(fill=TRUE)
        info <- info[[1]]
        names(info)[1]='Team'
        names(info)[2:(ncol(info)-3)] = innings[1:(ncol(info)-4)]
        info = info[2:3,]
        game_table <- data.frame('Team'=info$Team)
        for (j in 1:18){
          game_table[[paste0("Inning",j)]]<-NA
        }
        game_table$R = info[,ncol(info)-2]
        game_table$H = info[,ncol(info)-1]
        game_table$E = info[,ncol(info)]
        game_table[,2:(ncol(info)-3)] <- info[,2:(ncol(info)-3)]

        info2 <- html_code %>%
          html_nodes(".mytable+ table") %>%
          html_table(fill=TRUE)
        info2 <- unlist(info2)
        game_table$Weather =  strsplit(as.character(info2[1]),':')[[1]][2]
        game_table$Location = str_match_all(html_code, "(?s)Location:</td>\n      <td>(.*?)</td>\n")[[1]][,2]
        attendance = str_match_all(html_code, "(?s)Attendance:</td>\n      <td>(.*?)</td>\n   </tr>")[[1]][,2]
        if(identical(attendance, character(0)) ){
          game_table$Attendance = NA
        } else {
          game_table$Attendance = gsub(',','',attendance)
        }
        umps <- str_match_all(html_code, "(?s)Officials:(.*?)</td>\n   </tr>")[[1]][,2]
        game_table$home_umpire = trimws(substr(umps,gregexpr(pattern ='\n',umps)[[1]][3]+1,
                                         gregexpr(pattern ='\n',umps)[[1]][4]-1))
        game_table$first_umpire = trimws(substr(umps,gregexpr(pattern ='\n',umps)[[1]][6]+1,
                                          gregexpr(pattern ='\n',umps)[[1]][7]-1))
        game_table$third_umpire = trimws(substr(umps,gregexpr(pattern ='\n',umps)[[1]][9]+1,
                                          gregexpr(pattern ='\n',umps)[[1]][10]-1))
        game_table$GameCode = game_codes$gameID[i]
        game_table$Date <- as.Date(as.character(str_match_all(html_code, "(?s)Game Date:</td>\n      <td>(.*?)</td>\n   </tr>")[[1]][,2]),"%m/%d/%Y")

        if(i==1){
          complete_table <- game_table
        } else {
          complete_table <- rbind(complete_table,game_table)
        }
        setTxtProgressBar(pb, i)
      }
  }
  close(pb)
  if(bothteams==FALSE){
    complete_table = complete_table[which(complete_table$Team==as.character(names(sort(table(complete_table$Team),decreasing=TRUE))[1])),]
  }

  return(complete_table)
}
