#' ELO Function
#'
#' This function returns a data.frame of elo for each game for every team from start date to end date. Earliest available
#' data is February of 2014.
#' @param start_date The date you want to start from the start_score.
#' @param end_date When you would like to end
#' @param start_score What score you would like for teams to begin at
#' @param k_num K value you would like to use for results.
#' @keywords ncaa, baseball, college, elo
#' @export
#' @examples elo('02/10/2014','09/22/2018')
#' box_stats()
elo <- function(start_date,end_date,start_score=1000,k_num=10){
all_games = data.frame()

for(idate in as.character(seq(as.Date(start_date,'%m/%d/%Y'), by = "day", length.out = as.numeric(as.Date(end_date,'%m/%d/%Y')
                                                  - as.Date(start_date,'%m/%d/%Y'))))){


start_split <- unlist(strsplit(idate,'-'))
if(start_split[2] %in% c('02','03','04','05','06')){
if(start_split[3]=='01'){print(idate)}
html <- paste0('https://www.ncaa.com/scoreboard/baseball/d1/',start_split[1],
              '/',start_split[2],'/',start_split[3],'/all-conf')
html <- read_html(html)
info <- html %>%
  html_nodes("#scoreboardGames span")
info <- info[!unlist(lapply(info,function(x) grepl('round',x,fixed=T)))]
todays_scores <- c()
if(length(info)>0){
  for(j in 1:length(info)){
    cleaning <- unlist(strsplit(as.character(info[j]),'>'))[2]
    cleaning <- substr(cleaning,1,nchar(cleaning)-6)
    todays_scores <- c(todays_scores,cleaning)
  }
  todays_scores <- as.data.frame(matrix(todays_scores,ncol=3,byrow = T))
  names(todays_scores) <- c('Rank','Team','Score')
  todays_scores$Loc <- rep(c('Away','Home'),nrow(todays_scores)/2)
  todays_scores$Date <- as.Date(idate,'%Y-%m-%d')
  todays_scores$Score <- as.numeric(as.character(todays_scores$Score))
  todays_scores$Result <- NA
  for(i in 1:(nrow(todays_scores)/2)){
    if(todays_scores[(2*i-1),]$Score>todays_scores[(2*i),]$Score){
      todays_scores[(2*i-1),]$Result = 'W'
      todays_scores[(2*i),]$Result = 'L'
    } else if(todays_scores[(2*i-1),]$Score<todays_scores[(2*i),]$Score){
      todays_scores[(2*i-1),]$Result = 'L'
      todays_scores[(2*i),]$Result = 'W'
    } else {
      todays_scores[(2*i-1),]$Result = 'T'
      todays_scores[(2*i),]$Result = 'T'
    }
    todays_scores$Opponent = NA
    todays_scores[seq(1,nrow(todays_scores),2),]$Opponent = as.character(todays_scores[seq(2,nrow(todays_scores),2),]$Team)
    todays_scores[seq(2,nrow(todays_scores),2),]$Opponent = as.character(todays_scores[seq(1,nrow(todays_scores),2),]$Team)
  }
  if(nrow(all_games)==0){
    all_games=todays_scores
  } else {
    all_games=rbind(all_games,todays_scores)
  }


}
}#end outer for'
}
save.image("~/Data Projects/ncaa baseball/2018/elo.RData")
all_games$gameID = NA
all_games[seq(1,nrow(all_games),2),]$gameID = seq(1,nrow(all_games)/2,1)
all_games[seq(2,nrow(all_games),2),]$gameID = seq(1,nrow(all_games)/2,1)
all_games$pregameELO <- -1
all_games$pregameELO <- as.integer(all_games$pregameELO)
all_games[match(unique(all_games$Team), all_games$Team),]$pregameELO <- start_score

elo.calc = function(elo1,elo2,result,k=k_num){
  r1 = 10^(elo1/400)
  r2 = 10^(elo2/400)
  e1 = r1/(r1+r2)
  e2 = r2/(r1+r2)
  if(result=='W'){s1=1} else if (result=='L'){s1=0} else {s1=.5}
  if(result=='L'){s2=1} else if (result=='W'){s2=0} else {s2=.5}
  elochange = as.integer(k*(s1-e1))
  new_elo1 = as.integer(elo1 + elochange)
  new_elo2 = as.integer(elo2-elochange)

  new_elo = c(new_elo1,new_elo2)
  return(new_elo)
}
all_games$postgameELO <- -1

all_games$postgameELO <- as.integer(all_games$postgameELO)
pb <- txtProgressBar(min = 0, max = max(all_games$gameID), style = 3)
for(i in 1:max(all_games$gameID)){
  temp = all_games[which(all_games$gameID==i),]
  temp2 = elo.calc(temp[1,]$pregameELO,temp[2,]$pregameELO,temp[1,]$Result)
  all_games[which(all_games$gameID==i),]$postgameELO = temp2

  if(nrow(all_games[which(all_games$Team==temp[1,]$Team & all_games$gameID>i),])>0){
    all_games[which(all_games$Team==temp[1,]$Team & all_games$gameID>i),]$pregameELO[1] = temp2[1]
  }
  if(nrow(all_games[which(all_games$Team==temp[2,]$Team & all_games$gameID>i),])>0){
    all_games[which(all_games$Team==temp[2,]$Team & all_games$gameID>i),]$pregameELO[1] = temp2[2]
  }
  setTxtProgressBar(pb, i)
}
close(pb)
all_games$ELOchange = all_games$postgameELO-all_games$pregameELO
plot(all_games[which(all_games$Team == 'Oregon St.'),]$Date,all_games[which(all_games$Team == 'Oregon St.'),]$postgameELO,type='l')

all_games$Favorite = NA

pb <- txtProgressBar(min = 0, max = max(all_games$gameID), style = 3)
for( i in 1:max(all_games$gameID)){
  temp = all_games[which(all_games$gameID==i),]
  temp = temp$pregameELO[1]-temp$pregameELO[2]
  temp =c(temp,0-temp)
  all_games[which(all_games$gameID==i),]$Favorite = temp
  setTxtProgressBar(pb, i)
}
close(pb)
all_games$margin = as.numeric(NA)
for(i in 1:(nrow(all_games)/2)){
  all_games[(2*i-1),]$margin = all_games[(2*i-1),]$Score-all_games[(2*i),]$Score
  all_games[(2*i),]$margin = all_games[(2*i),]$Score-all_games[(2*i-1),]$Score
}
return(all_games)
}
