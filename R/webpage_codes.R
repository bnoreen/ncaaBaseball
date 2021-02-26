#' ncaaYearCodes function
#'
#' This function takes a dataframe from advanced_stats or box_stats and aggregates.
#' @param year The year to grab codes for
#' @keywords ncaa, baseball, college
#' ncaaYearCodes()
ncaaYearCodes <- function(year) {
  webpage_codes <- data.frame(
<<<<<<< HEAD
    'Year'=c(2019,2018,2017,2016,2015,2014,2013,2020,2021),
    'YearId'=c(14781,12973,12560,12360,12080,11620,11320,15204,509101),
    'Hitting'=c(14643,11953,11000,10946,10780,10460,10120,14760,14840),
    'Pitching'=c(14644,11954,11001,10947,10781,10461,10121,14761,14841),
    'Fielding'=c(14645,11955,11002,10948,10782,10462,10122,14762,14842)
=======
    'Year'=c(2019,2018,2017,2016,2015,2014,2013,2020),
    'YearId'=c(14781,12973,12560,12360,12080,11620,11320,15204),
    'Hitting'=c(14643,11953,11000,10946,10780,10460,10120,14760),
    'Pitching'=c(14644,11954,11001,10947,10781,10461,10121,14761),
    'Fielding'=c(14645,11955,11002,10948,10782,10462,10122,14762)
>>>>>>> origin/master
  )
  if(year %in% webpage_codes$Year){
    return(webpage_codes[which(year==webpage_codes$Year),])
  } else {
    stop('Only available for years 2013 through 2021')
  }

}

#' game_codes function
#'
#' This function takes a dataframe from advanced_stats or box_stats and aggregates.
#' @param team_num The team number to grab codes for
#' @param year The year to grab codes for
#' @keywords ncaa, baseball, college
#' game_codes()
game_codes <- function(team_num,Year){
  game_codes <- data.frame('gameID'=c(),'YearId'=c())
  codes <- ncaaYearCodes(Year)
  year <- read_html(paste0('http://stats.ncaa.org/team/',team_num,'/',codes$YearId))
  year <- as.character(year)
  game_codes_single_year <- str_match_all(year, "(?s)/game/index/(.*?)\\?")[[1]][,2]
  game_temp <- data.frame('gameID'=c(game_codes_single_year))
  game_temp$YearId = codes$YearId
  game_codes <- rbind(game_codes,game_temp)
  game_codes <- merge(game_codes,codes,by='YearId',all.x=T)
  return(game_codes)
}


