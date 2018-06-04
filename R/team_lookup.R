#' Team Lookup Function
#' #'
#' This function returns the team numbers based on school name. These numbers are needed for other functions. Wraps baseballr function.
#' @param teams A vector of teams that you would like returned
#' @param year The year of the season you want. Generally, team numbers stay consistent through multiple years.
#' @keywords ncaa, baseball, college
#' @export
#' @examples team_lookup(c('Florida','Coastal Caro.','Virginia'),2018)
#' box_stats()
team_lookup <- function(teams,year){
  final_table <- data.frame('Team'=c(),'Number'=c())
  for(i in teams){
    temp <- try(as.data.frame(school_id_lu(i)))
    temp <- temp[which(temp$year == year & temp$school == i),]
    if(nrow(temp)==0){
      stop(paste0('The team "',i,'" did not return results. It must match perfectly to NCAA website spelling.'))
    }
    temp <- data.frame('Team'=c(i),'Number'=c(temp$school_id))
    final_table <- rbind(final_table,temp)
  }
  return(final_table)
}



