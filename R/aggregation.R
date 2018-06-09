#' by_player function
#'
#' This function takes a dataframe from advanced_stats or box_stats and aggregates.
#' @param dframe the dataframe returned in advanced_stats or box_stats
#' @param type Options are sum, mean, max, min
#' @keywords ncaa, baseball, college
#' @export
#' @examples by_player(df,'sum')
#' by_player()
by_player <- function(dframe,type='sum'){
  if(type=='sum'){
    final <- dframe %>% group_by(Team,Player) %>% summarise_if(is.numeric,function(x){sum(x,na.rm=T)})
  } else if (type=='mean'){
    final <-dframe %>% group_by(Team,Player) %>% summarise_if(is.numeric,function(x){mean(x,na.rm=T)})
  } else if (type == 'max'){
    final <-dframe %>% group_by(Team,Player) %>% summarise_if(is.numeric,function(x){max(x,na.rm=T)})
  } else if (type == 'min'){
    final <-dframe %>% group_by(Team,Player) %>% summarise_if(is.numeric,function(x){min(x,na.rm=T)})
  } else {
    stop('type needs to be sum, mean, min, or max.')
  }
  return(as.data.frame(final))
}


#' by_team function
#'
#' This function takes a dataframe from advanced_stats or box_stats and aggregates.
#' @param dframe the dataframe returned in advanced_stats or box_stats
#' @param type Options are sum, mean, max, min
#' @keywords ncaa, baseball, college
#' @export
#' @examples by_team(df,'sum')
#' by_team()
by_team <- function(dframe,type='sum'){
  if(type=='sum'){
    final <- dframe %>% group_by(Team) %>% summarise_if(is.numeric,function(x){sum(x,na.rm=T)})
  } else if (type=='mean'){
    final <-dframe %>% group_by(Team) %>% summarise_if(is.numeric,function(x){mean(x,na.rm=T)})
  } else if (type == 'max'){
    final <-dframe %>% group_by(Team) %>% summarise_if(is.numeric,function(x){max(x,na.rm=T)})
  } else if (type == 'min'){
    final <-dframe %>% group_by(Team) %>% summarise_if(is.numeric,function(x){min(x,na.rm=T)})
  } else {
    stop('type needs to be sum, mean, min, or max.')
  }
  return(as.data.frame(final))
}

