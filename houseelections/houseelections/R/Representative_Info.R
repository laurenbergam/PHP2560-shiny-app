#' Information on a particular representative
#'
#' Given the name of someone who served in the US House of Representatives from
#' 1998-2016, this function will return the years they served in the House, their
#' party, state and average percentage of the vote they got in the elections between
#' 1998-2016.
#' @param state The name of the representative, properly capitalized.#
#' @export
#' @examples
#' Reprsentative_Info("Bernie Sanders")

Representative_Info <- function(name){
  m <- filter(Election_Data, Winner == name)
  n <- list()
  for(i in 1:nrow(m)){
    if(m$Incumbent[i] == name){
      elected <- as.numeric(m$First_year_incumbent_elected[i])
      break
    }else{
      elected <- min(as.numeric(m$Election_Year))
    }
  }
  if(m$Election_Year[1] != 2016){
    left_office <- max(as.numeric(m$Election_Year) + 2) + 1
  }else{
    left_office <- " "
  }
  n[[1]] <- name
  n[[2]] <- paste(elected + 1, left_office, sep = "-")
  n[[3]] <- c(unique(m$Winning_Party))
  n[[4]] <- c(unique(m$State))
  n[[5]] <- round(mean(m$Winning_Percentage, na.rm = TRUE), digits = 2)
  names(n) <- c("Representative", "Served", "Party", "State", "Average Winning Percentage")
  return(n)
}


