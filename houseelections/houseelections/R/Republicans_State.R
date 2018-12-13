#' Republicans sent by State
#' 
#' This function counts the number of Republicans a given state sent to Congress
#' in each election year from 1998 to 2016.
#' @param state Name of the state that you want the information for. Must be capitalized appropriately.
#' @export
#' @examples
#' Republicans_State("New York")


Republicans_State <- function(state){
  years <- as.character(seq(from = 1998, to = 2016, by = 2))
  repubs <- c()
  for(i in years){
    temp_df <- Election_Data %>% filter(State == state & Winning_Party == "Republican" & Election_Year == i)
    repubs <- c(repubs, nrow(temp_df))
  }
  names(repubs) = years
  return(repubs)
}