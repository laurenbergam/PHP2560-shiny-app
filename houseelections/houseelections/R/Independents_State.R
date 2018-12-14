#' Independents sent by State
#' 
#' This function counts the number of Independents a given state sent to Congress
#' in each election year from 1998 to 2016.
#' @param state Name of the state that you want the information for. Must be capitalized appropriately.
#' @export
#' @examples
#' Independents_State("Vermont")


Independents_State <- function(state){
  years <- as.character(seq(from = 1998, to = 2016, by = 2))
  inds <- c()
  for(i in years){
    temp_df <- Election_Data %>% filter(State == state & Winning_Party == "Independent" & Election_Year == i)
    inds <- c(inds, nrow(temp_df))
  }
  names(inds) = years
  return(inds)
}