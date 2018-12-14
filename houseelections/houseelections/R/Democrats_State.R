#' Democrats sent by State
#' 
#' This function counts the number of Democrats a given state sent to Congress
#' in each election year from 1998 to 2016.
#' @param state Name of the state that you want the information for. Must be capitalized appropriately.
#' @export
#' @examples
#' Democrats_State("Arizona")

Democrats_State <- function(state){
  years <- as.character(seq(from = 1998, to = 2016, by = 2))
  dems <- c()
  for(i in years){
    temp_df <- Election_Data %>% filter(State == state & Winning_Party == "Democrat" & Election_Year == i)
    dems <- c(dems, nrow(temp_df))
  }
  names(dems) = years
  return(dems)
}
