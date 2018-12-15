#' A function for state-specific per capita Gross Domestic Product data
#' 
#' This function allows you to get state-specific GDP data.
#' @param state Name of the state you want the GDP information for. Must be capitalized.
#' @export
#' @examples 
#' gdp_state("Virginia")

gdp_state <- function(state) {
  years <- as.character(seq(from = 1998, to = 2016, by = 2))
  stateGDP <- c()
  for (i in years) {
    temp_df <- PerCapitaGDP_by_State_1997_2017_2 %>% filter(GeoName == state) %>% select(i)
    stateGDP <- c(stateGDP, temp_df)
  }
  names(stateGDP) = years
  return(unlist(stateGDP))
}