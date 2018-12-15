#' A function for state-specific, time-specific per capita Gross Domestic Product
#' 
#' This function allows for you to see if per capita GDP increased for a given state over a given time period.
#'
#' @param state
#' @param year1
#' @param year2
#' @export
#' @examples  
#' gdp_state("Virginia", "1998", "2000")

gdp_status <- function(state, year1, year2){
  temp_df <- PerCapitaGDP_by_State_1997_2017_2 %>%
    filter(GeoName == state) %>%
    select(year1, year2)
  a <- temp_df[2] - temp_df[1]
  if(year1 < year2) {
    if(a > 0) {
      print("Increase")
    } else {
      print("Decrease")
    } 
  }
  else {
    print("make sure you pick 2 consecutive years")
  }
}