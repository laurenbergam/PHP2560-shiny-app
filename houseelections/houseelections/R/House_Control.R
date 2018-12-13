#' Who controlled the House of Representatives in a given year?
#' 
#' This function counts the number of Republicans and Democrats (and Independents) sent to Congress
#' in a given election year. It tells you which party controlled the House
#' after the election, and how many more seats that party had over the minority party.
#' @param year The year that you want the information from. Must be an even year between 1998 and 2016.
#' @export
#' @examples
#' House_Control(2004)

House_Control <- function(year){
  temp_df1 <- Election_Data %>% filter(Election_Year == year & Winning_Party == "Democrat")
  temp_df2 <- Election_Data %>% filter(Election_Year == year & Winning_Party == "Republican")
  temp_df3 <- Election_Data %>% filter(Election_Year == year & Winning_Party == "Independent")
  a <- nrow(temp_df1)
  b <- nrow(temp_df2)
  data <- list(a,b,nrow(temp_df3))
  if(a > b){
    data <- c(data, "Democratic")
  }else{
    data <- c(data, "Republican")
  }
  
  margin = a - b
  data <- c(data, abs(margin))
  names(data) <- c("Democrats", "Republicans", "Independents", "Control", "Margin")
  return(data)
}