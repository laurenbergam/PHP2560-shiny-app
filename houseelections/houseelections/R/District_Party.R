#' District broken down by party
#'
#' Given a state and a district in that state, this function will return the number
#' of Republicans, Democrats, Independents that district sent to Congress. It also
#' gives the average margin of victory for the elections in that district.
#' @param state The name of the state with the district you are interested in.
#' It must also be properly capitalized.
#' @param district_no The number of the district in the state. This is inputted as
#' a numeric (no parantheses needed).
#' @export
#' @examples
#' District_Results("Pennsylvania", 10)
#' District_Results("Wyoming")

District_Party <- function(state, district_no = 1){
  data <- District_Results(state, district_no)
  dems <- 0
  reps <- 0
  inds <- 0
  for(i in 1:nrow(data)){
    if(data$Winning_Party[i] == "Democrat"){
      dems <- dems + 1
    }else if(data$Winning_Party[i] == "Republican"){
      reps <- reps + 1
    }else if(data$Winning_Party[i] == "Independent"){
      inds <- inds + 1
    }
  }
  vec <- c(dems, reps, inds, mean(data$Margin, na.rm = TRUE))
  names(vec) <- c("Democrats", "Republicans", "Independents", "Average Margin")
  return(vec)
}
