#' Results from a district
#' 
#' Given a state and a district in that state, this function will return all of the
#' election results from that district. If the state only has one district, leave the 
#' second argument district_no blank.
#' @param state The name of the state with the district you are interested in.
#' It must also be properly capitalized.
#' @param district_no The number of the district in the state. This is inputted as
#' a numeric (no parantheses needed).
#' @export
#' @examples
#' District_Results("New Jersey", 3)
#' District_Results("Alaska")


District_Results <- function(state, district_no = 0){
  Election_Data[,1] <- gsub("[[:space:]]", " ", Election_Data[,1])
  district <- paste(state, district_no, sep = " ")
  if(district_no == 0){
    m <- filter(Election_Data, grepl(state, District))
  }else{
    m <- filter(Election_Data, District == district)
  }
  return(m)
}