#' Number of representatives of each party sent to Congress by a given state.
#'
#' Given the name of a state, this function will use the Democrats_State, Republicans_State,
#' and Independents_State to create a table that tells you how many Democrats, Republicans,
#' Independents. The data is in a format that makes it easy to create a bar graph
#' representing the information, which is used in the shiny app. If a state did not
#' send an Independent in any year, Independents are not included in the table.
#' @param state The name of the state, properly capitalized.#
#' @export
#' @examples
#' State_Party_Data("Maryland")

State_Party_Data <- function(state){
  if(all(Independents_State(state) == 0)){
    a <- seq(from = 1998, to = 2016, by = 2)
    l <- c()
    for(i in a){
      l <- c(l, i, i)
    }
    k <- data.frame(l)
    names(k) <- "year"
    evens <- seq(from = 2, to = 20, by = 2)
    odds <- seq(from = 1, to = 19, by = 2)
    k[odds,2] <- "Republicans"
    k[evens, 2] <- "Democrats"
    k[odds,3] <- Republicans_State(state)
    k[evens, 3] <- Democrats_State(state)
    names(k) <- c("year", "party", "num")
  }else{
    a <- seq(from = 1998, to = 2016, by = 2)
    l <- c()
    for(i in a){
      l <- c(l, i, i,i)
    }
    k <- data.frame(l)
    names(k) <- "year"
    ones <- seq(from = 1, to = 28, by = 3)
    twos <- seq(from = 2, to = 29, by = 3)
    threes <- seq(from = 3, to = 30, by = 3)
    k[ones,2] <- "Republicans"
    k[twos, 2] <- "Democrats"
    k[threes,2] <- "Independents"
    k[ones,3] <- Republicans_State(state)
    k[twos, 3] <- Democrats_State(state)
    k[threes, 3] <- Independents_State(state)
    names(k) <- c("year", "party", "num")
  }
  return(k)
}