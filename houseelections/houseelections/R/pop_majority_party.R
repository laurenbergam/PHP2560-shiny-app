#' Plot Population by Representatives with Majority Party
#'
#' Given a year, this function compares the population versus number of representatives
#' by state with the color of the point the state's House party majority. 
#' @param year The year must be an even number between and including 1998 and 2016. 
#' @export
#' @examples
#' pop_majority_party(2002)



pop_majority_party <- function(year) {
  if (year >= 1998 & year <= 2016 & as.numeric(year) %%2 == 0) {
    
    #Import population by state data
    pop_url <- "https://simple.wikipedia.org/wiki/List_of_U.S._states_by_population"
    pop_page <- read_html(pop_url)
    pop_tables <- html_table(html_nodes(pop_page, "table"), fill = TRUE)
    pop_table <- pop_tables[[2]]
    
    #Election results data
    by_state <- filter(Election_Data, Election_Year == year)
    
    #Some light cleaning and relabeling
    pop_table <- select(pop_table, 3,4)
    pop_table <- pop_table[ -c(30,50,53:64), ]
    colnames(pop_table)[colnames(pop_table)=="State or territory"] <- "State"
    colnames(pop_table)[2] = "Population_Est"
    
    #Remove commas from integers
    pop_table[,2] = as.integer(gsub(",","",pop_table[,2]))
    
    arranged_pop_table = pop_table %>% arrange(State)
    arranged_pop_table$state_abbr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY" )
    
    #Consolidate districts into states and count number of representatives in each party
    state_party_count_2 <- by_state %>%
      group_by(State) %>%
      summarize(num_rep = sum(Winning_Party=="Republican",na.rm=TRUE),
                num_dem = sum(Winning_Party=="Democrat",na.rm=TRUE),
                total_rep = num_rep + num_dem)
    
    #Column bind population to dem/rep count table
    state_party_count_4 <- state_party_count_2 %>% 
      arrange(State) %>% 
      cbind(arranged_pop_table[,2]) %>%
      cbind(Abbr = arranged_pop_table[,3])
    colnames(state_party_count_4)[5] = "Population"
    
    #sapply(state_party_count_4, class) necessary columns contain integers
    
    #Population vs Number of Representatives
    #Color is the majority party for each state
    #Have option for log scale to increase readability
    ggplot(state_party_count_4 %>% mutate(pct_republican = num_rep/total_rep, majority = ifelse(pct_republican > 0.5, "majority_republican", "majority_democrat")), aes(Population, total_rep) ) +
      geom_point(size = 4, alpha = 0.5, aes(color = majority)) +
      scale_color_manual(values = c("blue", "red"))+
      geom_smooth(method=lm, se = FALSE) +
      geom_text_repel(aes(label = Abbr)) +
      theme_bw() 
    #  scale_x_log10()
    
  } else {
    print("Choose an even year between and including 1998 and 2016")
  }
  
}