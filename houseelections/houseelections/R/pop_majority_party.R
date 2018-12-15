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
    
    #Consolidate districts into states and count number of representatives in each party
    state_party_count_2 <- by_state %>%
      group_by(State) %>%
      summarize(num_rep = sum(Winning_Party=="Republican",na.rm=TRUE),
                num_dem = sum(Winning_Party=="Democrat",na.rm=TRUE),
                total_rep = num_rep + num_dem)
    
    #Column bind population to dem/rep count table
    state_party_count_4 <- state_party_count_2 %>% arrange(State) %>% cbind(arranged_pop_table[,2])
    colnames(state_party_count_4)[5] = "Population"
    
    #sapply(state_party_count_4, class) necessary columns contain integers
    
    #Population vs Number of Representatives
    #Color is the majority party for each state
    #Have option for labels, but reduces readability
    #Have option for log scale to increase readability
    ggplot(state_party_count_4 %>% mutate(pct_republican = num_rep/total_rep, majority = ifelse(pct_republican > 0.5, "majority_republican", "majority_democrat")), aes(Population, total_rep) ) +
      geom_point(size = 4, alpha = 0.5, aes(color = majority)) +
      scale_color_manual(values = c("blue", "red"))+
      theme_bw() 
    #  geom_text_repel(aes(label = State)) 
    #  scale_x_log10()
    
  } else {
    print("Choose an even year between and including 1998 and 2016")
  }
  
}