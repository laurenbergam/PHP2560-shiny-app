reps_by_state_stacked <- function(year) {
  if (year >= 1998 & year <= 2016 & as.numeric(year) %%2 == 0) {
    load(file = "~/Brown/PHP 2560/week-09-inclass-blrp-project/Election_Data2.RData")
    #change previous line to data("Cleaned_House_Election_Results_States") when in the package
    
    #Uniform district labeling
    by_state <- filter(Election_Data, Election_Year == year)
    by_state$District <- gsub('[0-9]', '', by_state$District)
    by_state$District <- gsub('at-large', '', by_state$District)
    
    #Consolidate rows, count number of republicans and democrats per state
    state_party <-  by_state %>% group_by(State, Winning_Party) %>% summarize(num = n())
    
    
    #Plot number of representatives per state, colored by party
    ggplot(state_party, aes(State, num)) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      geom_bar(stat = "identity", aes(fill = Winning_Party)) +
      if (year < 2006) {
        scale_fill_manual(values = c("royalblue3",  "limegreen", "red1"), breaks=c("Democrat", "Republican", "Independent") )
      } else {
        scale_fill_manual(values = c("royalblue3", "red1"), breaks=c("Democrat", "Republican") )
      }
    
    
  } else {
    print("Choose an even year between and including 1998 and 2016")
  }
}