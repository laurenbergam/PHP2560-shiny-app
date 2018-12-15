house_prez_party <- function(year) {
  
  #Import data of party divisions through history
  all_years_url <- "https://history.house.gov/Institution/Party-Divisions/Party-Divisions/"
  all_years_page <- read_html(all_years_url)
  all_years_tables <- html_table(html_nodes(all_years_page, "table"), fill = TRUE)
  all_years_table <- all_years_tables[[1]]
  
  #Some light cleaning and relabeling
  all_years_table <- select(all_years_table, 1:4)
  all_years_table <- all_years_table[ -c(1:71, 74,84,96,107,118), ]
  colnames(all_years_table)[colnames(all_years_table)=="Pro-Administration"] <- "Democrat"
  colnames(all_years_table)[colnames(all_years_table)=="Anti-Administration"] <- "Republican"
  colnames(all_years_table)[colnames(all_years_table)=="Anti-Administration"] <- "Republican"
  colnames(all_years_table)[colnames(all_years_table)=="Congress (Years)"] <- "Year"
  
  #Insert column of President party
  all_years_table$President_Party <- c(rep("Democrat", 4), rep("Republican", 6), rep("Democrat", 10), rep("Republican", 4), rep("Democrat", 4), rep("Republican", 4), rep("Democrat", 2), rep("Republican", 6), rep("Democrat", 4), rep("Republican", 4), rep("Democrat", 4), rep("Republican", 1))
  
  #sapply(all_years_table, class) turns out they were character values, turn them instead into integers
  all_years_table$Democrat <- as.numeric(all_years_table$Democrat)
  all_years_table$Republican <- as.numeric(all_years_table$Republican)
  
  #All years data - Find dominant party: dems are "positive" numbers, republicans "negative" numbers
  all_years_table_2 <- all_years_table %>%
    group_by(Year) %>%
    mutate(dominant_party = Democrat - Republican)
  
  #Relabel first column
  all_years_table_3 <- all_years_table_2 %>% separate(Year,into=c("Congress Years","Year"))
  
  #Bars above 0 indicate Democrats controlled the House
  #Bars below 0 indicate Republicans controlled the House
  #Color of bars indicate the party of the president at the time
  ggplot(all_years_table_3, aes(Year, dominant_party) ) +
    geom_bar(stat = "identity", aes(fill = President_Party)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_fill_manual(values = c("royalblue3", "red1"))
  
  
}