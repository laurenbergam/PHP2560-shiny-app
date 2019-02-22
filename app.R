#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("rvest")
#install.packages("shinythemes")
#install.packages("ggrepel")
#install.packages("xml2")


library(shiny)
library(ggplot2)
library(dplyr)
library(rvest)
library(shinythemes)
library(ggrepel)
library(xml2)

load("~/GitHub/final-project-houseelections/Shiny-App/data/tabcountry.Rdata")
load("~/GitHub/final-project-houseelections/Shiny-App/data/tabstate.Rdata")
load("~/GitHub/final-project-houseelections/Shiny-App/data/Cleaned_House_Election_Results_States.Rdata")


house_data <- Cleaned_House_Election_Results_States
house_data$District <- gsub('[0-9]', '', house_data$District)
house_data$District <- gsub('at-large', '', house_data$District)

#Import population by state data
pop_url <- "https://simple.wikipedia.org/wiki/List_of_U.S._states_by_population"
pop_page <- read_html(pop_url)
pop_tables <- html_table(html_nodes(pop_page, "table"), fill = TRUE)
pop_table <- pop_tables[[1]]

#Some light cleaning and relabeling
pop_table <- select(pop_table, 3,4)
pop_table <- pop_table[ -c(30,50,53:69), ]
colnames(pop_table)[colnames(pop_table)=="State, federal district, or territory"] <- "State"
colnames(pop_table)[2] = "Population_Est"

#Remove commas from integers
pop_table[,2] = as.integer(gsub(",","",pop_table[,2]))

#Arrange by state
arranged_pop_table = pop_table %>% arrange(State)
arranged_pop_table$state_abbr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY" )
#End for plot 3

# Define UI for application
ui <- navbarPage(theme = shinytheme("spacelab"), "House Elections: A Look at Historic Data and Social Determinants",
                 
                 # Show a plot of the generated state-specific bar
                 tabPanel("Welcome", 
                          br(),br(),br(),br(),
                          h3("Hello RStudio Internship Committee!", align = "center"),
                          h4(tags$div("This is my portion of an app I developed on a team in a graduate level class.
                                      This app pulls data from our own R datatable cleaned and compiled from the internet and directly from a wikipedia table. 
                                      My Visualizing Elections tab shows historic trends dating between 1998 and 2016 of 
                                      the United States House of Representatives election data. ", align = "center"))
                          ),
                 tabPanel("Visualizing Elections",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("year",
                                          "Election Years",
                                          min = 1998,
                                          max = 2016,
                                          value = 2002,
                                          step = 2,
                                          sep = ""
                              )
                            ),
                            mainPanel(
                              plotOutput("rep_count_plot") ,
                              h4("The graphic above is a simple bar graph counting the number of House Representatives
                                 in each state, separated by party."),
                              br(),br(),
                              plotOutput("state_party_plot"),
                              h4("This bar graph shows the same data as the first except as a  
                                 net value, that is, the number of Representatives in the majority party
                                 minus the number of Representatives in the minority party per state."),
                              br(),br(),
                              plotOutput("pop_state_2"),
                              h4("This scatter plot graphs state population vs number of Representatives. 
                                 This is a linear relationship by design since a representative is appointed
                                 per 700,000 US citizens, on average. The color of each point shows the
                                 dominating party of that state."),
                              br(),br())))
                 
                            )



# Define server logic required to draw a bar
server <- function(input, output) {
  
  #year scale
  output$rep_count_plot <- renderPlot({
    house_data <-   house_data[[as.character(input$year)]] %>% 
      group_by(State, Winning_Party) %>%
      summarize(num = n())
    
    #plot 1
    ggplot(house_data, aes(State, num)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_fill_manual(values = c("royalblue3", "red1", "grey")) + 
      scale_y_continuous(breaks = seq(0, 60, 10)) +
      geom_bar(stat = "identity", aes(fill = Winning_Party))
  })
  
  
  #plot 2
  output$state_party_plot <- renderPlot({
    state_party_majority <- house_data[[as.character(input$year)]] %>%
      group_by(State) %>%
      summarize(num_rep = sum(Winning_Party=="Republican",na.rm=TRUE),
                num_dem = sum(Winning_Party=="Democrat",na.rm=TRUE),
                total_rep = num_rep + num_dem) %>%
      group_by(State) %>%
      summarize(majority_party = num_dem - num_rep) %>%
      mutate(majority = ifelse(majority_party > 0, "majority_dem", "majority_rep"))
    
    ggplot(state_party_majority, aes(State, majority_party)) + 
      theme_bw()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_fill_manual(values = c("royalblue3", "red1")) +
      scale_y_continuous(breaks = seq(0, 60, 10)) +
      geom_bar(stat = "identity", aes(fill = majority))
  })
  
  #plot 3
  output$pop_state_2 <- renderPlot({
    
    pop_state_2 <- Election_Data %>%
      filter(Election_Year == input$year) %>%
      
      #Consolidate districts into states and count number of representatives in each party
      group_by(State) %>%
      summarize(num_rep = sum(Winning_Party=="Republican",na.rm=TRUE),
                num_dem = sum(Winning_Party=="Democrat",na.rm=TRUE),
                total_rep = num_rep + num_dem) %>%
      
      #Column bind population to dem/rep count table
      arrange(State) %>%
      cbind(Population = arranged_pop_table[,2]) %>%
      cbind(Abbr = arranged_pop_table[,3])
    
    ggplot(pop_state_2 %>% mutate(pct_republican = num_rep/total_rep, majority = ifelse(pct_republican > 0.5, "majority_republican", "majority_democrat")), aes(Population, total_rep) ) +
      geom_point(size = 4, alpha = 0.5, aes(color = majority)) +
      scale_color_manual(values = c("blue", "red")) +
      geom_smooth(method=lm, se = FALSE) +
      geom_text_repel(aes(label = Abbr)) +
      #scale_x_log10() + #For readability
      theme_bw() 
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)