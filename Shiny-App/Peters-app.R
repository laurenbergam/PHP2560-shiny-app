library(houseelections)
library(shiny)
library(shinythemes)
load("~/R/final-project-houseelections/houseelections/houseelections/data/Election_Data2.Rdata")

ui <- fluidPage(theme = shinytheme("spacelab"),
  titlePanel("House of Representatives Election Data 1998-2016
             "),
    mainPanel(
      tabsetPanel(
        tabPanel("State",
                 sidebarPanel(
                   selectizeInput("stateInput", "State",choices = state.name)), plotOutput("graph")),
        tabPanel("District", sidebarPanel(selectizeInput("stateInput2", "State",
                                                                                                    choices = state.name),
                 numericInput("districtInput", "District No:", 1)),tableOutput("district_party"), tableOutput("district")),
        tabPanel("Representative", sidebarPanel(
          selectizeInput("repInput", "Representative", choices = unique(Election_Data$Winner))), tableOutput("rep")
        )
      )
    )
)


server <- function(input, output) {
  output$district_party <- renderTable({
    party <- District_Party(input$stateInput2, input$districtInput)
    names <- names(party)
    party <- as.data.frame(party)
    party[,2] <- names
    party[,c(1,2)] <- party[,c(2,1)]
    names(party) <- c("", "")
    party
  })

  output$district <- renderTable({
   district <- District_Results(input$stateInput2, input$districtInput)
    district
  })

  output$graph <- renderPlot({
    if(nrow(State_Party_Data(input$stateInput)) == 20){
      g <- ggplot(State_Party_Data(input$stateInput),aes(x=year,y=num,fill=factor(party)))+
        geom_bar(stat="identity",position="dodge")+
        xlab("Year")+ylab("Number of Representatives Sent to Congress")+
        scale_fill_manual(values = c("royalblue3", "red1"))+
        guides(fill = guide_legend(title = NULL))
    }else{
      g <- ggplot(State_Party_Data(input$stateInput),aes(x=year,y=num,fill=factor(party)))+
        geom_bar(stat="identity",position="dodge")+
        xlab("Year")+ylab("Number of Representatives Sent to Congress")+
        scale_fill_manual(values = c("royalblue3", "gray", "red1"))+
        guides(fill = guide_legend(title = NULL))
    }
    g
  })

  output$rep <- renderTable({
    repInfo <- Representative_Info(input$repInput)
    names <- names(repInfo)
    
    repInfo
  })
}

shinyApp(ui = ui, server = server)
