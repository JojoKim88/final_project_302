library(shiny)
#library(shinythemes)
library(dplyr)
library(ggplot2)
library(readr)

Titanic <- as.data.frame(Titanic)

ui <- fluidPage(
  
  titlePanel("Survival Titanic"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Passenger_Class",
                  label = "Enter passenger category:",
                  choices = c("1st", "2nd", "3rd", "Crew", "All") ),
      
      selectInput(inputId = "Sex", 
                  label = "Enter gender of interest", 
                  choices = c("Male", "Female", "Both")  )),
    
    mainPanel(plotOutput(outputId = "histogram"))))

server <- function(input, output) {
  
  selectData = reactive({
      Titanic_subset <- Titanic
    
    if (input$Sex == "Male") {Titanic_subset <- Titanic_subset %>% filter(Sex == "Male")}
    if (input$Sex == "Female") {Titanic_subset <- Titanic_subset %>% filter(Sex == "Female")}
    if (input$Sex == "Both") {Titanic_subset <- Titanic_subset}
    Titanic_subset
  })  
  output$histogram <- renderPlot({
    
    ggplot(selectData(), aes( x= Class, y= Freq, fill = Survived) ) +
      geom_bar(stat = "identity", position = position_dodge() ) +
      ylab ("Number of Passengers")})
  
}

shinyApp(ui = ui, server = server)

