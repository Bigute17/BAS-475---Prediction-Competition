library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(fpp3)

CREDIT <- read.csv("credit.csv")




ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)