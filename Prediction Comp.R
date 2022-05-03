library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(fpp3)

CREDIT <- read.csv("credit.csv")
CREDIT$Month <- seq(as.Date('2021/12/1'), by = "-1 month", length.out = 492)
CREDIT$Month <- yearmonth(CREDIT$Month)
CREDIT <- tsibble(CREDIT, index = Month)


ui <- dashboardPage(
  dashboardHeader(title = "Galactic Credits"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Plotted Series and Seasonality', tabName = 'AutoSeasonDecomp', icon = icon("dashboard")),
      menuItem('First Model', tabName = 'FirstModel', icon = icon('th'))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "AutoSeasonDecomp",
              fluidRow(
                h2("The Empire's Monthly Income and the Seasonality of Galactic Credits"),
                plotOutput("fullseries"),
                plotOutput("season")
              )
      ),
      # Second tab content
      tabItem(tabName = "FirstModel",
              h2("First Model")
      )
    )
  )
)

server <- function(input, output) { 
output$fullseries <- renderPlot({
  CREDIT %>%
    gg_tsdisplay(credit_in_millions)
})
output$season <- renderPlot({
 CREDIT %>% 
    gg_subseries(credit_in_millions)
})

  


}
shinyApp(ui, server)