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
      menuItem('ARIMA Model', tabName = 'ARIMAModel', icon = icon('th')),
      menuItem('Linear Model', tabName = 'LinearModel', icon = icon('th')),
      menuItem('Exponential Smoothing', tabName = 'ETSModel', icon = icon('th')),
      menuItem('Neural Network', tabName = 'NNETModel', icon = icon('th'))
    )
  ),
  dashboardBody(
    tabItems(
      # Autoplot/seasonality tab content
      tabItem(tabName = "AutoSeasonDecomp",
              fluidRow(
                h2("The Empire's Monthly Income and the Seasonality of Galactic Credits"),
                plotOutput("fullseries"),
                plotOutput("season")
              )
      ),
      # ARIMA tab content
      tabItem(tabName = "ARIMAModel",
              fluidRow(
                h2("ARIMA Model")
              )
      ),
      # Linear Model tab content
      tabItem(tabName = "LinearModel",
              fluidRow(
                h2("Linear Model")
              )
      ),
      #ETS Model tab content
      tabItem(tabName = "ETSModel",
              fluidRow(
                h2("ETS Model")
              )
      ),
      #Neural network tab content
      tabItem(tabName = "NNETModel",
              fluidRow(
              h2("NNET Model")
              )
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