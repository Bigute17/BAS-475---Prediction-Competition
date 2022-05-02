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
      menuItem('Plotted Series and Seasonality', tabName = 'AutoSeasonDecomp', icon = icon('dashboard')),
      menuItem('First Model', tabName = 'FirstModel', icon = icon('th'))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "AutoSeasonDecomp",
              fluidRow(
                h2("AutoSeasonDecomp tab content")
              )
      ),
      # Second tab content
      tabItem(tabName = "FirstModel",
              h2("FirstModel tab content")
      )
    )
  )
)

server <- function(input, output) { }

shinyApp(ui, server)