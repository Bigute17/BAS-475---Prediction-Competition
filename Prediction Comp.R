library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(fpp3)

rmse <- function(y_actual, y_pred) {
  sqrt(mean((y_actual - y_pred)^2))
}

CREDIT <- read.csv("credit.csv")
CREDIT$Month <- seq(as.Date('2021/12/1'), by = "-1 month", length.out = 492)
CREDIT$Month <- yearmonth(CREDIT$Month)
CREDIT <- tsibble(CREDIT, index = Month)

CREDIT2 <- read.csv("credit.csv")
CREDIT2$Month <- seq(as.Date('2021/12/1'), by = "-1 month", length.out = 492)
CREDIT2$Month <- yearmonth(CREDIT$Month)
CREDIT2 <- tsibble(CREDIT, index = Month)

#box_cox / differencing
lambda <- CREDIT %>% 
  features(credit_in_millions, features = guerrero) %>% 
  pull(lambda_guerrero)

CREDIT <- CREDIT %>% 
  mutate(bc_credit_in_millions = box_cox(credit_in_millions, lambda)) %>% 
  mutate(bc_credit_in_millions = difference(bc_credit_in_millions,12)) %>% 
  mutate(bc_credit_in_millions = difference(bc_credit_in_millions))

#train and test
trainarima <- head(CREDIT, nrow(CREDIT) - 12)
testarima <- tail(CREDIT, 12)

#Model
fitarima <- trainarima %>% 
  model(ARIMA(bc_credit_in_millions))

report(fitarima)

#ARIMA Predictions
arimapred <- fitarima %>% 
  forecast(testarima)

arima_y_pred <- inv_box_cox(arimapred$.mean, lambda)
arimarmse <- rmse(inv_box_cox(testarima$bc_credit_in_millions, lambda), arima_y_pred)

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
                h3("Stationary Credit Data"),
                plotOutput("stationary"),
                h3("Forecast From the ARIMA Model"),
                plotOutput("arimaforecast"),
                verbatimTextOutput("arimareport"),
                h3("Our Model's Residuals"),
                plotOutput("arimaresiduals"),
                textOutput("arimapredictions"),
                textOutput("arimarmse")
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
              h2("NNET Model"),
              plotOutput("neural network"),
              h3("Forecast From the Neural Network Model"),
              plotOutput("neuralforecast"),
              verbatimTextOutput("neuralreport"),
              plotOutput("neuralresiduals"),
              textOutput("neuralpredictions"),
              textOutput("neuralrmse")
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
output$stationary <- renderPlot({
    gg_tsdisplay(CREDIT, bc_credit_in_millions, plot_type = 'partial')
})
output$arimaforecast <- renderPlot({
  fitarima %>%
    forecast(h=12) %>%
    autoplot(CREDIT)
})
output$arimaresiduals <- renderPlot({
  gg_tsresiduals(fitarima)
})
output$arimareport <- renderPrint({report(fitarima)})
output$arimapredictions <- renderPrint({arima_y_pred})
output$arimarmse <- renderPrint({arimarmse})


output$neuralforecast <- renderPlot({
  fitneural <- CREDIT %>%
    model(NNETAR(bc_credit_in_millions))
  fitneural %>%
    forecast(h=12, times = 6) %>%
    autoplot(CREDIT)})

}






shinyApp(ui, server)