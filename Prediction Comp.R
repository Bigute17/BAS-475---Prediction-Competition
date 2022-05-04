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

#Train and Test 
train <- head(CREDIT, nrow(CREDIT) - 12)
test <- tail(CREDIT, 12)

#Model Cross-Validation
fit <- train %>% 
  stretch_tsibble(.init = 48, .step = 24) %>% 
  model(
    arima = ARIMA(credit_in_millions),
    ets = ETS(credit_in_millions),
    tslm = TSLM(credit_in_millions~trend()+season())
  )

fit %>% 
  forecast(h=5) %>% 
  accuracy(train) %>% 
  arrange(RMSE)

#Final Model
bestfit <- train %>% 
  model(ARIMA(credit_in_millions))

report(bestfit)

#Predictions
pred <- bestfit %>% 
  forecast(test)

y_pred <- pred$.mean

finalrmse <- rmse(test$credit_in_millions, y_pred)

ui <- dashboardPage(
  dashboardHeader(title = "Galactic Credits"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Plotted Series and Seasonality', tabName = 'AutoSeasonDecomp', icon = icon("dashboard")),
      menuItem('Model Cross-Validation', tabName = 'MODELCV', icon = icon('th')),
      menuItem('Final Model Evaluation', tabName = 'MODELEVAL', icon = icon('th')),
      menuItem('Predictions For the Next 12 Months', tabName = '12monthpred', icon=icon('th'))
    )
  ),
  dashboardBody(
    tabItems(
      # Autoplot/seasonality tab content
      tabItem(tabName = "AutoSeasonDecomp",
              fluidRow(
                h2("The Empire's Monthly Income and the Seasonality of Galactic Credits"),
                plotOutput("fullseries"),
                plotOutput("season"),
                plotOutput("decomp")
              )
      ),
      # Model CV tab content
      tabItem(tabName = "MODELCV",
              fluidRow(
                h3("Model Cross-Validation"),
                tableOutput("CV")
              )
      ),
      # Final Model tab content
      tabItem(tabName = "MODELEVAL",
              fluidRow(
                h2("Final Model Evaluation"),
                textOutput("bestmodel"),
                plotOutput("residuals"),
                textOutput("predictions"),
                textOutput("last")
              )
      ),
      #Predictions tab content
      tabItem(tabName = '12monthpred',
              fluidRow(
                h3("12 Month Forecast"),
                
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
output$decomp <- renderPlot({
  CREDIT %>% 
    model(STL(credit_in_millions)) %>% 
    components() %>% 
    autoplot()
})
output$CV <- renderTable({
  fit %>% 
    forecast(h=5) %>% 
    accuracy(train) %>% 
    arrange(RMSE)
})
output$bestmodel <- renderPrint({
  report(bestfit)
})
output$residuals <- renderPlot({
  gg_tsresiduals(bestfit)
})
output$predictions <- renderPrint({y_pred})
output$last <- renderPrint({
  finalrmse
})
output$finalpredictionplot <- renderPlot({
  bestfit %>% 
    forecast(h=12) %>% 
    autoplot(CREDIT)
})

}







shinyApp(ui, server)