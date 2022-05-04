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

autoplot(CREDIT)

#box cox and differencing
lambda <- CREDIT %>% 
  features(credit_in_millions, features = guerrero) %>% 
  pull(lambda_guerrero)


lambda

CREDIT <- CREDIT %>% 
  mutate(bc_credit_in_millions = box_cox(credit_in_millions, lambda)) %>% 
  mutate(bc_credit_in_millions = difference(bc_credit_in_millions,12)) %>% 
  mutate(bc_credit_in_millions = difference(bc_credit_in_millions))

CREDIT %>%
 gg_tsdisplay(bc_credit_in_millions, plot_type = 'partial')

#train and test
trainarima <- head(CREDIT, nrow(CREDIT) - 12)

testarima <- tail(CREDIT, 12)

#Model
fitarima <- trainarima %>% 
  model(ARIMA(bc_credit_in_millions))

report(fitarima)

#ljung_box test - passed
augment(fitarima) %>%
  features(.innov, ljung_box)


fitarima %>%
  forecast(h=12) %>%
  autoplot(CREDIT)

gg_tsresiduals(fitarima)

#ARIMA Predictions
arimapred <- fitarima %>% 
  forecast(testarima)

arima_y_pred <- inv_box_cox(arimapred$.mean, lambda)
arimarmse <- rmse(inv_box_cox(testarima$bc_credit_in_millions, lambda), arima_y_pred)

