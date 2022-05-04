
output$neuralforecast <- renderPlot({
  fitneural <- CREDIT %>%
    model(NNETAR(bc_credit_in_millions))
  fitneural %>%
    forecast(h=12, times = 6) %>%
    autoplot(CREDIT)})

  output$etsforecast <- renderPlot({
    fitets %>%
      forecast(h=12) %>%
      autoplot(CREDIT)
  })
output$etsresiduals <- renderPlot({
  gg_tsresiduals(fitets)
})
output$etsreport <- renderPrint({report(fitets)})
output$etspredictions <- renderPrint({ets_y_pred})
output$etsrmse <- renderPrint({etsrmse})




#ARIMA Model
fitarima <- train %>% 
  model(ARIMA(credit_in_millions, stepwise = FALSE, approx = FALSE))

report(fitarima)


#ARIMA Predictions
arimapred <- fitarima %>% 
  forecast(test)

arima_y_pred <- arimapred$.mean
arimarmse <- rmse(test$credit_in_millions, arima_y_pred)


# TSLM model
tslmfit <- CREDIT %>%
  model(tslm=TSLM(credit_in_millions~trend() + season()))

report(tslmfit)

#tslm predictions
tslmpred <- tslmfit %>% 
  forecast(test) 

tslm_y_pred <- tslmpred$.mean
tslmrmse <- rmse(test$credit_in_millions, tslm_y_pred)

#ETS model
fitets <- train %>%
  model(ETS(credit_in_millions))

report(fitets)

#ETS predictions
etspred <- fitets %>%
  forecast(test)

ets_y_pred <- etspred$.mean 
etsrmse <- rmse(test$credit_in_millions, ets_y_pred)

#NNET model
fitneural <- CREDIT %>%
  model(NNETAR(credit_in_millions))
report(fitneural)
