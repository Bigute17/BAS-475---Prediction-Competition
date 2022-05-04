CREDIT <- read.csv("credit.csv")


head(CREDIT, 3)
summary(CREDIT$credit_in_millions)


autoplot(CREDIT)


CREDIT %>% 
  gg_subseries()


CREDIT %>% 
  model(STL(credit_in_millions)) %>% 
  components() %>% 
  autoplot()

lambda <- CREDIT %>% 
  features(credit_in_millions, features = guerrero) %>% 
  pull(lambda_guerrero)

lambda

CREDIT <- CREDIT %>% 
  mutate(bc_credit_in_millions = box_cox(credit_in_millions, lambda))

CREDIT %>% 
  autoplot(bc_credit_in_millions)

CREDIT %>% 
  model(STL(bc_credit_in_millions)) %>% 
  components() %>% 
  autoplot()

CREDIT <- CREDIT %>% 
  mutate(log_credit_in_millions = log(credit_in_millions))

CREDIT %>% 
  autoplot(log_credit_in_millions)

CREDIT %>% 
  model(STL(log_credit_in_millions)) %>% 
  components() %>% 
  autoplot()

#Transformations don't seem to fix the nonconstant variance

  
#Train and Test
train <- head(CREDIT, nrow(CREDIT) - 12)
test <- tail(CREDIT, 12)

#Cross-validation
fit <- train %>% 
  stretch_tsibble(.init = 48, .step = 24) %>% 
  model(
    tslm = TSLM(credit_in_millions~trend() + season()),
    arima = ARIMA(credit_in_millions),
    ets = ETS(credit_in_millions),
    nnet = NNETAR(credit_in_millions)
  )

fit %>% 
  forecast(h=12) %>% 
  accuracy(train) %>% 
  arrange(RMSE)


