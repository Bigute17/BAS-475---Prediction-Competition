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

#ARIMA Testing

CREDIT2 <- read.csv("credit.csv")
CREDIT2$Month <- seq(as.Date('2021/12/1'), by = "-1 month", length.out = 492)
CREDIT2$Month <- yearmonth(CREDIT2$Month)
CREDIT2 <- tsibble(CREDIT2, index = Month)
#Make it stationary
CREDIT2 <- CREDIT2 %>% 
  mutate(credit_in_millions = sqrt(credit_in_millions)) %>% 
  mutate(credit_in_millions = difference(credit_in_millions,12)) 

gg_tsdisplay(CREDIT2, plot_type = 'partial')
#ARIMA MODEL
fit <- CREDIT2 %>% 
  model(ARIMA(credit_in_millions))

report(fit)

fit %>% 
  forecast(h = 10) %>% 
  autoplot(CREDIT2)

gg_tsresiduals(fit)
