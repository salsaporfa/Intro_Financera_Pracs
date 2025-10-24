knitr::opts_chunk$set(echo = TRUE)

# data(Tbrate, package="Ecdat")
# library(tseries)
# library(fGarch)
# # r = the 91 - day treasury bill rate
# # y = the log of real GDP
# # pi = the inflation rate
# 
# Tbill = Tbrate[,1]
# Del.Tbill = diff(Tbill)
# 
# garch.model = garchFit(formula =~ arma(1,0)+garch(1,0),Tbill)
# summary(garch.model)
# garch.model@fit$matcoef
# 
# res = residuals(garch.model)
# res_std =res/garch.model@sigma.t
# par(mfrow=c(2,3))
# plot(res)
# acf(res)
# acf(res^2)
# plot(res_std)
# acf(res_std)
# acf(res_std^2)

#1. a
  data(Tbrate, package="Ecdat")
  library(tseries)
  library(fGarch)
  Tbill = Tbrate[,1]
  plot(Tbill)
  acf(Tbill)
  adf.test(Tbill)
  kpss.test(Tbill)

  Del.Tbill = diff(Tbill)
  plot(Del.Tbill)
  acf(Del.Tbill)
  adf.test(Del.Tbill)
  kpss.test(Del.Tbill)

#1. b
  garch.model = garchFit(formula =~ arma(1,0)+garch(1,0),Tbill,trace = FALSE)
  summary(garch.model)
  garch.model@fit$matcoef

#1. c 
  res = residuals(garch.model)
  res_std =res/garch.model@sigma.t
  plot(res)
  acf(res)

#1. d
  acf(res^2)

#1. e
  acf(res_std^2)

plot(res_std)

#1. g
  fit <- garchFit(formula =~ arma(1,0)+garch(1,0),
                  diff(log(Tbill)), trace = FALSE)
  res = residuals(fit)
  res_std =res/fit@sigma.t
  plot(res_std)
  par(mfrow=c(2,2))
  acf(res_std)
  pacf(res_std)
  acf(res_std^2)
  pacf(res_std^2)

library(Ecdat)
library(fGarch)
data(SP500,package="Ecdat")
returnBM = SP500$r500[1805]
x = SP500$r500[(1804 - 2*253 + 1):1804]
plot(c(x, returnBM), type = 'l')
results=garchFit(~arma(1,0)+garch(1,1),data=x,cond.dist="std", trace = FALSE)
dfhat=as.numeric(results@fit$par[6])
forecast=predict(results,n.ahead=1)

#2. a
  mu_hat = forecast$meanForecast
  sigma_hat = forecast$standardDeviation
  dfhat = as.numeric(results@fit$par[6])
  z = (-0.228 - mu_hat) / sigma_hat
  cat("The probability of a return less or equal to 0.228 on Black Monday is: ",(p_blackmonday = pt(z, df = dfhat)))

#2 b.
  resid_std = residuals(results, standardize = TRUE)
  plot(resid_std, type='l', main="Standardized Residuals")
  acf(resid_std, main="ACF of Residuals")
  acf(resid_std^2, main="ACF of Squared Residuals")
  qqnorm(resid_std); qqline(resid_std)

#2. c
  results_arch = garchFit(~arma(1,0)+garch(1,0), 
                          data=x, cond.dist="std", trace = FALSE)
  resid_std = residuals(results_arch, standardize = TRUE)
  
  plot(resid_std, type='l', main="Standardized Residuals")
  acf(resid_std, main="ACF of Residuals")
  acf(resid_std^2, main="ACF of Squared Residuals")
  qqnorm(resid_std); qqline(resid_std)

#2. d
  ar1 = arima(x, order=c(1,0,0))
  res_ar1 = residuals(ar1)
  
  plot(res_ar1, type='l', main="Residuals")
  acf(res_ar1, main="ACF of Residuals")
  acf(res_ar1^2, main="ACF of Squared Residuals")
  qqnorm(res_ar1); qqline(res_ar1)

#3. 
  load("data_HW_3.RData")
  plot(x, type='l')
  par(mfrow=c(2,1))
  acf(x)
  pacf(x)
  print(adf.test(x))
  print(kpss.test(x))

  fit <- arima(x, order = c(2, 0, 0))
  res = residuals(fit)
  plot(res, type='l', main="Residuals")
  par(mfrow=c(2,2))
  acf(res, main="ACF of Residuals")
  pacf(res, main="PACF of Residuals")
  acf(res^2, main="ACF of Squared Residuals")
  pacf(res^2, main="PACF of Squared Residuals")

fit <- garchFit(~arma(2,0)+garch(1,1), 
                data=x, cond.dist="std", trace = FALSE)
res = residuals(fit, standardize = TRUE)
plot(res, type='l', main="Residuals")
par(mfrow=c(2,2))
acf(res, main="ACF of STD Residuals")
pacf(res, main="PACF of STD Residuals")
acf(res^2, main="ACF of Squared STD Residuals")
pacf(res^2, main="PACF of Squared STD Residuals")

# NA
