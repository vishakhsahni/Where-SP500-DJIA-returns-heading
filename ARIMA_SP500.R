##Loading Packages Package Installation##
install.packages(c("quantmod","plyr"))
install.packages("quantmod")
install.packages("tseries")
install.packages("timeSeries")
install.packages("forecast")
install.packages("xts")
install.packages("quadprog")

##Libraries Used##
library(quantmod)
library(plyr)
library(xts)
library(quantmod)
library(tseries)
library(timeSeries)
library(quadprog)
library(forecast)
require(forecast)


##Loading the S&P 500 Data##

getSymbols('^GSPC', from='2000-01-01', to='2017-12-07')

spclose1 = GSPC[,4]
spclose1

##Computing log returns##

splog1 = diff(log(spclose1),lag = 1)
splog1 = splog1[!is.na(splog1)]

splog1
##Plotting log returns##
plot(splog1,type='l', main='log returns plot')

##Checking the stationarity##
##Augumented Dickey Fuller Test##
print(adf.test(splog1))

##Results Augmented Dickey-Fuller Test##

# Data:  splog1
# Dickey-Fuller = -16.02, Lag order = 16, p-value = 0.01
# alternative hypothesis: stationary

##Box-Ljung test##
Box.test(splog1, lag = 10, type = "Ljung-Box")
##Results Box-Ljung Test##
# 
# Data:  splog1
# X-squared = 57.523, df = 10, p-value = 1.063e-08

#We reject null-hypothesis-/No serial correlation

#S#plit the dataset in two parts - training and testing##
breakpoint = floor(nrow(splog1)*(9.5/10))

##Apply the ACF and PACF functions on truncated data using breakpoint##
par(mfrow = c(1,1))
acf.splog1 = acf(splog1[c(1:breakpoint),], main='ACF Plot',lag.max = 100)
pacf.splog1 = pacf(splog1[c(1:breakpoint),], main='PACF Plot',lag.max = 100)

##No differencing required as data is stationary##
#Ar(3)
arima(splog1, order = c(0,0,2)) 
arima(splog1, order = c(1,0,2)) # aic = -26974.21  #Best Model

arima(splog1, order = c(2,0,2)) #aic = -26972.62
arima(splog1, order = c(3,0,2))  #aic = -26970.59


##Cross checking with auto arima##
#Auto arima 
auto.arima(splog1) #(1,0,2)

##Fitting arima model##
arima.fit <- arima(splog1, order = c(1,0,2))
arima.fit
future <- forecast(arima.fit, h = 5000)
plot(future)

############################################################
#1- Forecasting the entire returns series from breakpoint onwards.
#2- Within this loop we will forecast returns for each data point from the test dataset.

##Initialzing an xts object for Actual log returns##

Actual_series = xts(0,as.Date("2014-11-25","%Y-%m-%d"))

#3- We first initialize a series which will store the actual returns and another series to store the forecasted returns.
#4- In the For Loop, we first form the training dataset and the test dataset based on the dynamic breakpoint.



##Initialzing a dataframe for the forecasted return series##
forecasted_series = data.frame(Forecasted = numeric())

for (b in breakpoint:(nrow(splog1)-1)) {
  
  splog_train = splog1[1:b, ]
  splog_test = splog1[(b+1):nrow(splog1), ]
  
  #5- fitting the model, using ARMA parameters (1,0,2)
  
  ##Summary of the ARIMA model using the determined (p,d,q) parameters##
  fit = arima(splog_train, order = c(1, 0, 2),include.mean=FALSE)
  summary(fit)
  
  ##Plotting a acf plot of the residuals##
  acf(fit$residuals,main="Residuals plot")
  
  #6- Forecasting, Confidence level is 99% , to enhance the model. 
  ##Forecasting the log returns##
  arima.forecast = forecast(fit, h = 1,level = 99)
  summary(arima.forecast)
  
  ##Plotting the forecast##
  par(mfrow=c(1,1))
  plot(arima.forecast, main = "ARIMA Forecast")
  
  ##Creating a series of forecasted returns for the forecasted period##
  forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
  colnames(forecasted_series) = c("Forecasted")
  
  ##Creating a series of actual returns for the forecasted period##
  Actual_return = splog1[(b+1),]
  Actual_series = c(Actual_series,xts(Actual_return))
  rm(Actual_return)
  
  print(spclose1[(b+1),])
  print(spclose1[(b+2),])
  
}
###########################################################
##Adjust the length of the Actual return series##
Actual_series = Actual_series[-1]

##Create a time series object of the forecasted series##
forecasted_series = xts(forecasted_series,index(Actual_series))
forecasted_series <- forecasted_series

library(dygraphs)
Actual_series <- Actual_series
Actual_series
dygraph(Actual_series)

plot(Actual_series,type='l',main='Actual Returns Vs Forecasted Returns')
lines(forecasted_series,lwd=1.5,col='red')
legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))

##Create a table for the accuracy of the forecast##
comparison = merge(Actual_series,forecasted_series)
comparison$Accuracy = sign(comparison$Actual_series)==sign(comparison$Forecasted)
comparison
print(comparsion)

dygraph(comparsion)


##Compute the accuracy percentage metric##
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)


fit_res = residuals(arima.fit)

#diagnostics #Check serial coorelation #randomness of the residual
Box.test(fit_res, lag = 10, type = "Ljung-Box")

# 
# Box-Ljung test
# 
# data:  fit_res
# X-squared = 18.115, df = 10, p-value = 0.05306
#Since the pvalue is > 0.05, indicating that the residuals are random
#and that the model provides an adequate fit to the data. 


tsdiag(arima.final)

#Forcasting the returns
pred <- predict(arima.final, n.ahead = 2)
pred





###################################################################
##################DJIA-ARIMA-MODEL##########################

##Loading packages Package Installation##
install.packages(c("quantmod","plyr"))
install.packages("quantmod")
install.packages("tseries")
install.packages("timeSeries")
install.packages("forecast")
install.packages("xts")

##Libraries Used##
library(quantmod)
library(plyr)
library(xts)
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)

##Loading the DJIA Data##

getSymbols('DJI', from='2000-01-01', to='2017-12-04')


djclose = DJI[,4]
View(djclose)

##Computing log returns##

djlog = diff(log(djclose),lag = 1)
djlog = djlog[!is.na(djlog)]
djlog

##Plot log returns##
plot(djlog,type='l', main='log returns plot')
nrow(djlog)

##Checking the stationarity##
##Augumented Dickey Fuller Test##
print(adf.test(djlog))


##Results Augmented Dickey-Fuller Test##
# 
# data:  djlog
# Dickey-Fuller = -16.273, Lag order = 16, p-value = 0.01
# alternative hypothesis: stationary

##Box-Ljung test##
Box.test(djlog, lag = 10, type = "Ljung-Box")
##Results Box-Ljung test##
# 
# data:  djlog
# X-squared = 59.504, df = 10, p-value = 4.497e-09

##We reject null-hypothesis-/No serial correlation##

##Split the dataset in two parts - training and testing##
breakpoint = floor(nrow(djlog)*(9.5/10))

breakpoint

##Apply the ACF and PACF functions on truncated data using breakpoint##
par(mfrow = c(1,1))
acf.djlog = acf(djlog[c(1:breakpoint),], main='ACF Plot',lag.max = 100)
pacf.djlog = pacf(djlog[c(1:breakpoint),], main='PACF Plot',lag.max = 100)

##No differencing required as data is stationary##
#Ar(3)
arima(djlog, order = c(0,0,2)) 
arima(djlog, order = c(2,0,2)) # aic = -27580.33  #best model
arima(djlog, order = c(3,0,2)) #aic = -26972.62
arima(djlog, order = c(3,0,3))  #aic = -26970.59

##Cross checking with auto arima##
#Auto arima 

auto.arima(djlog) #(2,0,2)

##Fitting arima model##
arima.fit <- arima(djlog, order = c(2,0,2))
arima.fit

fit <- forecast(arima.fit, h=400)
plot(fit)
############################################################
#1- Forecasting the entire returns series from breakpoint onwards.
#2- Within this loop we will forecast returns for each data point from the test dataset.
# Initialzing an xts object for Actual log returns
Actual_series1 = xts(0,as.Date("2014-11-25","%Y-%m-%d"))

#3- We first initialize a series which will store the actual returns and another series to store the forecasted returns.
#4- In the For Loop, we first form the training dataset and the test dataset based on the dynamic breakpoint.



##Initialzing a dataframe for the forecasted return series##
forecasted_series1 = data.frame(Forecasted = numeric())

for (b in breakpoint:(nrow(djlog)-1)) {
  
  djlog_train = djlog[1:b, ]
  djlog_test = djlog[(b+1):nrow(djlog), ]
  
  #5- fitting the model, using ARMA parameters (1,0,2)
  
  ##Summary of the ARIMA model using the determined (p,d,q) parameters##
  fit = arima(djlog_train, order = c(1, 0, 2),include.mean=FALSE)
  summary(fit)
  
  ##Plotting a acf plot of the residuals##
  acf(fit$residuals,main="Residuals plot")
  
  #6- Forecasting, Confidence level is 99% , to enhance the model. 
  ##Forecasting the log returns##
  arima.forecast = forecast(fit, h = 1,level = 99)
  summary(arima.forecast)
  
  ##Plotting the forecast##
  par(mfrow=c(1,1))
  plot(arima.forecast, main = "ARIMA Forecast")
  
  ##Creating a series of forecasted returns for the forecasted period##
  forecasted_series1 = rbind(forecasted_series1,arima.forecast$mean[1])
  colnames(forecasted_series1) = c("Forecasted")
  
  ##Creating a series of actual returns for the forecasted period##
  Actual_return1 = djlog[(b+1),]
  Actual_series1 = c(Actual_series1,xts(Actual_return1))
  rm(Actual_return1)
  
  print(djclose[(b+1),])
  print(djclose[(b+2),])
  
}

###########################################################
##Adjust the length of the Actual return series##
Actual_series1 = Actual_series1[-1]

##Create a time series object of the forecasted series##
forecasted_series1 = xts(forecasted_series1,index(Actual_series1))
forecasted_series1


########################################
# candleChart(forecasted_series, up.col = "black", dn.col = "red", theme = "white")
# addSMA(n = 20)
# lines(Actual_series,lwd=1.5,col='red')
# legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))
# # Create a plot of the two return series - Actual versus Forecasted
###########################################
dygraph(Actual_series1)
plot(Actual_series1,type='l',main='Actual Returns Vs Forecasted Returns')
lines(forecasted_series1,lwd=1.5,col='red')
legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))

##Create a table for the accuracy of the forecast##
comparsion1 = merge(Actual_series1,forecasted_series1)
comparsion1 <- comparsion1*1000000000
print(comparsion1)
View(comparsion1)
library(dy)
dygraph(comparsion1, xlab = "Time", ylab= "Log Returns of DJIA", main = "Actual VS Forecasted") %>%
  dyRangeSelector(height = 50)
# comparsion$Accuracy = sign(comparsion$Actual_series1)==sign(comparsion$Forecasted)
# print(comparsion)



##############MAE and RMSE on Comparison1###################
rmse <- function(error)
{
  sqrt(mean(error^2))
}



##Function that returns Mean Absolute Error##
mae <- function(error)
{
  mean(abs(error))
}
error <- comparsion1[,1] - comparsion1[,2]
rmse(error)
mae(error)
##Compute the accuracy percentage metric##
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)

fit_res = residuals(arima.fit)
plot(fit_res)

##Diagnostics #Check serial coorelation #randomness of the residual##
Box.test(fit_res, lag = 10, type = "Ljung-Box")

# 
# Box-Ljung test
# 
# data:  fit_res
# X-squared = 18.115, df = 10, p-value = 0.05306
#Since the pvalue is > 0.05, indicating that the residuals are random
#and that the model provides an adequate fit to the data. 


tsdiag(arima.final)

##Forcasting the returns##
pred <- predict(arima.final, n.ahead = 2)
pred
