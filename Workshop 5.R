#Load Library
library(forecast)
library(tseries)


#reading in data
data1 <- read.csv("differencing.csv", header=FALSE)
data1 <- ts(data1, frequency = 12)

plot(data1)

#KPSS
kpss.test(data1)
#since p-value smaller than printed p-value, we can reject null hypothesis that the data is 
#stationary and conclude that the data is not stationary

#ADF
adf.test(data1)

#This is a test to test with the null being that the data is stationary and that the alternative
#is that the data is not stationary
#since p-value is 0.19, we can't reject the null so it shows that the data is stationary

##Differencing

diff_data1 <- diff(data1)

plot(diff_data1)


#taking two differences

diff_data1 <- diff(data1, differences = 2)

plot(diff_data1)


diff(data1, lag = 12)

plot(AirPassengers)
plot(diff(AirPassengers,lag = 12))


#AR and MA

#AR process

#Load data
ar1.data <- read.csv(file = "ar1.data.csv", header = FALSE)
ar1.data <- ts(ar1.data, frequency = 12)

plot(ar1.data)

tsdisplay(ar1.data)



#MA process

#Load data
ma1.data <- read.csv(file = "ma1.data.csv", header= FALSE)
ma1.data <- ts(ma1.data, frequency = 12)

tsdisplay(ma1.data)


##Forceasting using ARIMA

#Load data
exercise <- read.csv("exercise.csv", header= FALSE)
#Convert to monthly time series
exercise <- ts(exercise, frequency = 12)


#Box Jenkins Methodology

#Fitting ARIMA

#Calculate ARIMA(2,1,0)
fit1 <- Arima(exercise, order=c(2,1,0))
#Plot series, ACF, and PACF of the residuals
tsdisplay(residuals(fit1))

#Fitting model manually, adding AR term
fit2 <- Arima(exercise, order = c(2,1,0))
tsdisplay(residuals(fit2))
summary(fit2)

#Extracting coefficients
coef(fit1)

forecast(fit1, h=12)





#Estimate AUTO ARIMA
auto_fit <- auto.arima(exercise)

#Find best method via AIC 
auto.arima(exercise, ic = "aic")

#Find the best method with ADF test 
auto.arima(exercise, test = "adf")




#Seasonal ARIMA

data <- AirPassengers

plot(data)

h <- 12

data_length <- length(data)
data_train <- ts(data[1:(data_length-h)], frequency = frequency(data), start = start(data))
data_test <- data[(data_length-h+1):data_length]

#Plot ACF and PACF of first differences
tsdisplay(diff(data_train))

#Plot ACF and PACF of first and seasonal differences
tsdisplay(diff(diff(data_train),lag=12))

#Doing test using nsdiffs
nsdiffs(data, test = "ocsb") #This is the recommended seasonal difference test
#shows that we should do 1 difference
nsdiffs(data, test = "ch")
#shows that we should do 0 difference

ndiffs(data, test = "adf")
#shows we should do 0 difference
ndiffs(data, test = "pp")
#shows that we should do 0 differecne
ndiffs(data, test = "kpss") #KPSS is the recommended test


#SARIMA (1,1,0)(0,1,0)[12]

fit1 <- Arima(data_train, order = c(1,1,0), seasonal = c(0,1,0))
tsdisplay(residuals(fit1))


#Fitting auto arima
fit2 <- auto.arima(data_train)
fit2$arma

#fitted values of autoarima
fitted(fit2)

summary(fit2)
summary(fit1)


qqnorm(fit1$residuals)
qqline(fit1$residuals)

qqnorm(fit2$residuals)
qqline(fit2$residuals)
