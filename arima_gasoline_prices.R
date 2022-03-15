library(readxl)
library(tseries)
library(forecast)
library(TSA)
library(fitdistrplus)
library(zoo)
library(car)
library(nortest)

data <- read_xls('PET_PRI_GND_DCUS_NUS_W.xls', sheet = 'Data 1', range = 'A3:B1625')
data <- data.frame(Date = data[[1]], Gasoline_Prices = data[[2]])
data <- na.omit(data)
data <- data[data$Date >= '2010-01-04' & data$Date <= '2015-01-05', ]

ts <- ts(data$Gasoline_Prices, start = c(2010, 1), frequency = 52)

plot(ts, ylab = 'Gasoline Prices', 
     main = 'Weekly U.S. All Grades All Formulations Retail Gasoline Prices (Dollars per Gallon)')
abline(v = (seq(2010, 2015, by = 1)), col = 'darkgray', lty = 'dotted')
abline(h = (seq(2.5, 4, by = 0.5)), col = 'darkgray', lty = 'dotted')
abline(reg = lm(ts ~ time(ts)), col = 'darkblue', lw = 2)
summary(lm(ts ~ time(ts)))
boxplot(ts ~ cycle(ts), xlab = 'Week', ylab = '', 
        main = 'Gasoline Prices Boxplot')
adf.test(ts)

dts <- diff(ts)

plot(dts, ylab = 'Gasoline Price Differences', 
     main = 'Weekly U.S. All Grades All Formulations Retail Gasoline Prices (Dollars per Gallon)')
abline(v = (seq(2010, 2015, by = 1)), col = 'darkgray', lty = 'dotted')
abline(h = (seq(-0.15, 0.15, by = 0.05)), col = 'darkgray', lty = 'dotted')
abline(reg = lm(dts ~ time(dts)), col = 'darkblue')
boxplot(dts ~ cycle(dts), xlab = 'Week', ylab = '', 
        main = 'Gasoline Price Differences Boxplot')
adf.test(dts)

par(mfrow=c(1,2))
acf(dts, lag.max = 110, main = 'ACF')
pacf(dts, lag.max = 110, main = 'PACF')
par(mfrow=c(1,1))

auto.arima(dts, approximation = FALSE, trace = TRUE,
           max.p = 4, max.q = 4, max.d = 0, max.D = 1)

FORECAST_RANGE <- 12

# ARIMA(1,0,0)(1,0,0)[52]
m1 <- arima(dts, order = c(1,0,0), 
            seasonal = list(order = c(1,0,0), period = 52), method = 'ML')
f1 <- predict(m1, n.ahead = FORECAST_RANGE)
lci <- f1$pred - qnorm(0.025,0,1)*f1$se
uci <- f1$pred + qnorm(0.025,0,1)*f1$se

plot_diff1 <- data.frame(Original_data=c(dts, rep(NA, FORECAST_RANGE)), 
                        Prediction = c(rep(NA, length(dts)), f1$pred), 
                        Upper_CI = c(rep(NA, length(dts)), uci), 
                        Lower_CI = c(rep(NA, length(dts)), lci))
matplot(plot_diff1, type = 'l', pch = 20, lty = 1,
        col = c('black', 'red', 'blue', 'blue'),
        lwd = 2, xlab = 'Time', ylab = 'Values',
        main = 'Gasoline price differences prediction')
legend('bottomleft', c('Original Data', 'Prediction', '95% Confidence Interval'),
       col = c('black', 'red', 'blue'), lwd = 2, cex = 0.75)

first_value <- ts[1]
last_value <- ts[length(ts)]
plot_orig1 <- data.frame(Original_data = c(diffinv(dts, lag = 1, differences = 1, first_value), 
                                           rep(NA, FORECAST_RANGE)), 
                        Prediction = c(rep(NA, length(ts)), 
                                       diffinv(f1$pred, lag = 1, differences = 1, last_value)[2:(FORECAST_RANGE+1)]), 
                        Upper_CI = c(rep(NA, length(ts)), 
                                     diffinv(uci, lag = 1, differences = 1, last_value)[2:(FORECAST_RANGE+1)]), 
                        Lower_CI = c(rep(NA, length(ts)), 
                                     diffinv(lci, lag = 1, differences = 1, last_value)[2:(FORECAST_RANGE+1)]))
matplot(plot_orig1, type = 'l', pch = 20, lty = 1, 
        col = c('black', 'red', 'blue', 'blue'),
        lwd = 2, xlab = 'Time', ylab = 'Values',
        main = 'Gasoline prices prediction')
legend('bottomleft', c('Original Data', 'Prediction', '95% Confidence Interval'),
       col = c('black', 'red', 'blue'), lwd = 2)

# ARIMA(4,0,0)(1,0,0)[52]
m2 <- arima(dts, order = c(4,0,0), 
            seasonal = list(order = c(1,0,0), period = 52), method = 'ML')
f2 <- predict(m1, n.ahead = FORECAST_RANGE)
lci <- f2$pred - qnorm(0.025,0,1)*f2$se
uci <- f2$pred + qnorm(0.025,0,1)*f2$se

plot_diff2 <- data.frame(Original_data=c(dts, rep(NA, FORECAST_RANGE)), 
                        Prediction = c(rep(NA, length(dts)), f2$pred), 
                        Upper_CI = c(rep(NA, length(dts)), uci), 
                        Lower_CI = c(rep(NA, length(dts)), lci))
matplot(plot_diff2, type = 'l', pch = 20, lty = 1,
        col = c('black', 'red', 'blue', 'blue'),
        lwd = 2, xlab = 'Time', ylab = 'Values',
        main = 'Gasoline price differences prediction')
legend('bottomleft', c('Original Data', 'Prediction', '95% Confidence Interval'),
       col = c('black', 'red', 'blue'), lwd = 2, cex = 0.75)

first_value <- ts[1]
last_value <- ts[length(ts)]
plot_orig2 <- data.frame(Original_data = c(diffinv(dts, lag = 1, differences = 1, first_value), 
                                           rep(NA, FORECAST_RANGE)), 
                        Prediction = c(rep(NA, length(ts)), 
                                       diffinv(f2$pred, lag = 1, differences = 1, last_value)[2:(FORECAST_RANGE+1)]), 
                        Upper_CI = c(rep(NA, length(ts)), 
                                     diffinv(uci, lag = 1, differences = 1, last_value)[2:(FORECAST_RANGE+1)]), 
                        Lower_CI = c(rep(NA, length(ts)), 
                                     diffinv(lci, lag = 1, differences = 1, last_value)[2:(FORECAST_RANGE+1)]))
matplot(plot_orig1, type = 'l', pch = 20, lty = 1, 
        col = c('black', 'red', 'blue', 'blue'),
        lwd = 2, xlab = 'Time', ylab = 'Values',
        main = 'Gasoline prices prediction')
legend('bottomleft', c('Original Data', 'Prediction', '95% Confidence Interval'),
       col = c('black', 'red', 'blue'), lwd = 2)

# Model selection
c(AIC(m1), AIC(m2)) # select m1

# Analysis of residuals
res <- as.vector(m1$residuals)
plot(res, ylab = 'Residuals', type = 'b')

hist(res, freq = FALSE)
x <- seq(-0.15, 0.15, length.out = 1000)
fd_norm <- fitdist(res, 'norm')
a <- fd_norm$estimate[1]
sigma <- fd_norm$estimate[2]
lines(x, 1/( sigma*sqrt(2*pi) )*exp( -0.5*( ( ( x-a )/sigma)^2 ) ), 
      col='red', lwd = 2, type = 'l')
qqPlot(res, ylab = 'sample quantiles')

RATIO <- 0.7
tr_index <- sample(1:length(res), length(res)*RATIO)
train_res <- res[tr_index]
test_res <- res[-tr_index]
fd_train <- fitdist(train_res, 'norm')
ks.test(test_res, 'pnorm', fd_train$estimate[1], fd_train$estimate[2])

par(mfrow=c(1,2))
acf(res, lag.max = 110, main = 'ACF')
pacf(res, lag.max = 110, main = 'PACF')
par(mfrow=c(1,1))

Box.test(res, lag = 52, type = 'Ljung-Box', fitdf = length(coef(m1)))

k3 <- kernel('daniell', c(5,5,5))
s_res <- spec(res, kernel = k3, log = 'no', sub = '', plot = FALSE)
s <- spec(dts, kernel = k3, log = 'no', sub = '', col = 'blue', lwd = 2,
     xlab = 'Frequency', ylab = 'Smoothed Sample Spectral Density',
     main = 'Smoothed Periodogram', ylim=)
lines(s_res$freq, s_res$spec, col = 'green', lwd = 2)
legend('topright', c('Initial series', 'Residuals'), lwd = 2, 
       col = c('blue', 'green'))

# Holt-Winters model
m3 <- HoltWinters(ts, seasonal = 'multiplicative')
f3 <- predict(m3, n.ahead = 12)
plot(m3, predicted.values = f3, lwd = 2)

m4 <- HoltWinters(ts, seasonal = 'additive')
f4 <- predict(m4, n.ahead = 12)
plot(m4, predicted.values = f4, lwd = 2)
