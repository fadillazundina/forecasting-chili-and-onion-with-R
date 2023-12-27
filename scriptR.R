library(readxl)
data <- read_excel(path="D:/Penelitian Harga Pangan/Pasar Manis Fix.xlsx")
data

plot.ts(data$`Cabai Merah`)
which.is.na(data$`Cabai Merah`)

cabai <- data$`Cabai Merah Besar (kg)`
cabai2 <- data$`Cabai Rawit`
bawangM <- data$`Bawang Merah`
bawangP <- data$`Bawang Putih`
cabai
View(data)

#deskriptif statistik
summary(cabai, cabai2, bawangM, bawangP)
var(bawangP)
sapply(data, function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
sapply(data, function(x) sd(x) / mean(x) * 100)
cv <- sd(cabai) / mean(cabai) * 100
cv

#Plot data
par(mfrow=c(2,2))
plot.ts(data$`Cabai Merah Besar (kg)`)
plot.ts(data$`Cabai Rawit`)       
plot.ts(data$`Bawang Merah`)              
plot.ts(data$`Bawang Putih`)

#ACF dan PCF data cabai merah besar
acf(data$`Cabai Merah Besar (kg)`, main='Plot ACF Cabai Merah Besar')
diff1 <- diff(data$`Cabai Merah Besar (kg)`)
plot.ts(diff1, main='Differencing Cabai Merah Besar', xlab='Waktu',
        ylab='Harga (Rp)')
acf(diff1, main='ACF Cabai Merah Besar Differencing')
pacf(diff1, main='PACF Cabai Merah Besar Differencing')
diff2 <- diff(data$`Cabai Merah Besar (kg)`,differences = 2)
plot.ts(diff2)
acf(diff2)
pacf(diff2)

#ACF dan PCF data cabai Rawit
acf(data$`Cabai Rawit`, main='Plot ACF Cabai Rawit')
diff1 <- diff(data$`Cabai Rawit`)
plot.ts(diff1, main='Differencing Cabai Rawit', xlab='Waktu',
        ylab='Harga (Rp)')
acf(diff1, main = 'ACF Cabai Rawit Differencing')
pacf(diff1, main='PACF Cabai Rawit Differencing')
diff2 <- diff(data$`Cabai Rawit`,differences = 2)
plot.ts(diff2)
acf(diff2)
pacf(diff2)

#ACF dan PCF data Bawang merah
acf(data$`Bawang Merah`, main='Plot ACF Bawang Merah')
diff1 <- diff(data$`Bawang Merah`)
plot.ts(diff1, main='Differencing Bawang Merah', xlab='Waktu',
        ylab='Harga (Rp)')
acf(diff1, main= 'ACF Bawang Merah Differencing')
pacf(diff1, main='PACF Bawang Merah Differencing')
diff2 <- diff(data$`Bawang Merah`,differences = 2)
plot.ts(diff2)
acf(diff2)
pacf(diff2)

#ACF dan PCF data Bawang Putih
acf(data$`Bawang Putih`, main='Plot ACF Bawang Putih')
diff1 <- diff(data$`Bawang Putih`)
plot.ts(diff1, main='Differencing Cabai Merah Besar', xlab='Waktu',
        ylab='Harga (Rp)')
acf(diff1, main = 'ACF Bawang Putih Differencing')
pacf(diff1, main='PACF Bawang Putih Differencing')
diff2 <- diff(data$`Bawang Putih`,differences = 2)
plot.ts(diff2)
acf(diff2)
pacf(diff2)

#forecast
library(forecast)

#arima cabai merah besar
fit1 <- arima(data$`Cabai Merah Besar (kg)`, order = c(0,1,0))
fit2 <- arima(data$`Cabai Merah Besar (kg)`, order = c(0,1,1))
fit3 <- arima(data$`Cabai Merah Besar (kg)`, order = c(0,1,2))
fit4 <- arima(data$`Cabai Merah Besar (kg)`, order = c(1,1,0))
fit5 <- arima(data$`Cabai Merah Besar (kg)`, order = c(1,1,1))
fit6 <- arima(data$`Cabai Merah Besar (kg)`, order = c(1,1,2))
fit7 <- arima(data$`Cabai Merah Besar (kg)`, order = c(2,1,0))
fit8 <- arima(data$`Cabai Merah Besar (kg)`, order = c(2,1,1))
fit9 <- arima(data$`Cabai Merah Besar (kg)`, order = c(2,1,2))

#BIC dan AIC
BIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9)
AIC(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9)

#auto arima cabai merah besar
auto.arima(data$`Cabai Merah Besar (kg)`,trace = TRUE)

#arima cabai 
fit11 <- arima(data$`Cabai Rawit`, order = c(0,1,0))
fit21 <- arima(data$`Cabai Rawit`, order = c(2,0,1))
fit31 <- arima(data$`Cabai Merah Besar (kg)`, order = c(0,1,2))
fit41 <- arima(data$`Cabai Merah Besar (kg)`, order = c(1,1,0))
fit51 <- arima(data$`Cabai Merah Besar (kg)`, order = c(1,1,1))
fit61 <- arima(data$`Cabai Merah Besar (kg)`, order = c(1,1,2))
fit71 <- arima(data$`Cabai Merah Besar (kg)`, order = c(2,1,0))
fit81 <- arima(data$`Cabai Merah Besar (kg)`, order = c(2,1,1))
fit91 <- arima(data$`Cabai Merah Besar (kg)`, order = c(2,1,2))

#arima bawang
fit23 <- arima(data$`Bawang Merah`, order = c(0,1,0))
fit24 <- arima(data$`Bawang Putih`, order = c(3,1,0))

#BIC
BIC(fit1.1, fit2.1, fit3.1, fit4.1, fit5.1, fit6.1, fit7.1, fit8.1, fit9.1)

#fit 
fit1 <- arima(data$`Cabai Merah Besar (kg)`, order = c(0,1,0))
fit11 <- arima(data$`Cabai Rawit`, order = c(0,1,0))
fit23 <- arima(data$`Bawang Merah`, order = c(0,1,0))
fit24 <- arima(data$`Bawang Putih`, order = c(3,1,0))

#auto arima
auto.arima(data$`Cabai Merah Besar (kg)`,trace = TRUE)
auto.arima(data$`Cabai Rawit`,trace = TRUE)
auto.arima(data$`Bawang Merah`,trace = TRUE)
auto.arima(data$`Bawang Putih`,trace = TRUE)
#0,1,0
#1,1,0
#0,1,0
#3,1,0
tsdiag(fit1)
tsdiag(fit41)
tsdiag(fit23)
tsdiag(fit24)

tsdiag(fit21)

checkresiduals(fit1)
checkresiduals(fit41)
checkresiduals(fit23)
checkresiduals(fit24)

#Ljung-Box test
Box.test(data$`Cabai Rawit`, type = "Ljung-Box")

forecast1 = forecast(fit1, h=30)
plot(forecast1, main='Forecast Cabai Merah Besar', xlab= 'Deret Waktu',
     ylab='Harga')
forecast2 = forecast(fit41, h=30)
plot(forecast2, main='Forecast Cabai Rawit', xlab= 'Deret Waktu',
     ylab='Harga')
forecast3 = forecast(fit23, h=30)
plot(forecast3, main='Forecast Bawang Merah', xlab= 'Deret Waktu',
     ylab='Harga')
forecast4 = forecast(fit24, h=30)
plot(forecast4, main='Forecast Bawang Putih', xlab= 'Deret Waktu',
     ylab='Harga')

forecast1
forecast2
forecast3
forecast4

predict(fit1, h=7)

#cek akurasi
accuracy(fit1, )

par(mfrow=c(2,2))
plot.ts(data$`Cabai Merah Besar (kg)`, main='Plot Cabai Merah Besar', xlab='Waktu',
        ylab='Harga')
plot.ts(data$`Cabai Rawit`, main='Plot Cabai Rawit', xlab='Waktu',
        ylab='Harga')
plot.ts(data$`Bawang Merah`, main='Plot Bawang Merah', xlab='Waktu',
        ylab='Harga')        
plot.ts(data$`Bawang Putih`, main='Plot Bawang Putih', xlab='Waktu',
        ylab='Harga')

par(mfrow=c(1,1))
acf(diff2)
pacf(diff2)

library(stats)
plot(data$`Cabai Rawit`, type = "h", ylim = c(0, 10), main = "Plot Ljung-Box", 
     xlab = "Lag", ylab = "Q-statistic")
abline(h = data$`Cabai Rawit`, col = "red", lty = 2)

#koefisien
coef(fit1)
coef(fit41)
coef(fit23)
coef(fit24)

#evaluasi
#load Metrics package
library(Metrics)

#prediksi
pred_CMB = fitted.values(fit1)
pred_CR = fitted.values(fit41)
pred_BM = fitted.values(fit23)
pred_BP = fitted.values(fit24)
pred_CR2 = fitted.values(fit21)

#calculate RMSE
rmse(cabai, pred_CMB)
rmse(cabai2, pred_CR)
rmse(bawangM, pred_BM)
rmse(bawangP, pred_BP)
rmse(cabai2, pred_CR2)

#calculate MAPE
mape(cabai, pred_CMB)
mape(cabai2, pred_CR)
mape(bawangM, pred_BM)
mape(bawangP, pred_BP)
mape(cabai2, pred_CR2)

#make a graph
par(mfrow=c(2,2))
plot(pred_CMB, type = "l", col = "red", 
     ylim = range(pred_CMB, cabai), 
     xlab = "Deret Waktu", ylab = "Harga", main='Cabai Merah Besar')
lines(cabai, col = "blue")
legend("topleft", legend = c("Prediksi", "Aktual"), 
       col = c("red", "blue"), lty = 2, cex=0.8)

plot(pred_CR, type = "l", col = "red", 
     ylim = range(pred_CR, cabai2), 
     xlab = "Deret Waktu", ylab = "Harga", main='Cabai Rawit')
lines(cabai2, col = "blue")
legend("topleft", legend = c("Prediksi", "Aktual"), 
       col = c("red", "blue"), lty = 2, cex=0.8)

plot(pred_BM, type = "l", col = "red", 
     ylim = range(pred_BM, bawangM), 
     xlab = "Deret Waktu", ylab = "Harga", main='Bawang Merah')
lines(bawangM, col = "blue")
legend("topleft", legend = c("Prediksi", "Aktual"), 
       col = c("red", "blue"), lty = 2, cex=0.8)

plot(pred_BP, type = "l", col = "red", 
     ylim = range(pred_BP, bawangP), 
     xlab = "Deret Waktu", ylab = "Harga", main='Bawang Putih')
lines(bawangP, col = "blue")
legend("topleft", legend = c("Prediksi", "Aktual"), 
       col = c("red", "blue"), lty = 2, cex=0.8)

plot(pred_CR2, type = "l", col = "red", 
     ylim = range(pred_CR2, cabai2), 
     xlab = "Deret Waktu", ylab = "Harga", main='Cabai Rawit')
lines(cabai2, col = "blue")
legend("topleft", legend = c("Prediksi", "Aktual"), 
       col = c("red", "blue"), lty = 2, cex=0.8)

