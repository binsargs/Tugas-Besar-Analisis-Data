library(forecast)
library(graphics)
library(tseries)
library(ggplot2)
library(readr)
library(psych)
library(PerformanceAnalytics)

#INPUT DATA
setwd("C:/Users/Binsar G S/OneDrive - Institut Teknologi Bandung/Semester 3/Andat/TUBES/")
ASIANPAINT <- read_csv("ASIANPAINT.csv")
datafull <- ASIANPAINT
# Mengambil baris dan kolom yang diperlukan
# Baris yang diambil adalah baris 4236 sampai 5306 yang merupakan stock market 2 Jan 2017 hingga 30 April 2021
# Kolom yang diambil sebagai contoh adalah "Date", "Open", "High", "Low", "Close", "Volume"
ASIANPAINT <- ASIANPAINT[c(4236:5306),c(1,5,6,7,9,10,11)]

# Melihat cuplikan data
ASIANPAINT[1:16,]

# Mengelompokkan data per tahun
data_2017 = as.numeric(unlist(ASIANPAINT[1:248,"VWAP"]))
data_2018 = as.numeric(unlist(ASIANPAINT[249:494,"VWAP"]))
data_2019 = as.numeric(unlist(ASIANPAINT[495:739, "VWAP"]))
data_2020 = as.numeric(unlist(ASIANPAINT[740:991,"VWAP"]))
data_2021 = as.numeric(unlist(ASIANPAINT[992:1071,"VWAP"]))

# Sebagian data 2020 dan 2021 untuk kasus sederhana
data_202021 <- as.numeric(unlist(ASIANPAINT[740:1071,"VWAP"]))
tanggal <- ASIANPAINT$Date[740:1071]
plot(tanggal,data_202021, main="Grafik VWAP Saham ASIANPAINT 2020-2021",xlab="Tahun", ylab="VWAP (Rupee)", type='solid')


# Statistik Deskriptif Data
describe(ASIANPAINT$VWAP) # Tahun 2017-2021
summary(ASIANPAINT$VWAP)
describe(data_2017)       # Tahun 2017
summary(data_2017)
describe(data_2018)       # Tahun 2018
summary(data_2018)
describe(data_2019)       # Tahun 2019
summary(data_2019)
describe(data_2020)       # Tahun 2020
summary(data_2020)
describe(data_2021)       # Tahun 2021
summary(data_2021)

# Boxplot data 2017-2021
boxplot(ASIANPAINT$VWAP,main="Boxplot VWAP Saham ASIANPAINT Januari 2017 - April 2021",ylab="VWAP")
pencilan = boxplot(ASIANPAINT$VWAP, plot=FALSE)$out # Data pencilan
paste("Terdapat sejumlah", length(pencilan), "data pencilan sebagai berikut: ")
(pencilan) # Data pencilan

# Mengecek kenormalan data
qqnorm(ASIANPAINT$VWAP)
qqline(ASIANPAINT$VWAP)

# Boxplot data per tahun
boxplot2017 = boxplot(data_2017, xlab="2017", main="Boxplot VWAP Saham ASIANPAINT 2017")
boxplot2018 = boxplot(data_2018, xlab="2018", main="Boxplot VWAP Saham ASIANPAINT 2018")
boxplot2019 = boxplot(data_2019, xlab="2019", main="Boxplot VWAP Saham ASIANPAINT 2019")
boxplot2020 = boxplot(data_2020, xlab="2020", main="Boxplot VWAP Saham ASIANPAINT 2020")
boxplot2021 = boxplot(data_2021, xlab="2021", main="Boxplot VWAP Saham ASIANPAINT Januari-April 2021")
boxplot(data_2017,data_2018,data_2019,data_2020,data_2021, main="Perbandingan Boxplot VWAP Saham ASIANPAINT Januari 2017 - April 2021",ylab="VWAP", 
        xlab=('2017                      2018                      2019                      2020                      2021'))

# Melihat grafik harga Open
open <- ts(ASIANPAINT$Open)
plot(ASIANPAINT$Date,open, main="Grafik Open Saham ASIANPAINT",xlab="Tahun", ylab="Harga (Rupee)", type='solid')

# Mengubah Data menjadi time series dan menggunakan kolom "VWAP" untuk dianalisis
data <- ts(ASIANPAINT$VWAP)

# Plotting data Harga VWAP
plot(ASIANPAINT$Date,data, main="Grafik VWAP Saham ASIANPAINT",xlab="Tahun", ylab="VWAP (Rupee)", type='solid')

# Mengecek kestationeran data
acf(data, main="Grafik ACF Data")
adf.test(data)
pacf(data, main="Grafik PACF Data" )
# ---- Data tidak stasioner ----

# Transformasi data menggunakan logaritma natural karena terdapat 15 data pencilan
data_log = log(data)
plot(ASIANPAINT$Date,data_log, main="Grafik VWAP Saham ASIANPAINT (Transformasi ln)",xlab="Tahun", ylab="VWAP (Rupee)", type='solid')
boxplot(data_log, main="Boxplot Transformasi Logaritma 'VWAP' Saham ASIANPAINT Januari 2017 - April 2021",ylab="VWAP")

# Membentuk diferensiasi pertama dari data yang telah di transformasi
data_dif_log = diff(data_log)
ts_datadiflog = ts(data_dif_log)
date = datafull$Date[4237:5306]
plot(date,ts_datadiflog, main="Grafik Data_dif_log", xlab="Tahun", ylab="ln(VWAP)",type="solid")

# Mengecek kenormalan data transformasi
qqnorm(data_log)
qqline(data_log)

# Mengecek kestationeran data yang telah di transformasi dan diferensialkan
acf(data_dif_log, main="Grafik ACF Data_dif_log" )
adf.test(data_dif_log)
pacf(data_dif_log, main="Grafik PACF Data_dif_log" )

# Model yang mungkin

# 1. ACF Cut off lag ke-1 dan PACF Gelombang sinus -> ARIMA(1,1,0)
model_ari = arima(data_log,order = c(1,1,0))
summary(model_ari)
checkresiduals(model_ari)

# 2. ACF gelombang sinus dan PACF cut off lag ke-1 -> ARIMA(0,1,1)
model_ima = arima(data_log, order = c(0,1,1))
summary(model_ima)
checkresiduals(model_ima)

# 3. Menggabungkan (1) dan (2) -> ARIMA (1,1,1)
model_arima = arima(data_log, order = c(1,1,1))
summary(model_arima)
checkresiduals(model_arima)

# 4. Model otomatis
model_auto = auto.arima(data_log)
summary(model_auto)
checkresiduals(model_auto)

# Forecast untuk 7 hari ke depan, misalkan pake model auto
(prediksi = forecast(model_auto, h = 7))
date2 = datafull$Date[4237:5314]
plot(prediksi,main="Grafik VWAP Saham ASIANPAINT", ylab="ln(VWAP)", xlab="Hari",type='solid')

# Mengubah hasil forecast menjadi data frame
df <- data.frame(prediksi)
forecast <- df$Point.Forecast # Hasil Forecast Moderat (rata-rata)
dataforecastfull = c(ASIANPAINT$VWAP, exp(forecast)) # Menggabungkan data asli dan forecast
lo95 <- df$Lo.95 # Batas bawah SK 95%
hi95 <- df$Hi.95 # Batas atas SK 95%

# Mengaplikasikan antilog ke data hasil forecast
(hasil_prediksi = exp(forecast)) #moderat
(pesimis = exp(lo95)) # pesimis
(optimis = exp(hi95)) # optimis

# Plot setelah diforecast menggunakan hasil forecasting moderat
plot(date2,dataforecastfull,main="Grafik VWAP Saham ASIANPAINT", ylab="VWAP (Rupee)", xlab="Tahun",type='solid')
