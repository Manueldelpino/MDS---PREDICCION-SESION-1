
# Forecast Models II
# ARIMA Models
#(c)Ricardo A. Queralt
# 

require(forecast)
require(xts)
require(ggplot2)

#read data from CSV file
rawData <- read.csv("C:/Users/Knowhow/Desktop/CUNEF/PREDICCION-FORECASTING/PREDICCION SESION 4/ko.csv", sep=";", dec=",")

#Create a XTS object
xVentas=xts((rawData$Ingresos),order.by=as.POSIXct(strptime(rawData$Fecha,"%Y%m%d")))

#Generate quarterly data
xVentas=to.quarterly(xVentas)

#Transform to zoo data (forecast package)
zVentas=as.zoo(xVentas$xVentas.Close)


#Change name
names(zVentas)="Ventas"


##Plot Serie
autoplot(zVentas)+ylab("Ventas")+ggtitle("Ventas Trimestrales CocaCola")+xlab("Trimestres")

df_new <- data.frame(value = as.vector(zVentas),
                     time = time(zVentas))
ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ylab("Ventas")+ggtitle("Ventas Trimestrales CocaCola")+xlab("Trimestres")





#Log transformation?
zlVentas=log(zVentas)
df_newl <- data.frame(value = as.vector(zlVentas),
                     time = time(zlVentas))
ggplot(df_newl)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ylab("Ventas")+ggtitle("Ventas Trimestrales LOG CocaCola")+xlab("Trimestres")


#Difference
ggtsdisplay(zlVentas)   
ggtsdisplay(diff(zlVentas))
ggtsdisplay(diff(zlVentas,4))
ggtsdisplay(diff(diff(zlVentas,4),1))

#Select number of observation to compare forecast
cOmit=6

#Data Size
nObs=length(zVentas)

#sub_sample
oVentas <- window(zVentas,start=index(zVentas[1]),end=index(zVentas[nObs-cOmit]))

#out sample (real data to forecast performance)
pVentas <- window(zVentas,start=index(zVentas[nObs-cOmit+1]),end=index(zVentas[nObs]))


#ARIMA MODEL
fit1=auto.arima(oVentas,lambda=0)  #Lambda = 0 significa aplicar el Logaritmo
summary(fit1)

#residual analysis
ggtsdisplay(fit1$residuals)        #Estadísticamente son ruido blanco porque no son significativas a la variable.

#box-Ljung Test
Box.test(fit1$residuals,lag=4, fitdf=3, type="Lj")       #Autcorrelacion de orden 4
Box.test(fit1$residuals,lag=8, fitdf=3, type="Lj")       #Autcorrelacion de orden 8
Box.test(fit1$residuals,lag=12, fitdf=3, type="Lj")      #Autcorrelacion de orden 12

fventas.arima=forecast(fit1)

ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ geom_forecast(fventas.arima,alpha=0.4)+ggtitle("ARIMA: Predicción CocaCola")


fventas.arima





