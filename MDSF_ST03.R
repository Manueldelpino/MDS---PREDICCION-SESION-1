 
# Forecast Models I
# ETS Models
# Forecast KO sales 
#(c)Ricardo A. Queralt
# 

require(forecast)
require(xts)
require(ggplot2)
library(ggfortify) #Plot Monthplot

#read data from CSV file
rawData <- read.csv("C:/Users/Knowhow/Desktop/CUNEF/PREDICCION-FORECASTING/PREDICCION SESION 4/ko.csv", sep=";", dec=",")

#Create a XTS object
#Convert data to XTS
mData = xts(rawData$Ingresos, order.by = as.Date(as.character(rawData$Fecha),"%Y%m%d"),frequency=4)

xVentas=xts((rawData$Ingresos),order.by=as.POSIXct(strptime(rawData$Fecha,"%Y%m%d")),frequency=4)

#Generate quarterly data
xVentas = to.quarterly(xVentas)

#Transform to zoo data (forecast package)
zVentas =as.zoo(xVentas$xVentas.Close)

#Change name
names(zVentas)="Ventas"


##Plot Serie
autoplot(zVentas)+ggtitle("Ventas Trimestrales CocaCola")+xlab("Trimestres")+ylab("Ventas")
#Seasonal Plot
ggfreqplot(as.ts(zVentas),freq=4,nrow=1,facet.labeller=c("1T","2T","3T","4T"))+ggtitle("Ventas Trimestrales")
#Tiene tendencia alcista estacionaria en los 2T anuales debido a la distribución comercial que se prepara para el verano (periodo de más consumo de CocaCola Refrescos)

#Select number of observation to compare forecast
cOmit=4

#Data Size
nObs=length(zVentas)

#sub_sample
#oVentas=zVentas[1:(nObs-cOmit),]
oVentas <- window(zVentas,start=index(zVentas[1]),end=index(zVentas[nObs-cOmit]))

#Fit Simple Exponential Smoothing
fit1 <- ses(oVentas)

#Fit Holt
fit2 <- holt(oVentas)

#Fit Holt- exponential
fit3 <- holt(oVentas,exponential=TRUE)

#Fit Holt - damped
fit4 <- holt(oVentas,damped=TRUE)

#Fit Holt - (exponential+damped)
fit5 <- holt(oVentas,exponential=TRUE,damped=TRUE)

# Results for first model:
fit1$model

#Plot models fitted
plot(fit3, type="o", ylab="Ventas",  flwd=1, plot.conf=FALSE)   #Muy buen gráfico para estimar - predecir
lines(window(zVentas),type="o")
lines(fit1$mean,col=2)
lines(fit2$mean,col=3)
lines(fit4$mean,col=5)
lines(fit5$mean,col=6)
legend("topleft", lty=1, pch=1, col=1:6,
       c("Data","SES","Holt's","Exponential",
         "Additive Damped","Multiplicative Damped"))



#seasonal model Holt-winters
fit6 <- hw(oVentas,seasonal="additive")
fit7 <- hw(oVentas,seasonal="multiplicative")

#Plot models
plot(fit7,ylab="Ventas",
     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(window(zVentas),type="o",col="blue")
lines(fitted(fit6), col="red", lty=2)
lines(fitted(fit7), col="green", lty=2)
lines(fit6$mean, type="o", col="red")
lines(fit7$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))



#Calculate Components
states <- cbind(fit6$model$states[,1:3],fit7$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab="Year")
fit6$model$state[,1:3]
fitted(fit6)
fit6$mean

 
## Select automatic ETS              #ERROR TENDENCIA STACIONARY  (ETS)                    
etsfit<-ets(oVentas)
#forecast model
fventas.ets=forecast(etsfit)
#Results
summary(fventas.ets)

#Plot
plot(fventas.ets)
lines(window(zVentas),type="o")

#Actual and Forecast
matrix(c(fventas.ets$mean[1:cOmit],zVentas[(nObs-cOmit+1):nObs]),ncol=2)

#[,1]  [,2]
#[1,] 12094.71 11976    PREDICCION
#[2,] 11195.27 10872    REAL
#[3,] 10725.07 10711
#[4,] 12985.83 12156


## Select automatic ETS
etsfit2<-ets(oVentas,damped=TRUE)
#forecast model
fventas.ets2=forecast(etsfit2)
#Results
summary(fventas.ets2)

#Plot
plot(fventas.ets2)
lines(window(zVentas),type="o")

#Actual and Forecast
matrix(c(fventas.ets2$mean[1:cOmit],fventas.ets$mean[1:cOmit],zVentas[(nObs-cOmit+1):nObs]),ncol=3)

#Plot all models
plot(fventas.ets2)
lines(window(zVentas),type="o")
lines(fventas.ets$mean,type="o",col="red")

model2 =ets(zVentas,damped = TRUE)
f.model2 =forecast(model2)
plot(f.model2)

