rm()       
ls()       
gc()
rm(list = ls())

#CREACIÓN DE ACUMULADOS DE COVID NACIONAL
library(readr)    #Importación y lectura de CSV (read_csv())
library(readxl)   #Lectura de archivos de excel
library(tidyverse)
library(plyr)#libreria para ddply, realiza sumarizaciones
library(ggplot2)#paquete especializado para visualizaciÓn de datos avanzados
library(janitor)#tablas de frecuencias
library(deSolve)
library(easynls)
library(forecast)
library(tseries)

#library(kinfitr)
library(knitr)
library(nls.multstart)
library(nlme)
library(hrbrthemes)
library(broom)
library(viridis)
library(brms)

#Leyendo archivo de COVID
#covid_data <- read_csv("C:/COVID/ENT/00NACIONAL/01_ENERO_2021.csv")
covid_data <- read_csv("C:/COVID/ENT/06COL/2020/YEAR2020_colima.csv")
colnames(covid_data)
summary(covid_data)
#---------------------------------- TOTAL DE CASOS DE INFECCION ----------------------------------------------
#CLAVE	CLASIFICACIÓN
#1	CASO DE COVID-19 CONFIRMADO POR ASOCIACIÓN CLÍNICA EPIDEMIOLÓGICA
#2	CASO DE COVID-19 CONFIRMADO POR COMIT� DE  DICTAMINACI�N
#3	CASO DE SARS-COV-2  CONFIRMADO POR LABORATORIO
#4	INV�LIDO POR LABORATORIO
#5	NO REALIZADO POR LABORATORIO
#6	CASO SOSPECHOSO
#7	NEGATIVO A SARS-COV-2 POR LABORATORIO
table(covid_data$CLASIFICACION_FINAL)
#   1        4        5        6        7 
#42927611  4980906  1072894 27802907 50857702 

#CLAVE	  RESULTADO_LAB
# 1  	POSITIVO A SARS-COV-2
# 2  	NO POSITIVO A SARS-COV-2
# 3 	RESULTADO PENDIENTE
# 4 	RESULTADO NO ADECUADO 
#97 	NO APLICA (CASO SIN MUESTRA)
table(covid_data$RESULTADO_LAB)
#   1        2        3        4       97 
#42927611 50857702  1072894  4980906 27802907 

#+-+-+-+-+-+-+-+-+-+- TOTALES -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
TOTALES<-as.data.frame(covid_data %>% 
                         filter(FECHA!=0))
colnames(TOTALES)
TOTALES_FECHA<-ddply(TOTALES, .(FECHA), nrow)
colnames(TOTALES_FECHA)
names(TOTALES_FECHA)[2]="TOTALES"
summary(TOTALES_FECHA)

TOTALES_FECHA[1,1]<-"2020-04-19"
TOTALES_FECHA[2,1]<-"2020-04-20"
TOTALES_FECHA[3,1]<-"2020-04-21"
TOTALES_FECHA[4,1]<-"2020-04-22"
TOTALES_FECHA[5,1]<-"2020-04-23"
TOTALES_FECHA[6,1]<-"2020-04-24"
TOTALES_FECHA[7,1]<-"2020-04-25"
TOTALES_FECHA[8,1]<-"2020-04-26"
TOTALES_FECHA[9,1]<-"2020-04-27"
TOTALES_FECHA[10,1]<-"2020-04-28"
TOTALES_FECHA[11,1]<-"2020-04-29"
TOTALES_FECHA[12,1]<-"2020-04-30"

#OBTENIENDO LOS CARACTERES CORRECTOS DE LA FECHA
TOTALES_FECHA$DIA<- ifelse(2020 == substr(TOTALES_FECHA$FECHA,1,4),
                      (substr(TOTALES_FECHA$FECHA,9,10)),
                      (substr(TOTALES_FECHA$FECHA,1,2 )))

TOTALES_FECHA$MES<- ifelse(2020 == substr(TOTALES_FECHA$FECHA,1,4),
                      (substr(TOTALES_FECHA$FECHA,6,7)),
                      (substr(TOTALES_FECHA$FECHA,4,5 )))

TOTALES_FECHA$FECHA_limpia<- str_c("2020-",TOTALES_FECHA$MES,"-",TOTALES_FECHA$DIA)
as.Date(TOTALES_FECHA$FECHA_limpia, format="%yyyy-%mm-%dd")
summary(TOTALES_FECHA$FECHA_limpia)
colnames(TOTALES_FECHA)

TOTALES_FECHA2<-data.frame(TOTALES_FECHA$FECHA_limpia,TOTALES_FECHA$TOTALES)
names(TOTALES_FECHA2)[1]="FECHA"
names(TOTALES_FECHA2)[2]="TOTALES"

#+-+-+-+-+-+-+-+-+-+- INFECTADAS +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
INFECTADOS<-as.data.frame(covid_data %>% 
                            filter(RESULTADO_LAB==1 & (CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL== 2|CLASIFICACION_FINAL==3)))
colnames(INFECTADOS)
INFECTADOS_FECHA<-ddply(INFECTADOS, .(FECHA), nrow)
colnames(INFECTADOS_FECHA)
names(INFECTADOS_FECHA)[2]="TOTAL_INF"
summary(INFECTADOS_FECHA)

INFECTADOS_FECHA[1,1]<-"2020-04-19"
INFECTADOS_FECHA[2,1]<-"2020-04-20"
INFECTADOS_FECHA[3,1]<-"2020-04-21"
INFECTADOS_FECHA[4,1]<-"2020-04-22"
INFECTADOS_FECHA[5,1]<-"2020-04-23"
INFECTADOS_FECHA[6,1]<-"2020-04-24"
INFECTADOS_FECHA[7,1]<-"2020-04-25"
INFECTADOS_FECHA[8,1]<-"2020-04-26"
INFECTADOS_FECHA[9,1]<-"2020-04-27"
INFECTADOS_FECHA[10,1]<-"2020-04-28"
INFECTADOS_FECHA[11,1]<-"2020-04-29"
INFECTADOS_FECHA[12,1]<-"2020-04-30"

#+-+-+-+-+-+-+-+-+-+- FALLECIDAS +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#ACUMULAR LAS FECHAS POR DíA
FALLECIDOS <-as.data.frame(covid_data %>% 
                             filter(FECHA_DEF!=0)) #colnames(FALLECIDOS)
FALLECIDOS_FECHA<-ddply(FALLECIDOS, .(FECHA), nrow)
colnames(FALLECIDOS_FECHA)
names(FALLECIDOS_FECHA)[2] = "TOTAL_DEF"

#+-+-+-+-+-+-+-+-+-+- REGISTROS TOTALES DE PERSONAS RECUPERADAS +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
SINTOMAS<-data.frame(table(covid_data$FECHA_SINTOMAS))
colnames(SINTOMAS)
names(SINTOMAS)[1] = "FECHA"
names(SINTOMAS)[2] = "SINTOMAS_FECHA"
#summary(SINTOMAS)

#OBTENIENDO LOS CARACTERES CORRECTOS DE LA FECHA
SINTOMAS$DIA<- ifelse(2020 == substr(SINTOMAS$FECHA,1,4),
                    (substr(SINTOMAS$FECHA,9,10)),
                    (substr(SINTOMAS$FECHA,1,2 )))

SINTOMAS$MES<- ifelse(2020 == substr(SINTOMAS$FECHA,1,4),
                    (substr(SINTOMAS$FECHA,6,7)),
                    (substr(SINTOMAS$FECHA,4,5 )))

#CONCATENANDO LA FECHA
SINTOMAS$FECHA_limpia<- str_c("2020-",SINTOMAS$MES,"-",SINTOMAS$DIA)
as.Date(SINTOMAS$FECHA_limpia, format="%yyyy-%mm-%dd")
summary(SINTOMAS$FECHA_limpia)

#AGRUPANDO LOS DATOS
SINTOMAS_FECHA <- ddply(SINTOMAS, .(FECHA_limpia), nrow)
colnames(SINTOMAS_FECHA)
names(SINTOMAS_FECHA)[1]="FECHA"
names(SINTOMAS_FECHA)[2]="TOTAL_SINTOMAS"

#+-+-+-+-+-+-+-+-+-+- COMBINACIÓN +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
covid2020ALL<-merge(x = TOTALES_FECHA2, y = INFECTADOS_FECHA, all = TRUE)
covid2020ALL<-merge(x = covid2020ALL, y = FALLECIDOS_FECHA, all = TRUE)
covid2020ALL$TOTAL_DEF<-replace(covid2020ALL$TOTAL_DEF, is.na(covid2020ALL$TOTAL_DEF), 0)
covid2020ALL<-merge(x = covid2020ALL, y = SINTOMAS_FECHA, all = TRUE)
covid2020ALL$TOTALES<-replace(covid2020ALL$TOTALES, is.na(covid2020ALL$TOTALES), 0)
covid2020ALL$TOTAL_INF<-replace(covid2020ALL$TOTAL_INF, is.na(covid2020ALL$TOTAL_INF), 0)
covid2020ALL$TOTAL_DEF<-replace(covid2020ALL$TOTAL_DEF, is.na(covid2020ALL$TOTAL_DEF), 0)
covid2020ALL$TOTAL_SINTOMAS<-replace(covid2020ALL$TOTAL_SINTOMAS, is.na(covid2020ALL$TOTAL_SINTOMAS), 0)
colnames(covid2020ALL)

#CALCULANDO SOSPECHOSOS
#TOTAL_REGISTRADOS = SOSPECHOSOS + INFECTADOS + RECUPERADOS(FALLECIDOS)
covid2020ALL$TOTAL_SOS <- covid2020ALL$TOTALES-covid2020ALL$TOTAL_INF-covid2020ALL$TOTAL_DEF


#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
rm(FALLECIDOS,FALLECIDOS_FECHA,INFECTADOS,INFECTADOS_FECHA,SINTOMAS)
rm(SINTOMAS_FECHA,TOTALES,TOTALES_FECHA,TOTALES_FECHA2)
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#Graficando los datos

dataToFit <- tibble(Time = c(covid2020ALL$FECHA), 
                    Counts = c(covid2020ALL$TOTALES))
table(dataToFit)
ggplot(dataToFit, aes(x=Time,y=Counts)) + xlab('Días Registrados') + ylab('No. Casos Acumulados') +
  geom_point()
#write.csv(dataToFit,"C:/COVID/ENT/00NACIONAL/dataToFit.csv",row.names=FALSE)

library(tseries)
library(forecast)

serie <- ts(c(covid2020ALL$TOTALES),frequency = 1)
plot(serie,xlab="Días registrados", ylab="Datos Acumulados", main="Total de Casos Registrados Año 2020")

#DEscomposición de la serie de tiempo
count_ma = ts(na.omit(covid2020ALL$TOTALES), frequency=30)
# serie <- ts(c(covid2020ALL$TOTALES),frequency = 1)
decomp = stl(count_ma, s.window="periodic")
estacionalidad_TOTALES <- seasadj(decomp)
plot(decomp, main = "CASOS REGISTRADOS DEL AÑO 2020")

serieINF <- ts(c(covid2020ALL$TOTAL_INF),frequency = 1)
plot(serieINF, xlab="Días registrados", ylab="Datos Infectados", main="Total de Infectados")

count_ma = ts(na.omit(covid2020ALL$TOTAL_INF), frequency=30)
# serie <- ts(c(covid2020ALL$TOTALES),frequency = 1)
decomp = stl(count_ma, s.window="periodic")
estacionalidad_TOTALESINF <- seasadj(decomp)
plot(decomp,main = "CASOS DE INFECCIÓN DEL AÑO 2020")

#AGREGANDO UN ÍNDICE A LA FECHA
covid2020ALL <- covid2020ALL %>% dplyr::mutate(ID = row_number())
head(covid2020ALL)

serieINF <- ts(c(covid2020ALL$TOTAL_INF),frequency = 1)
plot(serieINF, main="Total de Infectados", xlab="Días registrados", ylab="Datos Infectados")

PENDIENTE <- lm(covid2020ALL$TOTAL_INF~covid2020ALL$ID)
PENDIENTE 
summary(PENDIENTE)
abline(PENDIENTE, lty=2)
PENDIENTE[[3]]
DistanciaPendiente <-unlist(PENDIENTE[[3]])
head(DistanciaPendiente)#(effects)
length(DistanciaPendiente)
covid2020ALL<-cbind(covid2020ALL,DistanciaPendiente)

#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
library(fpp2)

data_serie <- ts(covid2020ALL$TOTAL_INF, frequency=1, start=1)#frecuencia=7 por semana, 30 por mes, 1 por día

autoplot(data_serie)+
  labs(title = "Serie de tiempo INFECTADOS",       
       x = "Fecha/Día",
       y = "Valor acumulado",
       colour = "#00a0dc")+
  theme_bw() 

# Descomposición de la serie de tiempo. Se almacena en el objeto fit
fit <- decompose(data_serie, type='additive')
#fit <- decompose(data_serie, type='multiplicative')

# Para graficar esta descomposición volvemos a emplear la funcion autoplot, pero con el objeto fit
autoplot(fit)+
  labs(title = "Descomposición de la serie de tiempo",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+
  theme_bw()

#gráfico de la serie de tiempo con su tendencia
autoplot(data_serie, series="Serie tiempo") + 
  autolayer(trendcycle(fit), series="Tendencia") +
  labs(title = "Serie de tiempo",      
       x = "Tiempo",
       y = "Valor"
  ) + 
  theme_bw()

ggseasonplot(data_serie, ylab="Estacionalidad", xlab="Días")

#ggseasonplot(data_serie, ylab="Estacionalidad", xlab="Días") +
#  ggtitle("Gráfica estacional")

#ggseasonplot(data_serie, polar=TRUE) +
#  ylab("$ Estacionalidad") +
#  ggtitle("Gráfica estacional Polar")

#GRÁFICOS DE SUBSERIES ESTACIONALES
#ggsubseriesplot(data_serie) +
#  ylab("Estacionalidad") +
#  ggtitle(" Gráficas de subseires estaxionalesSeasonal subseries plot: antidiabetic drug sales")

# + + + + + + ++ + CORRELACIONES + + + + + + + + + + + + + + + +
#Diagramas de dispersión
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")
qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")

qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")

GGally::ggpairs(as.data.frame(visnights[,1:5]))

#################  https://otexts.com/fpp2/scatterplots.html

#+ + + Matrices de diagramas de dispersión + + + 
#autoplot(visnights[,1:5], facets=TRUE) +
#  ylab("Number of visitor nights each quarter (millions)")
#GGally::ggpairs(as.data.frame(visnights[,1:5]))

#+++++++++++++ GRÁFICAS DE RETRASO +++++++++++++
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

#AUTOCORRELACIÓN
ggAcf(beer2)

#Tendencia y estacionalidad en parcelas ACF
aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")
ggAcf(aelec, lag=48) 

#Ruido blanco
#Las series de tiempo que no muestran autocorrelación se denominan ruido blanco.
#La Figura 2.17 da un ejemplo de una serie de ruido blanco.

#set.seed(30)
#y <- ts(rnorm(50))
#autoplot(y) + ggtitle("White noise")
#ggAcf(y)
#Para las series de ruido blanco, esperamos que cada autocorrelación sea cercana a cero.
#Por supuesto, no serán exactamente iguales a cero ya que existe alguna variación aleatoria.
#Para una serie de ruido blanco, esperamos que el 95% de los picos en el ACF se encuentren
#dentro de ±2/√T, dónde T es la longitud de la serie temporal.

#+++++++++++++++++++++++++++++++++ PRONÓSTICO ++++++++++++++++++++++++++++++++++++++++++++
#pronóstico por método simple

#------------------ snaive(), metodo naive considerando estacionalidad ------------------
m1 <- snaive(data_serie, h=1) #h=cantidad de datos que deseamos pronosticar
# graficando el pronóstico
autoplot(m1)
#Finalmente, para verificar el ajuste del método podemos emplear las siguientes funciones:
#fitted(), obtiene un ajuste con la data historica
#checkresiduals(), permite analizar los residuales
# verificando el ajuste del método
autoplot(m1)+autolayer(fitted(m1), series="Ajuste")
# verificando los residuales
checkresiduals(m1)
#ESTIMACIÓN DEL ERROR modelo en base a métodos simples
real <- data.frame(data_serie)
data_real <- ts(real,start = 1)
accuracy(m1,data_real)

#------------------ ses(), exponential smoothing ------------------
mS <- ses(data_serie, h=30) #h=cantidad de datos que deseamos pronosticar
# graficando el pronóstico
autoplot(mS)
#Finalmente, para verificar el ajuste del método podemos emplear las siguientes funciones:
#fitted(), obtiene un ajuste con la data historica
#checkresiduals(), permite analizar los residuales
# verificando el ajuste del método
autoplot(mS)+autolayer(fitted(mS), series="Ajuste")
# verificando los residuales
checkresiduals(mS)
#ESTIMACIÓN DEL ERROR modelo en base a métodos simples
real <- data.frame(data_serie)
data_real <- ts(real,start = 1)
accuracy(mS,data_real)


#------------------ meanf(), media movil ------------------
mf <- ses(data_serie, h=7) #h=cantidad de datos que deseamos pronosticar
# graficando el pronóstico
autoplot(mf)
#Finalmente, para verificar el ajuste del método podemos emplear las siguientes funciones:
#fitted(), obtiene un ajuste con la data historica
#checkresiduals(), permite analizar los residuales
# verificando el ajuste del método
autoplot(mf)+autolayer(fitted(mf), series="Ajuste")
# verificando los residuales
checkresiduals(mf)
#ESTIMACIÓN DEL ERROR modelo en base a métodos simples
real <- data.frame(data_serie)
data_real <- ts(real,start = 1)
accuracy(mf,data_real)

#------------------ método REGRESIóN ------------------
# elaborando la regresion
regresion <- tslm(data_serie ~ trend + season)
# elaborando el pronostico
m2 <- forecast(regresion, h=7)
# graficando el pronóstico
autoplot(m2)
# verificando el ajuste del método
autoplot(m2)+autolayer(fitted(m2), series="Ajuste")
# verificando los residuales
checkresiduals(m2)
#ESTIMACIÓN DEL ERROR modelo en base a regresion lineal
real <- data.frame(data_serie)
data_real <- ts(real,start = 1)
accuracy(m2,data_real)


#------------------ Método holt winters ------------------ #veificar PACKAGES
# elaborando el pronostico
#m3 <- hw(data_serie, h=96, seasonal = 'multiplicative')
# graficando el pronóstico
#autoplot(m3)
# verificando el ajuste del método
#autoplot(m3)+autolayer(fitted(m3), series="Ajuste")
# verificando los residuales
#checkresiduals(m3)

#ESTIMACIÓN DEL ERROR modelo en base a holt winters
#real <- data.frame(data_serie)
#data_real <- ts(real,start = 1)
#accuracy(m3,data_real)

#------------------ ARIMA ------------------
#Luego con la función forecast realizamos el pronostico. El argumento a colocar en estas funcion
#es el modelo ARIMA y el valor de h. Este valor de h es la cantidad de datos que deseamos pronosticar.
#Finalmente, para verificar el ajuste del método podemos emplear las siguientes funciones:
#fitted(), obtiene pronostico con la data historica
#checkresiduals(), permite analizar los residuales

# elaborando el modelo ARIMA
modelo_arima <- auto.arima(data_serie)
# elaborando el pronostico
m4 <- forecast(modelo_arima, h=7)
# graficando el pronóstico
autoplot(m4)
# verificando el ajuste del método
autoplot(m4)+autolayer(fitted(m4), series="Ajuste")
# verificando los residuales
checkresiduals(m4)
#ESTIMACIÓN DEL ERROR modelo en base a ARIMA
real <- data.frame(data_serie)
data_real <- ts(real,start = 1)
accuracy(m4,data_real)

#------------------ ANN ------------------
# elaborando el modelo de red neuronal
neural_network <- nnetar(data_serie)
# elaborando el pronostico
m5 <- forecast(neural_network, h=7)
# graficando el pronóstico
autoplot(m5)
# verificando el ajuste del método
autoplot(m5)+autolayer(fitted(m5), series="Ajuste")
# verificando los residuales
checkresiduals(m5)
#ESTIMACIÓN DEL ERROR modelo en base a red neuronal
real <- data.frame(data_serie)
data_real <- ts(real,start = 1)
accuracy(m5,data_real)

#estimación del error
#real <- c(13487, 12776, 13812, 13032, 14268, 14473, 15359, 14457 )
#real <- data.frame(data_serie)
#data_real <- ts(real, frequency=12,start=2019)
#data_real <- ts(real,start = 1)
#data_real <- ts(real,start = 1, frequency = 7)

###https://rstudio-pubs-static.s3.amazonaws.com/855973_05b0301e30a54be7afd13073caef3a99.html###
###https://rpubs.com/brianz0r/570975#:~:text=Para%20escoger%20el%20mejor%20modelo,0%2C1%2C1).
###https://support.minitab.com/es-mx/minitab/21/help-and-how-to/statistical-modeling/time-series/how-to/forecast-with-best-arima-model/interpret-the-results/key-results/
#Entonces a partir de estos errores de pronóstico podemos determinar cual es el modelo mas adecuado.
#En la práctica se suelen evaluar varios modelos e incluso tomar como pronóstico un valor medio del
#resultados de dos o más modelos.

##https://developer.ibm.com/tutorials/use-ibm-spss-statistics-to-analyze-covid-19-data/
###https://github.com/navido89/Time-Series-Analysis-ARIMA-Model-Covid19-Predictions
###https://towardsdatascience.com/predicting-number-of-covid19-deaths-using-time-series-analysis-arima-model-4ad92c48b3ae

#Diferencia entre la pendiente y los datos REGRESION LINEAL
covid2020ALL$DIFERENCIA<-covid2020ALL$dato1...N - covid2020ALL$effects
rm(FALLECIDOS_FECHA,INFECTADOS,INFECTADOS_FECHA)
head(PENDIENTE)
str(PENDIENTE)
#unlist(PENDIENTE)
PENDIENTE<-as.vector(PENDIENTE)
covid2020ALL$Pendiente<-PENDIENTE

#Graficando la pendiente
pendiente <- matrix(serieINF, ncol = 7, byrow = T)#ventana de días
MediaSerie <- rowMeans(pendiente)
días <- seq(1,7,len=52) #len=subconjuntos 359/7 = 52

#pendiente
ajuste <- lm(MediaSerie~días)
ajuste
summary(ajuste)
plot(días,MediaSerie, type = "l")
abline(ajuste, lty=2)

MediaSerie
#filtrando la serie quitando los picos
fserie <- filter(serie)

#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#MODELO ARIMA
require(graphics)
arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796))
# mildly long-tailed
arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5))

# An ARIMA simulation
ts.sim <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 200)
ts.plot(ts.sim)
help(arima.sim)
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#AR: Autoregresivo – cómo una regresión sobre sí mismo
# I: Integrado – diferencias con respecto a valores posteriores
#MA:- Media movil (Moving Average en inglés) – tiene que ver con los «errores» de la predicción
# S: Estacional – para fenómenos que se repiten.
# creando objeto ts para modelo
conteo_ts <- ts(covid2020ALL$TOTALES,
                start = 1,
                frequency = 257)
#start(): inicio de donde se va a observar las predicciones
#nùmero de observaciones por unidad de tiempo

library(forecast)
ajuste <- auto.arima(y = conteo_ts)
summary(ajuste)
predicciones <- forecast(ajuste)
autoplot(predicciones)
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#CALCULANDO LOS PARAMETERS DE SEIR
# Define las variables 
t <- covid2020ALL$FECHA # tiempo 
N <- covid2020ALL$TOTALES # población total 
I <- covid2020ALL$TOTAL_INF # número de infectados
R <- covid2020ALL$TOTAL_SOS # número de recuperados ***********
D <- covid2020ALL$TOTAL_DEF # número de fallecidos 

# Define la función SIR 
SIR <- function(t, y, p) {
  with(as.list(c(y, p)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I - mu * I
    dR <- gamma * I - mu * R
    dD <- mu * I
    list(c(dS, dI, dR, dD))
  })
}

# Define los valores iniciales 
S0 <- N - I[1]
I0 <- I[1]
R0 <- R[1]
D0 <- D[1]
y0 <- c(S = S0, I = I0, R = R0, D = D0)
valores<- as.data.frame(y0)
y0 <- head(y0,-3)  #TOTALES
plot(y0, xlab = "FECHAS 2020", ylab = "No. de Casos", main = "Acumulados por Día año 2020" )

# Define los límites para los parámetros 
lower <- c(beta = 0, gamma = 0, mu = 0)
upper <- c(beta = 1, gamma = 1, mu = 1)

na.omit
# Ajusta el modelo a los datos 
ajuste <- nls(y ~ as.vector(integrate(SIR, t = 257, y = y0, parms = p)$value[-1, ]),
              start = c(beta = 0.2, gamma = 0.1, mu = 0.01), lower = lower, upper = upper)

# Muestra los resultados 
summary(ajuste)

help("nls")
help(EpiModel)

#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
#+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

library(deSolve)
# Define el vector de tiempo
t <- seq(0, 200, by = 1)
# Simula el modelo SIR con los parámetros óptimos
sim <- as.data.frame(ode(y = c(N - I[1], I[1], 0), times = t, func = sir_model, parms = c(alpha = alpha, gamma = gamma, N = N)))
# Grafica los resultados
plot(t, sim$S, type = "l", xlab = "Tiempo", ylab = "Fracción de población", col = "blue", lwd = 2, ylim = c(0,1))
lines(t, sim$I, col = "red", lwd = 2)
lines(t, sim$R, col = "green", lwd = 2)
legend("topright", c("Susceptibles", "Infectados", "Recuperados"), col = c("blue", "red", "green"), lwd = 2)

#Este código graficará la fracción de población susceptible, infectada y recuperada a lo largo del tiempo, simulado con los parámetros óptimos
#encontrados.

#LIMPIANDO FECHAS
#summary(SINTOMAS)
SINTOMAS

#CREANDO UN DATAFRAME DE LA PRIMERA FECHA
FECHAS<-data.frame(SINTOMAS$FECHA)
names(FECHAS)[1] = "FECHA"

#UNIFICANDO EL CARACTER / A -
FECHAS$FECHA <- str_replace_all(FECHAS$FECHA, "/", "-")
colnames(FECHAS)
summary(FECHAS)

#OBTENIENDO LOS CARACTERES CORRECTOS DE LA FECHA
FECHAS$DIA<- ifelse(2020 == substr(FECHAS$FECHA,1,4),
                    (substr(FECHAS$FECHA,9,10)),
                    (substr(FECHAS$FECHA,1,2 )))

FECHAS$MES<- ifelse(2020 == substr(FECHAS$FECHA,1,4),
                    (substr(FECHAS$FECHA,6,7)),
                    (substr(FECHAS$FECHA,4,5 )))

#CONCATENANDO LA FECHA
FECHAS$FECHA_limpia<- str_c("2020/",FECHAS$MES,"/",FECHAS$DIA)
as.character(FECHAS$FECHA, format="%yyyy/%mm/%dd")

#PASANDOLA DE CARACTER  A NUMERO
FECHAS$FECHA_limpia<-paste0("2020",FECHAS$MES,FECHAS$DIA)
FECHAS$FECHA_limpia <- as.numeric(FECHAS$FECHA_limpia)
summary(FECHAS)

#+-+-+-+-+-+-+-+-+-+- COMBINACIÓN +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
COVID2020<-merge(x = INFECTADOS_FECHA, y = FALLECIDOS_FECHA, all = TRUE)

##----------- REMPLAZANDO FECHA CON FECHA LIMPIA ---------------
colnames(SINTOMAS)
colnames(FECHAS)

DataSetCLEAN1<-cbind(FECHAS$FECHA_limpia, SINTOMAS[,c(2:25)])
colnames((DataSetCLEAN1))
names(DataSetCLEAN1)[1] = "FECHA"
summary(DataSetCLEAN1)

# No_CASOS TOTALES=SIR
#agregar ceros a los NAs

#calcular parametros de las tasas

#POBLACIÓN = TOTAL_cASOS + INFECTADOS + FALLECIDOS + RECUPERADOS

#Recuperados
TOTAL_cASOS = Susceptibles + total_infectados + Fallecidos(recuperados)
#   10                7                3            0                       alpha beta gama
#   10                7                3            2                       alpha beta gama
#   10                7            +  -                +

COVID2020all<-merge(x = COVID2020, y = SINTOMAS, all = TRUE)
#S   I   R
par[ametros]

TOTALES<-count(FECHA, sort = TRUE )
  group_by(covid_data)
TOTALES<-as.data.frame(covid_data %>% 
                            filter(FECHA))
TOTALES_FECHA<-ddply(TOTALES, .(FECHA), nrow)
colnames(TOTALES_FECHA)

#UNIR INFECTADOS CON FALLECIDOS
RECUPERADOS <-as.data.frame(covid_data %>% 
                             filter(FECHA_SINTOMAS!=0)) #colnames(FALLECIDOS)

conteo<-as.data.frame(table(covid_data$FECHA_SINTOMAS))
#+-+-+-+-+-+-+-+-+-+- CONTEO POR FECHA DE RECUPERADOS +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
RECUPERADOS_FECHA<-ddply(FALLECIDOS, .(FECHA), nrow)
colnames(FALLECIDOS_FECHA)

#++++++++++++++++++++ SERIE DE TIEMPO ++++++++++++++++++++
Timeserie <- c(INFECTADOS_FECHA$TOTAL_INF)
Serie2 <- ts(c(Timeserie), start = c(1,30))
Serie2
plot(Serie2, main="Infectados de COLIMA durante el año 2020 (DÍA)",xlab="Días (Time)", ylab="No. Casos de Infección")  
ts.plot(Serie2)

#++++++++++++++++++++ GOMPERTZ ++++++++++++++++++++
# Función GOMPERTZ para calcular el crecimiento de casos
gompertz <- function(t, a, b, c) {
  a * exp(-b * exp(-c * t))
}

day<-INFECTADOS_FECHA$FECHA
day<-na.omit(day)

casos<-INFECTADOS_FECHA$TOTAL_INF
casos<-na.omit(casos)

muestra<-cbind(day, casos)
muestra

day
casos

d<-cbind(day, casos)
d<-as.data.frame(d)

help("nlsfit")

model = nlsfit(d, model = 10, start = c(a = 7000, b = 2, c = 0.0009))
model

#Estimación previa
model2 = nlsfit(d, model = 10, start = c(a = 1.469655e+06, b = 4.622443e+00, c = 2.685300e-03))
model2

#plot data
nlsplot(d, model = 10, start = c(a = 862544, b = 2, c = 0.05), 
        xlab = "Days" , ylab = "infectados por COVID en Bolivia", position = 7)    

time_simul<-c(1:257) # Al 30 de junio/2022
time_simul 
alpha<-9.712779e+05
beta<-4.509359e+00
k<-3.418130e-03
y_simul<-alpha*exp(-beta*exp(-k*time_simul))
plot(y_simul, lty=2, ylab= "Cantidad de infectados", xlab= "D�as",main= "Pron�stico de Gompertz )

#install.packages("#tseries")
library(tseries)

#install.packages("rlang")
library(rlang)

tt <- seq(as.Date('2020-03-11'),as.Date('2022-06-30'),by = 1)
tt
infectados_gompertz<-ts(y_simul, start=c(2020,71), freq=365)
plot(infectados_gompertz) 
var_casos<-diff(infectados_gompertz)
plot(var_casos)
var_casos

#1.2. Gr�ficando las variables

# http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/

#install.packages("ggplot2")
#install.packages("colorspace")
library(colorspace)
library(ggplot2)

# Graficando una serie individual
# method:  "auto", "lm", "glm", "gam", "loess"
t <- seq(as.Date('2020-03-12'),as.Date('2022-06-30'),by = 1)
t
var_casos

muestra<-ts.intersect(var_casos)
muestra
muestra<-as.data.frame(t,var_casos)
muestra

#a<-ggplot(data = muestra, aes(x = t, y = var_casos))+
#  geom_line(color = "#00AFBB", size = 2) +  
#  ggtitle("Escenario base de nuevos infectados por COVID-19 en Bolivia, pron�stico GOMPERTZ")+
#  xlab("2020-2022") + ylab("Nuevos casos/d�a")
#a

t1 <- seq(as.Date('2020-03-11'),as.Date('2022-06-30'),by = 1)
t1

y_simul<-ts(y_simul, start=c(2020,71), freq=365)
y_simul

muestra<-ts.intersect(y_simul)
muestra<-as.data.frame(y_simul)
muestra

# Gr�fico de una serie

muestrab<-as.data.frame(y_simul)


b<-ggplot(data = muestrab, aes(x = tt, y = y_simul))+
  geom_line(color = "#00AFBB", size = 2) +  
  ggtitle("Pron�stico de infectados por COVID-19 en Bolivia, 
           estimaci�n GOMPERTZ")+
  xlab("2020-2022") + ylab("Casos totales")
b

# Gr�ficando dos series
# Graficando dos series


t0 <- seq(as.Date('2020-03-11'),as.Date('2022-01-19'),by = 1)
t0

muestra<-cbind(y_simul, casos_totales)
muestra<-na.omit(muestra)
muestra<-as.data.frame(muestra)

# Gr�fico de una serie

ab<-ggplot(muestra, aes(t0)) +
  geom_line(aes(y = casos_totales, ,colour = "Observados"), size=1.5) +
  geom_line(aes(y = y_simul, colour = "Estimados"), size=1.5) +
  scale_colour_hue("variable")+
  ggtitle("Infectados acumulados en Bolivia por COVID-19")+
  xlab("tiempo") + ylab("En número de casos")

#TRUE 406579-
INFECTADOS_MUJERES<-ddply(INFECTADOS, .(SEXO==1), nrow) #CONTEO TOTAL

MujeresCOVID <- select(filter(INFECTADOS,SEXO=='1' & CLASIFICACION_FINAL=='1'),FECHA,
                       SEXO,ENTIDAD_NAC,ENTIDAD_RES,MUNICIPIO_RES, CLASIFICACION_FINAL)

MUJERES_TOTAL<-left_join(INFECTADOS_FECHA,MujeresCOVID, by = "FECHA")#MATCH DE LOS DATOS CON FECHA
INFECTADOS_MUJERES<-ddply(MUJERES_TOTAL, .(FECHA), nrow) #CONTEO INFECTADOS POR FECHA DE MUJERES
names(INFECTADOS_MUJERES)[2]="INFEC_MUJERES"  

#-------------------------------------------------------------------------------------
#TRUE 455973
INFECTADOS_HOMBRES<-ddply(INFECTADOS, .(SEXO==2), nrow) #CONTEO TOTAL

HombresCOVID <- select(filter(INFECTADOS,SEXO=='2' & CLASIFICACION_FINAL=='1'),FECHA,
                       SEXO,ENTIDAD_NAC,ENTIDAD_RES,MUNICIPIO_RES, CLASIFICACION_FINAL)

HOMBRES_TOTAL<-left_join(INFECTADOS_FECHA, HombresCOVID, by = "FECHA")#MATCH DE LOS DATOS CON FECHA
INFECTADOS_HOMBRES<-ddply(HOMBRES_TOTAL, .(FECHA), nrow) #CONTEO INFECTADOS POR FECHA DE MUJERES
names(INFECTADOS_HOMBRES)[2]="INFEC_HOMBRES"  

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Timeserie <- c(INFECTADOS_FECHA$TOTAL_INF)
Serie2 <- ts(c(Timeserie), start = c(1,31))
Serie2
plot(Serie2,main="Infectados POR FECHA del año 2021")  
ts.plot(Serie2)

data(oilfilters); plot(oilfilters,type='o',ylab='Sales')

boxplot(split(Serie2,cycle(Serie2)))

summary(INFECTADOS_FECHA$FECHA_ACTUALIZACION)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Filtrando los datos de un SOLO ESTADO
Colima<-case_when(covid_data$)

#Contando el numero de casos por día
covid_data$CLASIFICACION_FINAL<-case_when( covid_data$RESULTADO_LAB ==1~"1",covid_data$RESULTADO_LAB ==2~"7",
                                           covid_data$RESULTADO_LAB ==3~"5",covid_data$RESULTADO_LAB ==4~"4",
                                           covid_data$RESULTADO_LAB ==97~"6")
Colima_DataSet<-covid_data

summary(covid_data)
INFECTADOS<-as.data.frame(Colima_DataSet %>% 
                            filter(RESULTADO_LAB==1 & CLASIFICACION_FINAL==1))

INFECT_MUJER<-as.data.frame(Colima_DataSet %>% 
                              filter(RESULTADO_LAB==1 & CLASIFICACION_FINAL==1  & SEXO ==1))

INFECT_HOMBRE<-as.data.frame(Colima_DataSet %>% 
                               filter(RESULTADO_LAB==1 & CLASIFICACION_FINAL==1 & SEXO ==2))

summary(INFECT_HOMBRE)

#ELIMINANDO LOS INDICES X DE R
file_path <- str_c("C:/COVID/COLIMA/CSV/dataSET/GOMPERtz/COLIMA2020.csv")
SINTOMAS <- read.table(file_path, sep = "," , header = T ,
                           na.strings ="", 
                           stringsAsFactors= F)
colnames(Year2020_ORI)

DATOS_F1<-c("X")
Colima_DataSet<-Year2020_ORI[ , !(names(Year2020_ORI) %in% DATOS_F1)]
colnames(Colima_DataSet)

Year2020_ORI$CLASIFICACION_FINAL<-case_when( Year2020_ORI$RESULTADO_LAB ==1~"1",Year2020_ORI$RESULTADO_LAB ==2~"7",
                                             Year2020_ORI$RESULTADO_LAB ==3~"5",Year2020_ORI$RESULTADO_LAB ==4~"4",
                                             Year2020_ORI$RESULTADO_LAB ==97~"6")
Colima_DataSet<-Year2020_ORI

INFECTADOS<-as.data.frame(Colima_DataSet %>% 
                            filter(RESULTADO_LAB==1 & CLASIFICACION_FINAL==1))


# Filtrar los datos para un país específico (en este caso, México)
mexico_data <- covid_data %>%
  filter(location == "Mexico") %>%
  select(FECHA_ACTUALIZACION, new_cases, new_deaths) %>%
  mutate(total_cases = cumsum(new_cases),
         total_deaths = cumsum(new_deaths))
#Después de cargar los datos y seleccionar las columnas relevantes, puedes ajustar el
#modelo SEIR utilizando la función seir.fit de la librería outbreaks:
#library(outbreaks)