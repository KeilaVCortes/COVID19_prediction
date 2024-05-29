#CREACIÓN DE ACUMULADOS DE COVID NACIONAL
library(readr)    #Importación y lectura de CSV (read_csv())
library(readxl)   #Lectura de archivos de excel
library(tidyverse)
library(plyr)#libreria para ddply, realiza sumarizaciones

#Leyendo archivo de COVID
#covid_data <- read_csv("C:/COVID/ENT/00NACIONAL/01_ENERO_2021.csv")
covid_data <- read_csv("C:/COVID/ENT/06_COLIMA/YEAR2021.csv")
colnames(covid_data)

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

#+-+-+-+-+-+-+-+-+-+- REGISTROS TOTALES DE PERSONAS INFECTADAS +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
INFECTADOS<-as.data.frame(covid_data %>% 
                            filter(RESULTADO_LAB==1 & (CLASIFICACION_FINAL==1 | CLASIFICACION_FINAL== 2|CLASIFICACION_FINAL==3)))
colnames(INFECTADOS)
#+-+-+-+-+-+-+-+-+-+- CONTEO POR FECHA DE INFECTADOS +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

INFECTADOS_FECHA<-ddply(INFECTADOS, .(FECHA_ACTUALIZACION), nrow)
colnames(INFECTADOS_FECHA)
names(INFECTADOS_FECHA)[2]="TOTAL_INF"
summary(INFECTADOS_FECHA)

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

#data(oilfilters); plot(oilfilters,type='o',ylab='Sales')
#boxplot(split(Serie2,cycle(Serie2)))

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
Year2020_ORI <- read.table(file_path, sep = "," , header = T ,
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

library(outbreaks)