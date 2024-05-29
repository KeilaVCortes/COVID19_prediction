#SEPARANDO ARCHIVO POR ESTADOS
library(readr)
library(readxl)
library(psych)
library(tidyverse)#manipulación y análisis de datos
library(dslabs)  #library(lattice) #library(dplyr)

No_Edo="06" #"15"
Edo="COL" #"MEX"
Year="YEAR2021"
file <- str_c(No_Edo,"_",Edo,"/",Year,".csv") #06_col/

#ABRIEND O ARCHIVO ORIGINAL
file_path <- str_c("C:/COVID/ENT/", file)
MES2021_ORI <- read.table(file_path, sep = "," , header = T ,
                          na.strings ="", 
                          stringsAsFactors= F)
colnames(MES2021_ORI)
#summary(MES2021_ORI)
table(MES2021_ORI)
table(MES2021_ORI$ENTIDAD_RES)

#-----------------------------------------------------------------------------------------------------------
MES2021_ORI <- str_replace_all(MES2021_ORI$FECHA_ACTUALIZACION, "/", "-")
ymd(MES2021_ORI$FECHA_ACTUALIZACION)
colnames(MES2021_ORI)
summary(MES2021_ORI)
MES2021_ORI$FECHA <- format(as.numeric(as.POSIXct(MES2021_ORI$FECHA_ACTUALIZACION, format="%Y-%m-%d")))
MES2021_ORI$FECHA <- as.numeric(MES2021_ORI$FECHA_ACTUALIZACION)
summary(MES2021_ORI)
#-----------------------------------------------------------------------------------------------------------
#LIMPIEZA DE FECHA DE INGRESO
FECHAS<-data.frame(MES2021_ORI$FECHA_INGRESO)
names(FECHAS)[1] = "FECHA_INGRESO"

FECHAS$FECHA_INGRESO <- str_replace_all(FECHAS$FECHA_INGRESO, "/", "-")
colnames(FECHAS)
summary(FECHAS)

as.character(FECHAS$FECHA_INGRESO, format="%d/%m/%Y")
as.character(FECHAS$FECHA_INGRESO, format="%Y/%m/%d")

#FECHAS$MES <- month(FECHAS$FECHA_INGRESO)
FECHAS$DIA<- ifelse(2020 == substr(FECHAS$FECHA_INGRESO,1,4),
                    (substr(FECHAS$FECHA_INGRESO,9,10)),
                    (substr(FECHAS$FECHA_INGRESO,1,2 )))

FECHAS$MES<- ifelse(2020 == substr(FECHAS$FECHA_INGRESO,1,4),
                    (substr(FECHAS$FECHA_INGRESO,6,7)),
                    (substr(FECHAS$FECHA_INGRESO,4,5 )))


FECHAS$FECHA_limpia<- str_c(FECHAS$DIA,"/",FECHAS$MES,"/","2020")

colnames(FECHAS)
data.frame(FECHAS)
Summary(FECHAS$FECHA_INGRESO)

FECHAS

FECHAS$FECHA_limpia <- format(as.numeric(as.POSIXct(FECHAS$FECHA_limpia, format="%Y-%m-%d")))
FECHAS$FECHA_limpia <- as.numeric(FECHAS$FECHA_limpia)
summary(FECHAS)

as.Date(dias,format="%d/%m/%Y")
summary(FECHAS)

#-----------------------------------------------------------------------------------------------------------
MES2021_ORI$FECHA_SINTOMAS <- str_replace_all(MES2021_ORI$FECHA_SINTOMAS, "/", "-")
colnames(MES2021_ORI)
summary(MES2021_ORI)

MES2021_ORI$FECHA_SINTOMAS <- format(as.numeric(as.POSIXct(MES2021_ORI$FECHA_SINTOMAS, format="%d-%m-%Y")))
MES2021_ORI$FECHA_SINTOMAS <- as.numeric(MES2021_ORI$FECHA_SINTOMAS)
summary(MES2021_ORI)

#-----------------------------------------------------------------------------------------------------------
MES2021_ORI$FECHA_DEF <- str_replace_all(MES2021_ORI$FECHA_DEF, "/", "-")
colnames(MES2021_ORI)
summary(MES2021_ORI)

MES2021_ORI$FECHA_DEF <- str_replace_all(MES2021_ORI$FECHA_DEF, "2019-12-01","9999-99-99")
colnames(MES2021_ORI)
summary(MES2021_ORI)

MES2021_ORI$FECHA_DEF <- format(as.numeric(as.POSIXct(MES2021_ORI$FECHA_DEF, format="%d-%m-%Y")))
MES2021_ORI$FECHA_DEF <- as.numeric(MES2021_ORI$FECHA_DEF)
summary(MES2021_ORI)

#-----------------------------------------------------------------------------------------------------------

#CARGANDO EL PRIMER DATASET
ENTIDAD <- select(filter(MES2021_ORI, ENTIDAD_RES==13), FECHA_ACTUALIZACION, ID_REGISTRO, ENTIDAD_UM, SEXO, ENTIDAD_NAC,
                  ENTIDAD_RES, MUNICIPIO_RES, FECHA_INGRESO, FECHA_SINTOMAS, FECHA_DEF, EDAD, NACIONALIDAD, OTRO_CASO,
                  TOMA_MUESTRA_LAB, RESULTADO_LAB, TOMA_MUESTRA_ANTIGENO, RESULTADO_ANTIGENO, CLASIFICACION_FINAL, 
                  PAIS_NACIONALIDAD, PAIS_ORIGEN)

#Guardando dataframe               #C:\COVID\ENT\06COL\2021
ruta <- str_c("C:/COVID/ENT/06COL/2021/", file)
#ruta <- str_c("C:/COVID/ENT/13HGO/2021/", file)
write.csv(ENTIDAD, ruta,row.names=FALSE)