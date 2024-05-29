#ESTANDARIZACIÓN DE FECHAS
library(lattice) 
library(readr)
library(readxl)
library(psych)
library(tidyverse) #manipulación y análisis de datos
library(dslabs)    #library(lattice) 
library(dplyr)
library(lubridate)#Modificar fechas

#ABRIEND O ARCHIVO ORIGINAL
#file_path <- str_c("C:/COVID/ENT/06_COLIMA/YEAR2020.csv")
file_path <- str_c("C:/COVID/ENT/06_COLIMA/YEAR2021.csv")
MES2021_ORI <- read.table(file_path, sep = "," , header = T ,
na.strings ="", 
stringsAsFactors= F)
colnames(MES2021_ORI)
summary(MES2021_ORI)

#table(MES2021_ORI$FECHA_ACTUALIZACION)
table(MES2021_ORI$FECHA)                #table(MES2021_ORI$ID_REGISTRO)         table(MES2021_ORI$SEXO)             #table(MES2021_ORI$ENTIDAD_RES)
table(MES2021_ORI$MUNICIPIO_RES)        #table(MES2021_ORI$FECHA_INGRESO)       table(MES2021_ORI$FECHA_SINTOMAS)       
table(MES2021_ORI$FECHA_DEF)            #table(MES2021_ORI$EDAD)                table(MES2021_ORI$NACIONALIDAD)
table(MES2021_ORI$OTRO_CASO)            #table(MES2021_ORI$TOMA_MUESTRA_LAB)
table(MES2021_ORI$RESULTADO_LAB)        #table(MES2021_ORI$TOMA_MUESTRA_ANTIGENO)
table(MES2021_ORI$RESULTADO_ANTIGENO)   #table(MES2021_ORI$CLASIFICACION_FINAL)     table(MES2021_ORI$PAIS_NACIONALIDAD)    table(MES2021_ORI$PAIS_ORIGEN)

#+++++++++++++++++++++++++++ NORMALIZANDO VALORES +++++++++++++++++++++++++++
MES2021_ORI$MUNICIPIO_RES[MES2021_ORI$MUNICIPIO_RES==999]<-0
MES2021_ORI$FECHA_DEF[MES2021_ORI$FECHA_DEF=="9999-99-99"]<-""
#MES2021_ORI$OTRO_CASO[MES2021_ORI$OTRO_CASO==99]<-0
MES2021_ORI$RESULTADO_LAB[MES2021_ORI$RESULTADO_LAB==97]<-0
MES2021_ORI$RESULTADO_ANTIGENO[MES2021_ORI$RESULTADO_ANTIGENO==97]<-0

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#ARREGLANDO LAS FECHAS

#LIMPIEZA DE FECHA DE INGRESO
#FECHAS<-data.frame(MES2021_ORI$FECHA_INGRESO)
#names(FECHAS)[1] = "FECHA_INGRESO"
#FECHAS$FECHA_INGRESO <- str_replace_all(FECHAS$FECHA_INGRESO, "/", "-")
#colnames(FECHAS)
#summary(FECHAS)
#as.character(FECHAS$FECHA_INGRESO, format="%d/%m/%Y")
#as.character(FECHAS$FECHA_INGRESO, format="%Y/%m/%d")
#FECHAS$MES <- month(FECHAS$FECHA_INGRESO)
#FECHAS$DIA<- ifelse(2020 == substr(FECHAS$FECHA_INGRESO,1,4),
#                    (substr(FECHAS$FECHA_INGRESO,9,10)),
#                    (substr(FECHAS$FECHA_INGRESO,1,2 )))
#FECHAS$MES<- ifelse(2020 == substr(FECHAS$FECHA_INGRESO,1,4),
#                    (substr(FECHAS$FECHA_INGRESO,6,7)),
#                    (substr(FECHAS$FECHA_INGRESO,4,5 )))
#FECHAS$FECHA_limpia<- str_c(FECHAS$DIA,"/",FECHAS$MES,"/","2020")
#colnames(FECHAS)
#data.frame(FECHAS)
#Summary(FECHAS$FECHA_INGRESO)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#QUITAR LAS COLUMNAS X
write.csv(MES2021_ORI,"C:/COVID/ENT/06_COLIMA/06_COL_YEAR2021.csv",row.names=FALSE)