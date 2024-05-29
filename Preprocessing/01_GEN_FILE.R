# GENERACIÓN DE ARCHIVOS A NIVEL NACIONAL
library(readr)    #Importación y lectura de CSV (read_csv())
library(readxl)   #Lectura de archivos de excel
library(psych)    #Estadística descriptivas(gralmente. para psicología)
library(tidyverse)#manipulación y análisis de datos
library(dslabs)   
rm()       
ls()       
gc()
rm(list = ls())

#CARPETA ORIGEN
setwd("E:/México/01_COVID/21_04_Abril") #Year_NoMes_Mes
list.files() #Listando los archivos
archivos<- list.files(pattern = ".\\csv") #Guardando los archivos en variables
archivos

#LECTURA DE ARCHIVOS POR DÍA
#------------------------- 01-10 -----------------------------------------------------------------------
DIEZ<-data.frame()
for (i in 1:10) {
  aux<-read.csv(archivos[i])
  DIEZ<-rbind(DIEZ,aux)      #uniendo los 10 archivos
  print(archivos[i])
}
colnames(DIEZ)               #quitar las columnas que no se ocupan
EDOs_01_10<-DIEZ[,-c(3,4,10,14,15,18,19,20,21,22,23,24,25,26,27,28,29,30,37,40)]
colnames(EDOs_01_10)

#------------------------- 11-20 -----------------------------------------------------------------------
VEINTE<-data.frame()
for (i in 11:20) {
  aux<-read.csv(archivos[i])
  VEINTE<-rbind(VEINTE,aux) #uniendo los 10 archivos
  print(archivos[i])
}
colnames(VEINTE)               #quitar las columnas que no se ocupan
EDOs_11_20<-VEINTE[,-c(3,4,10,14,15,18,19,20,21,22,23,24,25,26,27,28,29,30,37,40)]
colnames(EDOs_11_20)

#------------------------- 21-30 -----------------------------------------------------------------------
TREINTA<-data.frame()
for (i in 21:30) {
  aux<-read.csv(archivos[i])
  TREINTA<-rbind(TREINTA,aux) #uniendo los 10 archivos
  print(archivos[i])
}
colnames(TREINTA)               #quitar las columnas que no se ocupan
EDOs_21_30<-TREINTA[,-c(3,4,10,14,15,18,19,20,21,22,23,24,25,26,27,28,29,30,37,40)]
colnames(EDOs_21_30)

#-------------------------------------------------------------------------------------------------------
#MES2021<-rbind(EDOs_01_10,EDOs_11_20,EDOs_21_30)
MES2021<-rbind(EDOs_01_10)
MES2021<-rbind(EDOs_11_20)
MES2021<-rbind(EDOs_21_30)

#UNIDENDO ARCHIVOS POR DÍA A MES
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/01_ENERO_2021.csv",row.names=FALSE)
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/02_FEBRERO_2021.csv",row.names=FALSE)
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/03_MARZO_2021.csv",row.names=FALSE)
write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/04_ABRIL_2021_11_20.csv",row.names=FALSE)
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/05_MAYO_2021.csv",row.names=FALSE)
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/06_JUNIO_2021.csv",row.names=FALSE)
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/07_JULIO_2021.csv",row.names=FALSE)
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/08_AGOSTO_2021.csv",row.names=FALSE)
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/09_SEPTIEMBRE_2021.csv",row.names=FALSE)
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/6_JUNIO_2021.csv",row.names=FALSE)_
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/07_JULIO_2021.csv",row.names=FALSE)
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/08_AGOSTO_2021.csv",row.names=FALSE)
#write.csv(MES2021,"C:/COVID/ENT/06_COLIMA/09_SEPTIEMBRE_2021.csv",row.names=FALSE)