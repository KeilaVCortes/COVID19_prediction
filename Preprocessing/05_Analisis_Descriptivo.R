#Analisis descriptivo de las variables
library(readr)
library(readxl)
library(psych)
library(tidyverse)#manipulación y análisis de datos
library(dslabs) 
library(lattice) 
library(dplyr)
library(ggplot2)#paquete especializado para visualizaci�n de datos avanzados
library(janitor)#tablas de frecuencias
library(lubridate)

#ABRIEND O ARCHIVO ORIGINAL
file_path <- str_c("C:/COVID/ENT/06_COLIMA/01_ENERO_2021.csv")

MES2021_ORI <- read.table(file_path, sep = "," , header = T ,
                          na.strings ="", 
                          stringsAsFactors= F)
colnames(MES2021_ORI)
summary(MES2021_ORI)

#-+-+-+-+-+-+-+-+-   *** GRÁFICAS ***   -+-+-+-+-+-+-+-+-+-+-+-
#----------------------- FECHA ACTUALIZACION ------------------------------------------------------------------------------------------- 
table(MES2021_ORI$FECHA_ACTUALIZACION) #CAMBIAR LA FECHA A UN DATO NUMÉRICO O CUANTITATIVO
summary(MES2021_ORI$FECHA_ACTUALIZACION)
hist(MES2021_ORI$FECHA, main = "Histograma Variable FECHA", xlab = "Fechas",
     ylab = "Frecuencia", col=rainbow(15), xlim = c(20200419,20201231), probability = 0.5)
par(new = TRUE)
boxplot(MES2021_ORI$FECHA, horizontal = TRUE,
        lwd = 2, col = rgb(0, 0, 0, alpha = 0.2))
colnames(MES2021_ORI)
tabla_Fecha<- table((MES2021_ORI$FECHA))
#legend(x = "topright", legend = tabla_Fecha, fill = rainbow(15), 
#      title = "VALORES")

# VALORES
# fechas
boxplot(MES2021_ORI$FECHA, main="Variable FECHA", axes = TRUE, horizontal = TRUE)
#stripchart(MES2021_ORI, method = "jitter", jitter = 1, add = TRUE, vertical = TRUE, pch=19)
stripchart(MES2021_ORI$FECHA, method = "jitter", pch = 19, add = TRUE, col = "blue")
#Puede representar los intervalos de confianza del 95 % para la mediana
#en un diagrama de caja R, estableciendo el notchargumento en TRUE.
boxplot(MES2021_ORI$FECHA, notch = TRUE, main = "Histograma Variable FECHA")

#----------------------- SEXO ---------------------------------------------------------------------------------------------------------- 
table(MES2021_ORI$SEXO)
hist(MES2021_ORI$SEXO, main = "Variable SEXO")
# VALORES
# 1 MUJER
# 2 HOMBRE
#99 NO ESPECIFICADO

hist(MES2021_ORI$SEXO,breaks = 2, prob = TRUE, main = "Histograma Variable SEXO",
     xlab = "VALORES", ylab = "Frecuencia", col = c("#FF1493","#00BFFF"), xlim = c(1,2))
grid(nx = NA, ny = NULL, lty = 2, col = "gray", lwd = 1)
legend(x = "topright", legend = c("MUJER", "HOMBRE"), fill = c("#FF1493","#00BFFF"), 
       title = "VALORES")

grafica_SEXO<-table(MES2021_ORI$SEXO)

barplot(grafica_SEXO,beside=T,names.arg = c("mujer","hombre"), main="SEXO",xlab = "Valores",
        ylab = "No. Casos", col = c("#FF1493","#00BFFF"))
#, xlim = c(0,2),, ylim = c(83000,930000)
pie(grafica_SEXO, labels=as.character(grafica_SEXO),
    main="Variable SEXO",
    col = c("#FF1493","#00BFFF"),
    border="brown",
    clockwise=TRUE
)
legend(x = "topright", legend = c("MUJER", "HOMBRE"), fill = c("#FF1493","#00BFFF"), 
       title = "VALORES")

#----------------------- ENTIDAD RESIDENCIA --------------------------------------------------------------------------------------------- 
table(MES2021_ORI$ENTIDAD_RES)
hist(MES2021_ORI$ENTIDAD_RES, main="ENTIDAD_res")
summary(MES2021_ORI$ENTIDAD_RES)
# VALORES
# CLAVE ESTADOOS DE RESIDENCIA = COLIMA

#----------------------- MUNICIPIO RESIDENCIA ------------------------------------------------------------------------------------------- 
table(MES2021_ORI$MUNICIPIO_RES)
summary(MES2021_ORI$MUNICIPIO_RES)
tabla_municipio<-data.frame(table(MES2021_ORI$MUNICIPIO_RES))
hist(tabla_municipio$Freq, main = "MUNICIPIO RESIDENCIA", xlab = "MUNICIPIOS")
#boxplot(tabla_municipio$Freq~tabla_municipio$Var1)
barplot(tabla_municipio$Freq, names.arg = c("0","1","2","3","4","5","6","7","8","9","10"),
        xlab = "MUNICIPIOS", ylab = "No.Casos", main = "Variable MUNICIPIO DE RESIDENCIA", col = rainbow(11))
legend(x = "topright", legend = MES2021_ORI$MUNICIPIO_RES, fill = rainbow(11), title = "VALORES")

#----------------------- FECHA DE INGRESO ----------------------------------------------------------------------------------------------- 
hist(MES2021_ORI$FECHA_INGRESO, main = "Variable Fecha de Ingreso", xlab = "Fechas", ylab = "No. Casos",
     col = rainbow(15))
# VALORES
# fechas
boxplot(MES2021_ORI$FECHA_INGRESO)

#----------------------- FECHA SINTOMAS ------------------------------------------------------------------------------------------------- 
hist(MES2021_ORI$FECHA_SINTOMAS, main = "Variable Fecha de Sintomas", xlab = "Fechas", ylab = "No. Casos", col = rainbow(16))
boxplot(MES2021_ORI$FECHA_SINTOMAS, main = "Variable Fecha de Sintomas",ylab = "Fechas")
# VALORES
# fechas

#----------------------- FeCHA DEFUNCI�N ------------------------------------------------------------------------------------------------- 
#quitando Valores NULOS
FECHAS<-MES2021_ORI
table(FECHAS$FECHA_DEF)
FECHAS<-FECHAS$FECHA_DEF[!is.na(FECHAS$FECHA_DEF)]
table(FECHAS)
head(FECHAS)
str(FECHAS)

MES2021_ORI$FECHAS_limpia<-ifelse(MES2021_ORI$FECHA_DEF=="NA",0,
                                  MES2021_ORI$FECHA_DEF)
FECHAS_limpiaNEW<-MES2021_ORI$FECHAS_limpia[MES2021_ORI$FECHAS_limpia!="0"]
table(MES2021_ORI$FECHAS_limpia)
#FL<-data.frame(FECHAS_limpiaNEW)
table(FECHAS_limpiaNEW)
str(FECHAS_limpiaNEW)
FEXAS<- as.numeric(FECHAS_limpiaNEW)
FEXAS<-substring(FEXAS,1,6)
head(FEXAS)
str(FEXAS)

FEXAS<- as.numeric(FEXAS)
boxplot(FEXAS, main="Variable FECHA DEFUNCI�N")
unique(FEXAS)

densityplot(FEXAS, main="Variable FECHA DEFUNCI�N")

#HACERLO POR MESES
#quitar las FEcha
hist(MES2021_ORI$FECHA_DEF, main="Variable FECHA DEFUNCI�N")
# VALORES
# fechas
FECHAS<-MES2021_ORI$FECHA_DEF
names(FECHAS)[1] = "FECHA_DEF"
summary(FECHAS)
str(FECHAS)
class(FECHAS)

datos_convertidos <- ymd(FECHAS)
datos_convertidos
FECHA_sNA<- datos_convertidos[!is.na(datos_convertidos)]
str(FECHA_sNA)
class(FECHA_sNA)

FECHA_BIEN <- data.frame(format(as.numeric(as.POSIXct(FECHA_sNA))))
names(FECHA_BIEN)[1]="FECHAs"
FEXAS<- as.numeric(FECHA_BIEN$FECHAs)
summary(FEXAS)

boxplot(FEXAS)
densityplot(FEXAS)

lapply(datos_convertidos, density)

head(FECHAS)
#FECHAS$FECHA_DEF_limpia <-as.numeric(FECHAS$FECHA_DEF)
#summary(FECHAS$FECHA_DEF_limpia)
#boxplot(FECHAS$FECHA_DEF_limpia, main="FECHA DEFUNCI�N CON NA's")

#SIN NA's
FECHAS$FECHA_DEF <-as.numeric(FECHAS$FECHA_DEF)
boxplot(FECHAS, main="CON")
densityplot(FECHAS$FECHA_DEF)

FECHAS<-FECHAS[!is.na(FECHAS$FECHA_DEF),]#elimina el numero de observaciones
boxplot(FECHAS, main="SIN")
hist(FECHAS)
summary(FECHAS)

densityplot(FECHAS)

DEMO<- which(!is.na(FECHAS$FECHA_DEF_limpia))#127427
boxplot(DEMO, main="FECHA DEFUNCI�N")
summary(DEMO)
hist(DEMO, main = "Variable FECHA DEFUNCI�N", )

#----------------------- EDAD ------------------------------------------------------------------------------------------------------- 
hist(MES2021_ORI$EDAD,main = "Variable Edad" , xlab = "Edades", ylab = "No. Casos", col = rainbow(15))
# VALORES
# rangos de edades
boxplot(MES2021_ORI$EDAD,main = "Variable Edad" , xlab = "Edades", ylab = "No. Casos")

#----------------------- NACIONALIDAD ----------------------------------------------------------------------------------------------- 
table(MES2021_ORI$NACIONALIDAD)
hist(MES2021_ORI$NACIONALIDAD, breaks = 2, prob = TRUE, main = "Variable NACIONALIDAD",
     xlab="Edades", ylab="Frecuencia", col =  c("#008B00","#CD1076"))
legend(x = "topright", legend = c("MEXICANA", "EXTRANJERA"), fill = c("#008B00","#CD1076"), 
       title = "VALORES")
# vALORES
# 1	MEXICANA
# 2	EXTRANJERA
#99	NO ESPECIFICADO

grafica_NACIONALIDAD<-table(MES2021_ORI$NACIONALIDAD)

pie(grafica_NACIONALIDAD, labels=as.character(grafica_NACIONALIDAD),
    main="Variable  NACIONALIDAD",
    col= c("#008B00","#CD1076"),
    border="brown",
    clockwise=TRUE
)
legend(x = "topright", legend = c("MEXICANA", "EXTRANJERA"), fill =  c("#008B00","#CD1076"), 
       title = "VALORES")

#----------------------- OTRO_CASO --------------------------------------------------------------------------------------------- 
table(MES2021_ORI$OTRO_CASO)
hist(MES2021_ORI$OTRO_CASO, prob = TRUE, main = "Variable OTRO_CASO",
     xlab="CASOS", ylab="Frecuencia", col =  c("orange","blue","yellow"))
legend(x = "topleft", legend = c("0", "1","2"), fill = c("orange","blue","yellow"), 
       title = "VALORES")

#----------------------- TOMA_MUESTRA_LAB --------------------------------------------------------------------------------------------- 
table(MES2021_ORI$TOMA_MUESTRA_LAB)
hist(MES2021_ORI$TOMA_MUESTRA_LAB, breaks = 2, prob = TRUE, main = "Variable TOMA_MUESTRA_LAB",
     xlab="CASOS", ylab="Frecuencia", col =  c("pink","red"))
legend(x = "topright", legend = c("VALOR_1", "Valor_2"), fill = c("pink","red"), 
       title = "VALORES")

#----------------------- RESULTADO LABORATORIO -------------------------------------------------------------------------------------- 
resultado_lab<-table(MES2021_ORI$RESULTADO_LAB)

hist(resultado_lab, breaks = 5, prob = TRUE, main = "Variable RESULTADO_LAB",
     xlab="TIPOS", ylab="Frecuencia", col =  c("#66CD00","#FF1493","#00BFFF","#AB82FF","#EEE8AA"), labels = as.character(resultado_lab))

#CLAVE	  DESCRIPCI�N
# 1  	POSITIVO A SARS-COV-2
# 2  	NO POSITIVO A SARS-COV-2
# 3 	RESULTADO PENDIENTE
# 4 	RESULTADO NO ADECUADO 
# 0 	NO APLICA (CASO SIN MUESTRA)

pie(resultado_lab, labels=as.character(resultado_lab),
    main="Variable  RESULTADO_LAB",
    col= c("#66CD00","#FF1493","#00BFFF","#AB82FF","#FFFF00","#EEE8AA"),
    border="brown",
    clockwise=TRUE
)
legend(x = "topright", legend = c("0)NO APLICA (CASO SIN MUESTRA)","1)POSITIVO", "2)NO POSITIVO", "3)RESULTADO PENDIENTE",
                                  "4)RESULTADO NO ADECUADO"),
       fill = c("#66CD00","#FF1493","#00BFFF","#AB82FF","#EEE8AA"), title = "VALORES")

#----------------------- TOMA_MUESTRA_LAB --------------------------------------------------------------------------------------------- 
table(MES2021_ORI$TOMA_MUESTRA_ANTIGENO)
hist(MES2021_ORI$TOMA_MUESTRA_ANTIGENO, breaks = 2, prob = TRUE, main = "Variable TOMA_MUESTRA_LAB",
     xlab="CASOS", ylab="Frecuencia", col =  c("pink","red"))
legend(x = "topright", legend = c("VALOR_1", "valor_2"), fill = c("pink","red"), 
       title = "VALORES")

#----------------------- RESULTADO LABORATORIO -------------------------------------------------------------------------------------- 
resultado_lab<-table(MES2021_ORI$RESULTADO_LAB)

hist(resultado_lab, breaks = 5, prob = TRUE, main = "Variable RESULTADO_LAB",
     xlab="TIPOS", ylab="Frecuencia", col =  c("#66CD00","#FF1493","#00BFFF","#AB82FF","#EEE8AA"), labels = as.character(resultado_lab))

#CLAVE	  DESCRIPCI�N
# 1  	POSITIVO A SARS-COV-2
# 2  	NO POSITIVO A SARS-COV-2
# 3 	RESULTADO PENDIENTE
# 4 	RESULTADO NO ADECUADO 
#97 	NO APLICA (CASO SIN MUESTRA)

pie(resultado_lab, labels=as.character(resultado_lab),
    main="Variable  RESULTADO_LAB",
    col= c("#66CD00","#FF1493","#00BFFF","#AB82FF","#FFFF00","#EEE8AA"),
    border="brown",
    clockwise=TRUE
)
legend(x = "topright", legend = c("0)NO APLICA (CASO SIN MUESTRA)","1)POSITIVO", "2)NO POSITIVO", "3)RESULTADO PENDIENTE",
                                  "4)RESULTADO NO ADECUADO"),
       fill = c("#66CD00","#FF1493","#00BFFF","#AB82FF","#EEE8AA"), title = "VALORES")

#----------------------- TOMA_MUESTRA_ANTIGENO --------------------------------------------------------------------------------------------- 
ANTIGENO<-table(MES2021_ORI$TOMA_MUESTRA_ANTIGENO)
head(ANTIGENO)
hist(ANTIGENO, main = "Variable TOMA_MUESTRA_ANTIGENO",
     xlab="TIPOS", ylab="Frecuencia", col= c("#66CD00","#FF1493"), labels = as.character(ANTIGENO))

pie(ANTIGENO, labels=as.character(ANTIGENO),
    main="Variable  ANTIGENO",
    col= c("#66CD00","#FF1493"),
    border="brown",
    clockwise=TRUE
)
legend(x = "topright", legend = c("0)NO APLICA (CASO SIN MUESTRA)","1)POSITIVO"),
       fill = c("#66CD00","#FF1493"), title = "VALORES")

#----------------------- RESULTADO_ANTIGENO --------------------------------------------------------------------------------------------- 
RESULTA_ANTI<-table(MES2021_ORI$RESULTADO_ANTIGENO)
#breaks = seq(min(CLASIFICACION_FINAL),max(CLASIFICACION_FINAL),length.out=6),

hist(MES2021_ORI$RESULTADO_ANTIGENO,
     breaks = c(0,1,2,3),
     prob = TRUE, main = "VariablE RESULTA_ANTIGENO",
     xlab="TIPOS", ylab="Frecuencia", col =  c("#00BFFF","#AB82FF","#EEE8AA"),
     labels = as.character(RESULTA_ANTI))

pie(RESULTA_ANTI, labels=as.character(RESULTA_ANTI),
    main="Variable  RESULTADO ANTIGENO",
    col =  c("#00BFFF","#AB82FF","#EEE8AA"),
    border="brown",
    clockwise=TRUE
)

#----------------------- CLASIFICACION FINAL --------------------------------------------------------------------------------------------- 
CLASIFICACION_FINAL<-table(MES2021_ORI$CLASIFICACION_FINAL)
head(CLASIFICACION_FINAL)
#breaks = seq(min(CLASIFICACION_FINAL),max(CLASIFICACION_FINAL),length.out=6),

MES2021_ORI$CLASIFICACION_FINAL_arreglo<-case_when(MES2021_ORI$CLASIFICACION_FINAL=="1"~1,
                                                   MES2021_ORI$CLASIFICACION_FINAL=="4"~2,
                                                   MES2021_ORI$CLASIFICACION_FINAL=="5"~3,
                                                   MES2021_ORI$CLASIFICACION_FINAL=="6"~4,
                                                   MES2021_ORI$CLASIFICACION_FINAL=="7"~5)

hist(MES2021_ORI$CLASIFICACION_FINAL_arreglo,
     breaks = c(0,1,2,3,4,5),
     prob = TRUE, main = "Variable CLASIFICACION",
     xlab="TIPOS", ylab="Frecuencia", col =  c("#66CD00","#FF1493","#00BFFF","#AB82FF","#EEE8AA"),
     labels = as.character(CLASIFICACION_FINAL))

#CLAVE	         DESCRIPCI�N
# 1   	CASO DE COVID-19    CONFIRMADO POR ASOCIACI�N CL�NICA EPIDEMIOL�GICA
# 2   	CASO DE COVID-19    CONFIRMADO POR COMIT� DE  DICTAMINACI�N
# 3 	  CASO DE SARS-COV-2  CONFIRMADO POR LABORATORIO
# 4 	  INV�LIDO POR LABORATORIO
# 5 	  NO REALIZADO POR LABORATORIO
# 6 	  CASO SOSPECHOSO
# 7 	  NEGATIVO A SARS-COV-2 POR LABORATORIO

pie(CLASIFICACION_FINAL, labels=as.character(CLASIFICACION_FINAL),
    main="Variable CLASIFICACION",
    col= c("#66CD00","#FF1493","#00BFFF","#AB82FF","#FFFF00","#EEE8AA"),
    border="brown",
    clockwise=TRUE
)
legend(x = "topright", legend = c("1)POSITIVO", "2)NO POSITIVO", "3)RESULTADO PENDIENTE",
                                  "4)RESULTADO NO ADECUADO","97)NO APLICA (CASO SIN MUESTRA)"),
       fill = c("#66CD00","#FF1493","#00BFFF","#AB82FF","#EEE8AA"), title = "VALORES")

#----------------------- PAIS_NACIONALIDAD ------------------------------------------------------------------------------------------- 
#table(MES2021_ORI$PAIS_NACIONALIDAD)
#boxplot(MES2021_ORI$PAIS_NACIONALIDAD)
#CLAVE	  DESCRIPCI�N

#----------------------- PAIS_ORIGEN --------------------------------------------------------------------------------------------------- 
#table(MES2021_ORI$PAIS_ORIGEN)
#hist(MES2021_ORI$PAIS_ORIGEN)#CLAVE	  DESCRIPCI�N
#plot(MES2021_ORI$PAIS_ORIGEN)

#----------------------- NOMBRE ENTIDAD ----------------------------------------------------------------------------------------------
grafica_Nom_ENT<-table(MES2021_ORI$NOMBRE_ENTIDAD)
#boxplot(MES2021_ORI$NOMBRE_ENTIDAD=="Colima")

MES2021_ORI %>% filter(NOMBRE_ENTIDAD=="NA")

pie(grafica_Nom_ENT, labels=as.character(grafica_Nom_ENT),
    main="Variable ENTIDADES (datos completos)",
    col= rainbow(12),
    border="brown",
    clockwise=TRUE
)
legend(x = "topleft", legend = c("Armer�a","Colima","Comala","Coquimatl�n",
                                 "Cuauht�moc","Ixtlahuac�n","Manzanillo","Minatitl�n","NA","Tecom�n","Villa de �lvarez"),
       fill = rainbow(12), title = "VALORES")

SIN_NAs<- data.frame(MES2021_ORI %>% 
                       filter(NOMBRE_ENTIDAD!="NA"))
SIN_NAs2<-table(SIN_NAs$NOMBRE_ENTIDAD)

pie(SIN_NAs2, labels=as.character(SIN_NAs2),
    main="Variable ENTIDADES",
    col= rainbow(12),
    border="brown",
    clockwise=TRUE
)
legend(x = "topleft", legend = c("Armer�a","Colima","Comala","Coquimatl�n",
                                 "Cuauht�moc","Ixtlahuac�n","Manzanillo","Minatitl�n","Tecom�n","Villa de �lvarez"),
       fill = rainbow(12), title = "VALORES")

MES2021_ORI$CLASIFICACION_FINAL_arreglo<-case_when(MES2021_ORI$NOMBRE_ENTIDAD=="Armer�a"~1,
                                                      MES2021_ORI$NOMBRE_ENTIDAD=="Colima"~2,
                                                      MES2021_ORI$NOMBRE_ENTIDAD=="Comala"~3,
                                                      MES2021_ORI$NOMBRE_ENTIDAD=="Coquimatl�n"~4,
                                                      MES2021_ORI$NOMBRE_ENTIDAD=="Cuauht�moc"~5,
                                                      MES2021_ORI$NOMBRE_ENTIDAD=="Ixtlahuac�n"~6,
                                                      MES2021_ORI$NOMBRE_ENTIDAD=="Manzanillo"~7,
                                                      MES2021_ORI$NOMBRE_ENTIDAD=="Minatitl�n"~8,
                                                      MES2021_ORI$NOMBRE_ENTIDAD=="Tecom�n"~10,
                                                      MES2021_ORI$NOMBRE_ENTIDAD=="Villa de �lvarez"~11,
                                                      MES2021_ORI$NOMBRE_ENTIDAD=="NA"~9)
table(MES2021_ORI$CLASIFICACION_FINAL_arreglo)

hist(MES2021_ORI$CLASIFICACION_FINAL_arreglo,
     breaks = c(0,1,2,3,4,5,6,7,8,9,10,11),
     prob = TRUE, main = "Variable CLASIFICACION",
     xlab="TIPOS", ylab="Frecuencia",
     col =  c("#66CD00","#FF1493","#00BFFF","#AB82FF","#EEE8AA"),
     labels = as.character(grafica_Nom_ENT))

#----------------------- ANÁLISIS DE CORRELACIÓN DE VARIABLES ------------------------------------------------------------------------------------------- 
#table(MES2021_ORI$PAIS_NACIONALIDAD)
#boxplot(MES2021_ORI$PAIS_NACIONALIDAD)
#CLAVE	  DESCRIPCI�N