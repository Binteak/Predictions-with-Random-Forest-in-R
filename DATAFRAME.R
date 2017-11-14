#Load libraries_____________________________________________
library(readxl)
library(reshape)
source("My_functions.R")
#_________________PREPARACIÓN DE LOS DATOS__________________
#___________________________________________________________

#Lectura de datos___________________________________________
getwd()
licencias <- read_excel("C:/Users/Usuario2/MACHINE LEARNING/PROYECTO REHABILITACION/Datos/LICENCIAS.xlsx")
visados <- read_excel("C:/Users/Usuario2/MACHINE LEARNING/PROYECTO REHABILITACION/Datos/Visados_Total_Histórico.xlsx")
data <- merge(licencias, visados, by = c("ID","COMUNIDAD","AÑO","Mes"), all=F)
str(data)

#Guardo la columna de presupuesto total
pptototal <- data[,11]

#Formateo los datos, cambiando espacios por "_"_____________
names(data) <- gsub(" ", "_", names(data))

#Defino el obetivo(Target) a predecir de los posibles_______
target <- "PPTO._TOTAL" #PPTO._VIVIENDA o PPTO._TOTAL
posibles_targets <- names(data)[(ncol(data)-2):ncol(data)]

#Elimino la info redundante o que no sirve__________________
data$TOTAL_Nº_EDIF <- NULL
data$ID <- NULL
data[,posibles_targets[posibles_targets!=target]]<-NULL

#Ordeno los datos y creo factores___________________________
months <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
comms <- c("ANDALUCIA","ARAGON","ASTURIAS","BALEARES","CANTABRIA","CATALUÑA",
           "CLM","CVALENCIANA","CYL","EXTREMADURA" ,"GALICIA","ICANARIAS",               
           "MADRID","MURCIA","NAVARRA","PVASCO","RIOJA")
data$Mes <- factor(data$Mes, levels = months, ordered = T)
data$COMUNIDAD <- factor(data$COMUNIDAD,levels = comms, ordered = F)
data <- data[order(data$COMUNIDAD, data$AÑO, data$Mes), ]


shifter <- function(x = 1:10, n = 1) {
  if (n == 0) x <- x
  else x <- c(tail(x, -n), head(x, n))
  x
}

#Añado los presupuestos de los meses anteriores_____________
#y los muevo para que el target sea la última columna_______
data['PPTO._ANTERIOR'] <- shifter(data[,target], -1)
data <- data[c(1:(ncol(data)-2),ncol(data),(ncol(data)-1))]
data['PPTO._ANTERIOR_2'] <- shifter(data[,target], -2)
data <- data[c(1:(ncol(data)-2),ncol(data),(ncol(data)-1))]

#Que hago con el primer dato? Lo elimino o le paso el mismo???
data <- data[!((data$AÑO == 2000) & (data$Mes == "Ene")),]
#data[(data$AÑO == 2000) & (data$Mes == "Ene"), "PPTO._ANTERIOR"] <- data[(data$AÑO == 2000) & (data$Mes == "Ene"), target]

str(data)

#Si salen NAN
#data$COMUNIDAD[3185:3383] <- "RIOJA"

#añadir pptototal si no aparece
data$PPTO._TOTAL <- pptototal[1:3383]

#comprobación
str(data)
str(data$PPTO._TOTAL)
str(data$COMUNIDAD)
