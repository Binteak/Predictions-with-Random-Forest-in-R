#################
# RANDOM FOREST #
#################

#CARGAR SCRIPT: DATAFRAME.R

#PAQUETES Y LIBRERIAS____________________________________________________
install.packages("randomForest")
install.packages("party")
library(randomForest) 
library(party)

#DATASET________________________________________________________
str(data)

#TRAIN + TEST___________________________________________________________________________________________________
set.seed(126)  
ind <- sample(2, nrow(data), replace = TRUE, prob=c(0.85, 0.15))
Entrenamiento_RF = data[ind == 1,]
Test_RF = data[ind == 2,]

#OPTIMIZAR LA CANTIDAD DE VARIABLES_____________________________________________________________________________

#Para saber la cantidad optima de variables usamos la función 'tuneRf' del 
# mismo paquete. Esta función grafica el error OOB en cada iteración, 
# aumentando la cantidad de variables en cada paso.

#Tenemos que comprobar e identificar con cuantas variables se estabiliza el
# error.

mtry_valor <- tuneRF(x = Entrenamiento_RF,                    # data set de entrenamiento   
                     y = Entrenamiento_RF$PPTO._TOTAL ,   # variable a predecir
                     mtryStart  = 1,                           # cantidad de variables inicial 
                     stepFactor = 2,                           # incremento de variables
                     ntreeTry   = 500,                         # cantidad arboles a ejecutar en cada iteracion
                     improve    = .01)                         # mejora minina del OOB para seguir iteraciones

OOBError_sqrt <- sqrt(mtry_valor[,2])
mtry_valor1 <- cbind(mtry_valor, OOBError_sqrt)
tabla <- mtry_valor1[order(OOBError_sqrt ),]
  
print(paste0("El Error OOB óptimo es de ",tabla[2,3], ", para un valor de mtry = ", tabla[2,1], " variables recomendadas."))
print(tabla)
    
#VER ERROR POR CANTIDAD ARBOLES - MODELO__________________________________________________________________________
for (i in 500:1000){ #Normalmente a partir de 500:1000 mejora el resultado

  modelo.rf.narb <-randomForest(Entrenamiento_RF$PPTO._TOTAL ~ ., 
                           data=Entrenamiento_RF,           # datos para entrenar 
                           ntree=i,                            # cantidad de arboles   
                           mtry=8,                                # cantidad de variables
                           replace=T)                             # muestras con reemplazo
  
  print(paste0("Modelo_RandomForest con ", i, " árboles."))
  print(paste0("RMSE: ", sqrt(mean(modelo.rf.narb[]$mse))))
}
               # "Modelo_RandomForest con 630 árboles."
               # [1] "RMSE: 5309.03087107388"
               # [1] "Modelo_RandomForest con 623 árboles."
               # [1] "RMSE: 5304.0689058185"
                #Estos modelos tienen el error más bajo a la hora de segmentar en los distintos árboles
                
                
#MODELO REFORMULADO__________________________________________________________________________________
modelo.rf.ref <-randomForest(Entrenamiento_RF$PPTO._TOTAL ~ ., 
                             data=Entrenamiento_RF,              # datos para entrenar 
                             ntree=623,                                # cantidad de arboles   
                             mtry=8,                                   # cantidad de variables #comprobar
                             replace=T)                                # muestras con reemplazo

modelo.rf.ref

#VER TENDENCIA (CANTIDAD DE ARBOLES-ERROR)________________________________________________________________________
plot(modelo.rf.ref)

#TRAIN/TEST REFORMULADO_____________________________________________________________________
ind <- sample(2, nrow(data), replace = TRUE, prob=c(0.85, 0.15))
Entrenamiento_RF_ref = data[ind == 1,]
Test_RF_ref = data[ind == 2,]

#PREDICCION_______________________________________________________________________________
#Modelo reformulado
Pred.modelo.rf.ref1 <- predict(modelo.rf.ref, Test_RF_ref) 
Pred.modelo.rf.ref2 <- predict(modelo.rf.ref, Entrenamiento_RF_ref)

#MSE_____________________________________________________________________________________
MSE_TEST_RF<- sqrt(mean((Pred.modelo.rf.ref1 - Test_RF_ref$PPTO._TOTAL)^2))
MSE_TRAIN_RF <- sqrt(mean((Pred.modelo.rf.ref2 - Entrenamiento_RF_ref$PPTO._TOTAL)^2))

MSE_TEST_RF
MSE_TRAIN_RF

#IMPORTANCIA DE VARIABLES______________________________________________________________________________________________
Variables_ref<- varImpPlot(modelo.rf.ref)   #Grafica importancia de las variables.
Variables_ref
l <- length(valores)
valores <- Variables_ref[,1]
Porcentaje <- (valores/(sum(valores)))*100
tabla_valores <- cbind(Variables_ref,Porcentaje)
tabla_ordenada <- tabla_valores[order(Porcentaje, decreasing = TRUE),]
colnames(tabla_ordenada) = c("IncNodePurity","(%)")
tabla_ordenada  #% de importancia de las variables


#GRAFICOS (se hace con test)_________________________________________________________________________________
library("ggplot2")

p <- ggplot(Test_RF_ref, aes(Pred.modelo.rf.ref1, Test_RF_ref$PPTO._TOTAL)) + 
  geom_line(color="blue") +
  labs(y="Predichos") + 
  labs(x="Actuales") + 
  labs(title="Actuales vs. Predichos") +
   geom_smooth(method = "lm")
  p + geom_point(aes(colour = PPTO._TOTAL)) + scale_colour_gradient(low = "blue")



#FALTA TESTEAR CON 2017