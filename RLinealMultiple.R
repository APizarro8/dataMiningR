###################################################################################################################
############################## REGRESION LINEAL MULTIPLE DE VARIABLES #############################################

#Descargar librerias
library(ggplot2)
library(car)

## Start processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# ================================================================================================================
# ENTRADA DE DATOS PARA LA REGRESION LINEAL 
# ================================================================================================================

##Lectura de datos 
datos <- read.csv("XXXXXXX.csv", sep=";")
names(datos)
pairs(datos)


##Formula RLM
regresion <- lm( Bio ~ HVAR + FCC, data = datos)
summary(regresion)
 
##Analisis de la tabla de varianza
anova(datos)

# ================================================================================================================

##ENTRADA PARAMETROS DE EVALUACION
datos <- read.csv("XXXXXXX.csv", sep=";")
dim (datos)

##Modelo de regresion lineal
#Ajustar variables entrada
lm.mod <- lm ( VARDEP ~ VARINDEP + VARINDEP, data = datos)

#Prediccion
#Ajustar variables entrada
pred.lm = predict ( lm.mod, newdata=datos )

#Evaluacion
#Ajustar variables entrada
EvalRegr ( datos$VARDEPEN, pred.lm )


print(summary(lm.mod))


# ===================================================================================================================

## ENTRADA GRAFICOS DE DIAGNOSTICO
##Grafico de diagnostico
DiagPlot ( datos$VARDEPEN, pred.lm , main="XXXXXXX" )


# ===================================================================================================================
# FUNCION PARA ESTABLECER LOS CALCULOS DE LOS PARAMETROS.
# ===================================================================================================================

##Funcion para evaluar los modelos (Var. respuesta cuantitativas)
EvalRegr = function ( obs, pred )
{
  ##Deben tener la misma longitud
  if ( length(obs) != length(pred) ) #Obs y pred son los parametros de la funcion
  { stop ("Los vectores deben de tener la misma longitud") }
  
  ##MSE Y RSME
  mse = mean ( ( obs - pred ) **2 )
  rmse = sqrt ( mse)
  
  ##Error Estandar del RSE
  ##El SE se calcula como el SD
  se.mse = sd ( ( obs - pred) ** 2 ) / sqrt(length(obs))
  
  ##MAE
  mae =  mean ( abs (obs - pred ) )
  
  ##R2
  R2 = cor ( obs, pred ) ** 2
  
  ##Parametros de salida
  list( MSE = mse, RMSE = rmse, MAE = mae, R2 = R2, SE.MSE = se.mse )
}

# ==================================================================================================================
# FUNCION PARA LOS GRAFICOS DE DIAGNOSTICO 
# ==================================================================================================================

##Graficos de diagnostico de modelos de regresion (Var. respuesta cuantitativas).
DiagPlot = function ( obs, pred, ... ) 
{
  ##Deben tener la misma longitud
  if ( length(obs) != length(pred) )
  { stop ("Los vectoreS deben de tener la misma longitud") }
  
  ##Residuos
  res = obs - pred
  
  ##Grafico delos valores observados frente a los predichos
  w.min = min ( pred, obs )
  w.max = max ( pred, obs )

  dev.new()
  plot ( pred, obs,
         xlim = c( w.min, w.max ), ylim = c(w.min, w.max ),
         xlab="PREDICHO", ylab="OBSERVADO", pch=16, ... )
  abline ( b=1, a=0 )
}


#
#
#Calculando tiempo de procesamiento
timeDiff <- Sys.time() - startTime
cat("\nEl tiempo de procesamiento es de ", format(timeDiff), "\n")

####################################### FINISHED RUN ################################################################
#####################################################################################################################
