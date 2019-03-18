########################################################################################################
############################ REGRESION LINEAL SiMPLE DE VARIABLES ######################################

library(ggplot2)

## Start processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# =====================================================================================================
# ENTRADA DE DATOS PARA LA REGRESION LINEAL 
# =====================================================================================================

##ENTRADA DATOS REGRESION LINEAL
datos <- read.csv("D:/TFM_BIOMASA/INVENTARIOS_FORESTALES/BIOMASA/AGB_10Z2/AGB_10Z2_X.csv", sep=";")
names(datos)

##Modelo de regresion lineal
#Ajustar variables entrada
datos <- lm( log(Bio) ~ log(X) , data = datos)
summary(datos)

##Analisis de la tabla de varianza
anova(datos)

# =====================================================================================================
  
##ENTRADA PARAMETROS DE EVALUACION
datos <- read.csv("D:/TFM_BIOMASA/INVENTARIOS_FORESTALES/BIOMASA/AGB_10Z2/AGB_10Z2_X.csv", sep=";")
dim (datos)

##Modelo de regresion lineal
#Ajustar variables entrada
lm.mod <- lm(Bio ~ X , data = datos)

#Prediccion
pred.lm = predict ( lm.mod, newdata=datos )

#Evaluacion
#Ajustar variables entrada
EvalRegr ( datos$Bio, pred.lm )

# =====================================================================================================

## ENTRADA GRAFICOS DE DIAGNOSTICO
#Ajustar variables entrada
DiagPlot ( datos$Bio, pred.lm , main="BIOMASA (ton/ha)" )

##Otra forma grafica
#Ajustar variables entrada
dev.new()
ggplot(datos, aes(x=Bio, y=X)) +
  geom_point(shape=1) +   
  geom_smooth(method=lm)   # adjunta la linea de regresion por defecto es al 95% de confianza 


# =====================================================================================================
# FUNCION PARA ESTABLECER LOS CALCULOS DE LOS PARAMETROS DE EVALUACION
# =====================================================================================================

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

# ====================================================================================================
# FUNCION PARA LOS GRAFICOS DE DIAGNOSTICO 
# ====================================================================================================

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


################################### FINISHED RUN ####################################################
####################################################################################################