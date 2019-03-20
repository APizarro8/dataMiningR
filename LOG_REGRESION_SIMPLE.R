#################################################################################################################
#################################### REGRESION POTENCIAL  #######################################################

# ==============================================================================================================

# ==============================================================================================================
# ENTRADA DE DATOS REGRESION POTENCIAL
# ==============================================================================================================

##ENTRADA PARAMETROS DE EVALUACION
datos <- read.csv("D:/TFM_BIOMASA/INVENTARIOS_FORESTALES/BIOMASA/AGB_16Z2/AGB_16Z2_X.csv", sep=";")
dim (datos)

##Modelo de regresion potencial
### Modificaré la regresion por esta nueva definición
potencial = lm(log(Bio)~log(X), data = datos)

#Prediccion
#Ajustar variables entrada
pred.lm = predict ( potencial, newdata=datos )

#Evaluacion
#Ajustar variables entrada
EvalRegr ( datos$Bio, pred.lm )

print(summary(potencial))


# ==========================================================================================================
# FUNCION PARA ESTABLECER LOS CALCULOS DE LOS PARAMETROS.
# ==========================================================================================================

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


# ENTRADA GRAFICOS DE DIAGNOSTICO
#Ajustar variables entrada
DiagPlot ( datos$Bio, pred.lm , main="BIOMASA (ton/ha)" )

##Otra forma grafica
#Ajustar variables entrada
dev.new()
ggplot(datos, aes(x=log(Bio), y=log(X)) +
  geom_point(shape=1) +   
  geom_smooth(method=lm)   # adjunta la linea de regresion por defecto es al 95% de confianza 



################################### FINISHED RUN ###############################################################
################################################################################################################