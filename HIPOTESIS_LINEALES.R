###########################################################################################################
#### COMPROBACION DE LOS SUPUESTOS DE LA HIPOTESIS LINEAL  ##################################################

#Descargar librerias
library(ggplot2)
library(car)
library(carData)
library(lmtest)
library(corrplot)
library(dplyr)


# ========================================================================================================
# ENTRADA DE DATOS PARA LA REGRESION 
# ========================================================================================================

##Lectura de datos 
datos <- read.csv("D:/TFM_BIOMASA/INVENTARIOS_FORESTALES/BIOMASA/AGB_16Z1/AGB_16Z1_X_FCC.csv", sep=";")
names(datos)


##Modelo de regresion potencial
### Modificaré la regresion por esta nueva definición
regresion = lm(log(Bio) ~ log(X), data = datos)
summary(regresion)

##Analisis de la tabla de varianza
anova(regresion)

#Intervalos de confianza
confint(regresion)
confint(regresion,level = 0.90)



# ========================================================================================================
# Diagnóstico del modelo 
# =======================================================================================================

# Normalidad
## Distribución normal de los residuos 
qqnorm (regresion$residuals)
qqline (regresion$residuals)

shapiro.test(regresion$residuals)


# Homocedasticidad  O IGUALDAD DE VARIANZAS
## variabilidad constante de los residuos
ggplot(data= datos, aes(regresion$fitted.values, regresion$residuals)) +
geom_point() + 
geom_smooth(color= "firebrick", se = FALSE) +
geom_hline(yintercept = 0) +
theme_bw()

bptest(regresion, studentize = TRUE, data = list())

bptest(regresion)

## Matriz de correlacion entre predictores
corrplot(cor(dplyr::select(datos, X)),
         method = "number", tl.col = "black")

##ANALISIS DE INFLACION DE VARIANZA (VIF)
vif(regresion)##AUTOCORRELACION
dwt(regresion, alternative = "two.sided")

#LINEALIDAD (COLINEALIDAD)
## Se define como la existencia de una dependencia lineal entre las variables predictoras
qqnorm (residuos)
qqline (residuos)

##VALORES ATIPICOS
datos$studentized_residual <- rstudent(regresion)
ggplot(data = datos, aes(x = predict(regresion), y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
  # se identifican en rojo observaciones con residuos estandarizados absolutos > 3
  geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
  labs(title = "Distribución de los residuos stud
       entized",
       x = "predicción modelo") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

which(abs(datos$studentized_residual) > 3)


################################### FINISHED RUN ########################################
#########################################################################################




