#################################################################################################################
############################## REGRESION POR PASOS STEPWISE ##########################################·

#Descargar librerias
library(leaps)

# ==============================================================================================================
# ENTRADA DE DATOS PARA LA SELECCION PASO A PASO  
# ==============================================================================================================

##Lectura de datos 
datos <- read.csv("D:/TFM_BIOMASA/INVENTARIOS_FORESTALES/BIOMASA/AGB_16Z2/AGB_16Z2_X_FCC.csv", sep=";")
names(datos)

##Formula RLM
regresion <- lm( Bio ~ X + FCC, data = datos)
summary(regresion)

##Analisis de la tabla de varianza (ANOVA)
anova(regresion)

##SELECCION PASO A PASO 
step(object = regresion, direction = "both", trace = 1)

################################### FINISHED RUN ################################################################
#################################################################################################################
