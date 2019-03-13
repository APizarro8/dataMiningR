#################################################################################################################
############################## REGRESION POR PASOS STEPWISE ##########################################·

#Descargar librerias
library(leaps)

# ==============================================================================================================
# ENTRADA DE DATOS PARA LA SELECCION PASO A PASO  
# ==============================================================================================================

##Lectura de datos 
datos <- read.csv("XXXXXX.csv", sep=";")
names(datos)

##Formula RLM
regresion <- lm( VarDepen ~ VarIndep + VarIndep, data = datos)
summary(regresion)

##Analisis de la tabla de varianza (ANOVA)
anova(regresion)

##SELECCION PASO A PASO 
step(object = regresion, direction = "both", trace = 1)

################################### FINISHED RUN ################################################################
#################################################################################################################
