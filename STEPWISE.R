#################################################################################################################
################################### SELECCION POR PASOS STEPWISE ################################################

#Introduce la seleccion de variables hacia delante, pero en cada etapa platea si todas las variables
#introducidas en el modelo superan el test y si no las elimina. 

#Descargar librerias
library(leaps)

# Start processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

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

#
#
# Calculando tiempo de procesamiento
timeDiff <- Sys.time() - startTime
cat("\nEl tiempo de procesamiento es de ", format(timeDiff), "\n")

################################################## FINISHED RUN #################################################
#################################################################################################################
