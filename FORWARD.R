##########################################################################################
##########################  REGRESION POR PASOS ##########################################
#
#TECNICA QUE COMIENZA CON UN PREDICTOR (MAYOR R2 PRESENTE) Y PARA CADA UNA DE LAS VARIABLES 
#INDEPENDIENTES BAJO DISTRIBUCION NORMAL, RESULTANDO SER ELEGIDA LA QUE MENOS CONTRIBUYA 
#Y ASI SEGUIDAMENTE HASTA QUE TODAS CONTRIBUYEN POR ENCIMA. 
#
#LIBRERIAS  
library(leaps)

# Start processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# ========================================================================================
# REGRESION POR PASOS HACIA DELANTE (FORWARD)
# ========================================================================================

##DATOS DE ENTRADA 
datos <- read.csv("XXXXXXX", sep=";")

lm.forw = regsubsets ( Y ~ a + b , data = datos, method="forward" )
summ.forw = summary(lm.forw)
summ.forw

#ORDEN VARIABLES SELECCIONADAS
lm.forw$xnames [ lm.forw$vorder ]

##COEFICIENTES DEL MODELO CON x VARIABLES 
coef ( lm.forw, x )

##R2 SIEMPRE AUMENTA
summ.forw$rsq

#SUMA DE RESIDUOS AL CUADRADO SIEMPRE DISMINUYE (SE=RSS/N)
summ.forw$rss

##AdjR2
summ.forw$adjr2

##BIC
summ.forw$bic

##Cp de Mallow (equivalente a AIC)
summ.forw$cp


#
#
# Calculando tiempo de procesamiento
timeDiff <- Sys.time() - startTime
cat("\nEl tiempo de procesamiento es de ", format(timeDiff), "\n")

##############################################FINISHED RUN ##############################
#########################################################################################
