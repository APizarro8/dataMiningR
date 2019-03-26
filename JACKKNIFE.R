###############################################################################################################
########################################## VALIDACION JACKNIFE ################################################
##PRUEBA NO PARAMETRICA
##Remuestreo de datos que permite resolver problemas relacionados con la estimación de intervalos de confianza. 

#Descargar librerias
library(bootstrap)

# Start processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# ===========================================================================================================
# ENTRADA DE DATOS PARA LA VALIDACION BOOTRAPING 
# ===========================================================================================================

##Lectura de datos 
datos <- read.csv("xxxxDATAxxxx", sep=";")
names(datos)

x <- rnorm(17,mean = 3,sd = 1)
y <- rnorm(17,mean = 3,sd = 1)
xy <- c(x,y)
factor <- c(rep('A',17),rep('B',17))

DF <-data.frame(datos)
DF$PAIR_IDENTIFIER <- 1:17

paired_t_test <- t.test(xy ~ factor, DF, paired = T)
paired_t_test$p.value 

n <- length(DF$xy)

theta.fun <- function(x, mydata) {
  t.test(xy ~ factor, 
         data = mydata[mydata$PAIR_IDENTIFIER %in% X, ], 
         paired = T)$p.value
}

jackknife(x = 1:17, theta = theta.fun, mydata = DF)


#
#
# Calculando tiempo de procesamiento
timeDiff <- Sys.time() - startTime
cat("\nEl tiempo de procesamiento es de ", format(timeDiff), "\n")


############################################## FINISHED RUN ################################################
###########################################################################################################
