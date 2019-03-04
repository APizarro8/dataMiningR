## ===================================================================================================
# ANALISIS DE COMPONENTES PRINCIPALES (ACP)
# ====================================================================================================
# 
##TECNICA UTILIZADA PARA DESCRIBIR UN CONJUNTO DE COMPONENTES CON VARIABLES NO CORRELACIONADAS. 
#
##REDUCEN DIMENSIONALIDAD YA QUE DICHOS COMPONENTES SE ORDENAN SEGUN LA CANTIDAD DE VARIANZA ORIGINAL.
#
#
##LIBRERIAS
require(corrplot)

# Start processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

##DATOS DE ENTRADA 
datos <- read.csv("XXXdataXXXX", sep=";")
dim (datos)
head (datos)

##CORRELACION DE VARIABLES PREDICTORAS CONTINUAS 
cor.matr = cor(datos[c(X:X)], method= c("pearson"))
round (cor.matr, 4)
write.csv(cor.matr, "XXXdataXXXX")

#
#
# Calculando tiempo de procesamiento
timeDiff <- Sys.time() - startTime
cat("\nEl tiempo de procesamiento es de ", format(timeDiff), "\n")

############################################## FINISHED RUN ##########################################
######################################################################################################
