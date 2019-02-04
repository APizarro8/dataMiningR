###################################################################################################
#
##LIBRERIAS
require(corrplot)

# Start processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# =================================================================================================
# ANALISIS DE COMPONENTES PRINCIPALES (ACP)
# =================================================================================================

##DATOS DE ENTRADA 
datos <- read.csv("XXXdataXXXX", sep=";")
dim (madrid_10Z1)
head (madrid_10Z1)

##CORRELACION DE VARIABLES PREDICTORAS CONTINUAS 
cor.matr = cor(madrid_10Z1[c(3:29)], method= c("pearson"))
round (cor.matr, 4)
write.csv(cor.matr, "D:/TFM_BIOMASA/FUSION/Correlaciones/Pearson_10Z1.csv")

#
#
# Calculando tiempo de procesamiento
timeDiff <- Sys.time() - startTime
cat("\nEl tiempo de procesamiento es de ", format(timeDiff), "\n")

############################################## FINISHED RUN ########################################
####################################################################################################
