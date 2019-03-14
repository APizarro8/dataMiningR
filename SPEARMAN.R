##########################################################################################

############################# Coeficientes de Spearman ###################################

#TECNICA UTILIZADA PARA VER EL GRADO DE CORRELACION ENTRE LA VARIABLE DEPENDIENTE 
#CON CADA UNA DE LAS VARIABLES INDEPENDIENTES DEL MODELO

#Descargar librerias
require(corrplot)
library(pspearman)

##Variables a correlacionar
namevar <- read.csv("XXXXX.csv", sep=";")
dim (namevar)
head (namevar)

cor(rank(namevar[,1]),rank(namevar[,2]))

################################### FINISHED RUN #########################################
##########################################################################################
