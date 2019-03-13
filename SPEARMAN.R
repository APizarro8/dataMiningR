#####################################################################################################
#Descargar librerias
require(corrplot)
library(pspearman)

#############################   ANALISIS COMPONENTES PRINCIPALES ###################################

##Variables LiDAR a correlacionar
namevar <- read.csv("XXXXX.csv", sep=";")
dim (namevar)
head (namevar)

cor(rank(namevar[,1]),rank(namevar[,2]))

################################### FINISHED RUN ####################################################
#####################################################################################################
