#############################################################################
#Descargar librerias
require(corrplot)
library(pspearman)

#############################   ANALISIS COMPONENTES PRINCIPALES ###################################

##Variables LiDAR a correlacionar
madrid_10Z1 <- read.csv("D:/TFM_BIOMASA/INVENTARIOS_FORESTALES/BIOMASA/AGB_10Z1/AGB_10Z1_spearman
                        .csv", sep=";")
dim (madrid_10Z1)
head (madrid_10Z1)

cor(rank(madrid_10Z1[,1]),rank(madrid_10Z1[,2]))

################################### FINISHED RUN ########################################
#########################################################################################