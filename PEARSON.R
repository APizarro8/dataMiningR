#############################################################################
#Descargar librerias
require(corrplot)

#############################   ANALISIS COMPONENTES PRINCIPALES ###################################

##Variables LiDAR a correlacionar
madrid_10Z1 <- read.csv("D:/TFM_BIOMASA/FUSION/CloudMetrics/CloudMetrics_10Z1_0.15m.csv", sep=";")
dim (madrid_10Z1)
head (madrid_10Z1)

##Correlacion de variables predictoras continuas
cor.matr = cor(madrid_10Z1[c(3:29)], method= c("pearson"))
round (cor.matr, 4)
write.csv(cor.matr, "D:/TFM_BIOMASA/FUSION/Correlaciones/Pearson_10Z1.csv")
