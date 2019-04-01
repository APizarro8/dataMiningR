################################################################################################################
############################## TRANSFORMACION LOGARITMICA  ####################################
library(car)
library (MASS)
require(nnet)
library(labstatR)  # librer ́ıa que calcula la media geom ́etrica

# =============================================================================================================
# ENTRADA DE DATOS 
# =============================================================================================================

##Lectura de datos 
datos <- read.csv("D:/TFM_BIOMASA/INVENTARIOS_FORESTALES/BIOMASA/AGB_16Z2/AGB_16Z2_SET_STEP.csv", sep=";")
names(datos)

##Formula RLM
regresion <- lm( Bio ~ HMAX + X + FCC, data = datos)
summary(regresion)

# y un test de normalidad
shapiro.test(rstandard(regresion))

# y un test de HOMOCEDATICIDAD
bptest(regresion, studentize = TRUE, data = list())

##ANALISIS DE INFLACION DE VARIANZA (VIF)
vif(regresion)

# Buscamos el valor de lambda para la transformaci ́on:
par(mfrow=c(1,1))
boxCox(regresion)

# que pinta el valor  ́optimo para lambda y un
# intervalo de confianza al 95%:
bc <- boxCox(regresion,plotit=F)

# el valor de lambda que maximiza SSR es
lambda<-bc$x[which.max(bc$y)]; lambda #=0.3

# Luego reajustamos el modelo con la variable Volume transformada:
# la variable transformada por Box-Cox es:
BioAdj <- (Bio^lambda-1)/(lambda*meang(Bio)^(lambda-1))

# y el nuevo ajuste con dicha variable
regresion.bc <-lm ( BioAdj ~ HMAX + X + FCC , data=datos)

# cuyo diagn ́ostico gr ́afico resultante es:
opar<-par(mfrow=c(2,2))


summary(regresion.bc)

##ANALISIS DE INFLACION DE VARIANZA (VIF)
vif(regresion.bc)


# y un test de HOMOCEDATICIDAD
bptest(regresion.bc, studentize = TRUE, data = list())

# y un test de normalidad
shapiro.test(rstandard(regresion.bc))


################################### FINISHED RUN ########################################
#########################################################################################
