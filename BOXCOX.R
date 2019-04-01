################################################################################################################
################################################# BOXCOX ######################################################

##LIBRERIAS 
library(car)
library (MASS)
require(nnet)
library(labstatR)  #calcula la media geometrica

# Start processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# ==================================================================================================
# ENTRADA DE DATOS PARA LA VALIDACION BOOTRAPING 
# ==================================================================================================

##Lectura de datos 
datos <- read.csv("xxxxDATAxxxx", sep=";")
names(datos)

##Formula RLM
regresion <- lm( varDep ~ varIndep + varIndep + varIndep, data = datos)
summary(regresion)

# y un test de normalidad
shapiro.test(rstandard(regresion))

# y un test de HOMOCEDATICIDAD
bptest(regresion, studentize = TRUE, data = list())

## ANALISIS DE INFLACION DE VARIANZA (VIF)
vif(regresion)

# Buscamos el valor de lambda para la transformacion:
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
regresion.bc <-lm ( varDepAdj ~ varIndep1 + varIndep2, data=datos)

# cuyo diagn ́ostico gr ́afico resultante es:
opar<-par(mfrow=c(2,2))
summary(regresion.bc)

##ANALISIS DE INFLACION DE VARIANZA (VIF)
vif(regresion.bc)

# y un test de HOMOCEDATICIDAD
bptest(regresion.bc, studentize = TRUE, data = list())

# y un test de normalidad
shapiro.test(rstandard(regresion.bc))

#
#
# Calculando tiempo de procesamiento
timeDiff <- Sys.time() - startTime
cat("\nEl tiempo de procesamiento es de ", format(timeDiff), "\n")

######################################### FINISHED RUN ##############################################
#####################################################################################################
