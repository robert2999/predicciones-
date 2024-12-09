# Cargar la base de datos con encabezado
datiño<- read.csv("creditg.csv", header = TRUE)

# Renombrar las columnas
colnames(datiño) <- c("estado_de_comprobación", "duración", "historial_de_credito", 
                                "propósito", "monto_de_crédito", "estado_de_ahorro", 
                                "empleo", "compromiso_de_instalación", "estado_personal", 
                                "otras_partes", "residencia_desde", "magnitud_propiedad", 
                                "edad", "otros_planes_de_pago", "vivienda", 
                                "créditos_existentes", "trabajo", "num_dependientes", 
                                "teléfono_propio", "trabajador_extranjero", "clase")

# Convertir variables cualitativas nominales en factores
datiño$estado_de_comprobación <- factor(datiño$estado_de_comprobación)
datiño$historial_de_credito <- factor(datiño$historial_de_credito)
datiño$propósito <- factor(datiño$propósito)
datiño$estado_de_ahorro <- factor(datiño$estado_de_ahorro)
datiño$estado_personal <- factor(datiño$estado_personal)
datiño$otras_partes <- factor(datiño$otras_partes)
datiño$magnitud_propiedad <- factor(datiño$magnitud_propiedad)
datiño$otros_planes_de_pago <- factor(datiño$otros_planes_de_pago)
datiño$vivienda <- factor(datiño$vivienda)
datiño$trabajo <- factor(datiño$trabajo)
datiño$teléfono_propio <- factor(datiño$teléfono_propio)
datiño$trabajador_extranjero <- factor(datiño$trabajador_extranjero)
datiño$clase <- factor(datiño$clase)


datiño$empleo <- factor(datiño$empleo, levels = c("'unemployed'", "'1<=X<4'", "'4<=X<7'", "'>=7'"))
datiño$estado_de_comprobación <- factor(datiño$estado_de_comprobación, levels = c("'no checking'", "'0<=X<200'", "'<0'", "'>=200'"))
datiño$estado_de_ahorro <- factor(datiño$estado_de_ahorro, levels = c("'no known savings'", "'<100'", "'100<=X<500'", "'500<=X<1000'", "'>=1000'"))


# Verificar los cambios
str(datiño)
# Eliminar todas las filas que contengan NA (originalmente "None")
datiño <- na.omit(datiño)



# modelamiento de la ecuacion
# para predecir el monto de credito que le aprobaremos debe estar en funcion 
# estado de comprobacion,duracion, historial crediticio,proposito, 
# moto de credito, estado personal,empleo, otras partes, residencia desde, 
# magnitud de la propiedad, compromiso de instalacion, vivienda,edad
#creditos existentes ,trabajo,num dependientes, telefono propio, trabajador extranjero
# Imputar los NA con el promedio de la fila solo en columnas numéricas
# 

# Ver los datos resultantes





prueb1<-lm(monto_de_crédito~estado_de_comprobación+duración+historial_de_credito+propósito
           +estado_personal+otras_partes+empleo+residencia_desde+magnitud_propiedad+edad+
             compromiso_de_instalación+num_dependientes+trabajador_extranjero+clase,data=datiño )
prueb1
summary(prueb1)

# SELECCIÓN DE VARIABLES       -------------------------------------------
library(MASS)
modelo_prue <- stepAIC(prueb1, direction = "backward")
modelo_prue
summary(modelo_prue)
#modelo ajustado
modelo_ajustado<-lm(formula = monto_de_crédito ~ estado_de_comprobación + duración + 
                      historial_de_credito + propósito + estado_personal + magnitud_propiedad + 
                      compromiso_de_instalación + num_dependientes + clase, data = datiño)
# analisis de supuestos
#1 normalidad de errores
library(ggfortify)
autoplot(modelo_prue)
# Residuales del modelo lineal final ajustado(extraigo los residuos estandarizados)
residualesss <- rstandard(modelo_prue)
# QQ-Plot
qqnorm(residualesss)
qqline(residualesss)
# Gráfico Q-Q Plot con intervalos de confianza
library(car)
qqPlot(residualesss)


# Prueba de Normalidad
# Prueba de Anderson-Darling
library(nortest)
ad.test(residualesss)

# Prueba de Shapiro-Wilk
shapiro.test(residualesss)
#dada la evaluacion visual y dada la verificacion de hipotesis no 
#no habria sustento para que su supuesto no sea de normalidad

# transformacion de BOX-COX
install.packages("MASS")
library(MASS)
# Buscamos una variable y nueva, la cual sea Y^lambda 
BX<-boxcox(prueb1,lambda =seq(-10,10,lenght=0.01))
lambda<-BX$x[which.max(BX$y)]
lambda
datiño$monto_de_crédito<-with(datiño,monto_de_crédito^lambda)
# prueba de homosteacidad
library(lmtest)
bptest(modelo_prue)
# prueba multicolinialidad
library(car)
vif(modelo_ajustado)


# regresion logica:
# ANÁLISIS EXPLORATORIO ---------------------------------------------------
# Diagrama de cajas (Cuantitativa y Cualitativa)
boxplot(edad~historial_de_credito, data = datiño)
boxplot(duración~propósito, data = datiño)
boxplot(monto_de_crédito~estado_personal, data = datiño)
prop.table(table(datiño$historial_de_credito,datiño$clase), margin = 2)
prop.table(table(datiño$estado_de_comprobación,datiño$clase), margin = 2)
prop.table(table(datiño$propósito,datiño$clase), margin = 2)

# Convertir a character temporalmente
datiño$clase <- as.character(datiño$clase)

# Reemplazar "good" con 1 y "bad" con 0
datiño$clase[datiño$clase == "good"] <- 1
datiño$clase[datiño$clase == "bad"] <- 0

# Convertir la columna a numeric
datiño$clase <- as.numeric(datiño$clase)


logico<-glm(clase~estado_de_comprobación+duración+monto_de_crédito+propósito+estado_de_ahorro+
             otras_partes+ magnitud_propiedad+estado_personal+créditos_existentes +trabajo+num_dependientes+teléfono_propio,family = "binomial",data = datiño)
logico
# INTERPRETACIÓN DE LOS COEFICIENTES --------------------------------------
# ===
# ODDS RATIO = e^(beta_j)
exp(coef(logico))
exp(cbind(OR = coef(logico), confint(logico, level = 0.95)))
# SIGNIFICANCIA DEL MODELO GLOBAL -----------------------------------------

# Mostrar los resultados del modelo
summary(logico)
# Prueba de Razón de Verosimilitud

# X2_0 = 893.91 - 680.19 = 213.7185
r<-logico$null.deviance - logico$deviance
# Grados de libertad
gr<-logico$df.null - logico$df.residual
# p-value
log_p_value <- pchisq(r, gr, log.p = TRUE, lower.tail = FALSE)
# Convertir el logaritmo del valor p al valor p original
p_value <- exp(log_p_value)
p_value
# Cálculo del valor p directamente
p_value <- pchisq(r, gr, lower.tail = FALSE)
p_value


# como p-value en el r lo aproxima a 0 , porque es una muestra muy grande , se puede decir que tiene un ajuste significativo
#BONDAD DE AJUSTE --------------------------------------------------------
  # Pseudo-R2
  # # En RL no existe un valor que indique la “variabilidad explicada” como en la RLM.
  # Se puede calcular un valor similar a R^2 de los modelos lineales mediante el indicador llamado Pseudo R2 o también llamado McFadden
  # Los valores oscilan entre 0 y 1.
  # Pseudo-R2 que oscila entre 0.2 y 0.4 indica un muy buen ajuste del modelo.
  # https://cowles.yale.edu/sites/default/files/files/pub/d04/d0474.pdf
  
  # Cálculo de Pseudo-R2
  (logico$null.deviance - logico$deviance)/logico$null.deviance

# Mediante paquete
library(pscl)
pR2(logico)
# se puede conlcuir que tenemos una alta significancia 
# SIGNIFICANCIA INDIVIDUAL ------------------------------------------------
# Mostrar los resultados del modelo
summary(logico)

# H0: Beta1 = 0
# H1: Beta2 != 0

# p-value = 0.007928

# Como p-value < alpha = 0.05 => Se rechaza la H0.
