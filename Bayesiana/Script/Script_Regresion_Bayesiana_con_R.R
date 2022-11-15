library(readxl)
library(ggplot2)
library(gridExtra)
library(arm)

ruta<-"C:\\Users\\Lenovo\\Documents\\GitHub\\Proyecto\\Bayesiana\\Datos\\ENB2012_data.xlsx"

datos <- read_excel(ruta) # Carga del archivo de datos

datos <- data.frame(datos) # Identificación de la base de datos como un data frame

head(datos) # Muestra

attach(datos) 

g1 <- ggplot(data = datos, aes(x = X1, y = Y1)) + geom_point()
g2 <- ggplot(data = datos, aes(x = X2, y = Y1)) + geom_point()
g3 <- ggplot(data = datos, aes(x = X3, y = Y1)) + geom_point()
g4 <- ggplot(data = datos, aes(x = X4, y = Y1)) + geom_point()
g5 <- ggplot(data = datos, aes(x = X5, y = Y1)) + geom_point()
g6 <- ggplot(data = datos, aes(x = X6, y = Y1)) + geom_point()
g7 <- ggplot(data = datos, aes(x = X7, y = Y1)) + geom_point()
g8 <- ggplot(data = datos, aes(x = X8, y = Y1)) + geom_point()

grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8, nrow = 2, ncol = 4) 

detach(datos) 

cor.spearman <- cor(datos[,1:8], datos[,9], method = "spearman")

cor.spearman

datos <- datos[, c(1,2,3,4,5,7,9)]

set.seed(500) # Establecimiento de la semilla
muestra <- sample.int(n=nrow(datos), size = nrow(datos)*0.2, replace=T) # Tamaño de la muestra

datos.test <- datos[muestra,] # Test data
datos.train <- datos[-muestra,] # Training data

x.test <- datos.test[, 1:6] # Variables regresoras en el test data
y.test <- datos.test[, 7] # Variable de respuesta en el test data

mod.bayes <- bayesglm(Y1 ~ X1 + X2 + X3 + X5 + X7, family=gaussian(link=identity), data=datos.train, prior.df = Inf, prior.mean = 0, prior.scale = NULL, maxit = 10000) # Modelo de regresión lineal Bayesiana

summary(mod.bayes)

attach(datos.train)

mod.ols <- lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X7, data = datos.train) # Modelo lineal

summary(mod.ols) # Resumen del modelo

mod.coef <- mod.ols$coefficients # Coeficientes del modelo

ypred.ols <- predict.lm(mod.ols,x.test,interval = "prediction",se.fit = T) # Valores de predicción considerando el modelo lineal

head(ypred.ols$fit) # Muestra de los intervalos de predicción

yout.ols <- as.data.frame(cbind(y.test,ypred.ols$fit)) # Data frame con los valores de respuesta, la estimación y el intervalo de predicción
ols.upr <- yout.ols$upr # Límite superior del intervalo de predicción
ols.lwr <- yout.ols$lwr # Límite inferior del intervalo de predicción

mod.bayes <- bayesglm(Y1 ~ X1 + X2 + X3 + X5 + X7, family=gaussian(link=identity), data=datos.train, prior.df = Inf, prior.mean = 0, prior.scale = NULL, maxit = 10000) # Modelo de regresión lineal Bayesiana

summary(mod.bayes)
ypred.bayes <- predict.glm(mod.bayes, newdata = x.test, se.fit = T) # Función para la predicción
head(ypred.bayes$fit) # Muestra de valores predichos con el modelo Bayesiano


yout.bayes <- as.data.frame(cbind(y.test, ypred.bayes$fit)) # Data frame con los valores de respuesta y los valores predichos
names(yout.bayes) <- c("ytest","fit") # Cambio de nombre de las columnas del data frame
critval <- 1.96 # Valor crítico
bayes.upr <- ypred.bayes$fit + critval * ypred.bayes$se.fit # Límite superior de los intervalos
bayes.lwr <- ypred.bayes$fit - critval * ypred.bayes$se.fit # Límite inferior de los intervalos
p.ols <- ggplot(data = yout.ols, aes(x = y.test, y = fit)) + geom_point() + ggtitle("Regresión ordinaria") + labs(x = "Y-Prueba", y = "Y-Predicción") # Valores de prueba vs. valores predichos - OLS

p1 <- p.ols + geom_errorbar(ymin = ols.lwr, ymax = ols.upr) # Representación de los intervalos - OLS

p.bayes <- ggplot(data = yout.bayes, aes(x = y.test, y = fit)) + geom_point() + ggtitle("Regresión Bayesiana") + labs(x = "Y-Prueba", y = "Y-Predicción") # Valores de prueba vs. valores predichos - Bayes

p2 <- p.bayes + geom_errorbar(ymin = bayes.lwr, ymax = bayes.upr) # Representación de los intervalos - OLS

grid.arrange(p1,p2,ncol = 2)

sim(mod.bayes) # Funcionamiento de la función sim

posterior.bayes <- as.data.frame(coef(sim(mod.bayes))) # Almacenamiento de los resultados a posteriori

head(posterior.bayes) # Muestra del data frame anterior

attach(posterior.bayes)

h0 <- ggplot(data = posterior.bayes, aes(x = posterior.bayes$`(Intercept)`)) + geom_histogram() + labs(x = "Intercepto")
h1 <- ggplot(data = posterior.bayes, aes(x = X1)) + geom_histogram()
h2 <- ggplot(data = posterior.bayes, aes(x = X2)) + geom_histogram()
h3 <- ggplot(data = posterior.bayes, aes(x = X3)) + geom_histogram()
h5 <- ggplot(data = posterior.bayes, aes(x = X5)) + geom_histogram()
h7 <- ggplot(data = posterior.bayes, aes(x = X7)) + geom_histogram()

grid.arrange(h0,h1,h2,h3,h5,h7, nrow = 2, ncol = 3)

detach(posterior.bayes)