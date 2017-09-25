prod <- read.table("http://www.stat.tamu.edu/~sheather/book/docs/datasets/production.txt", header = TRUE)
attach(prod)
RunTime

par(mfrow = c(1,1))
plot(RunSize, RunTime, xlab = "Run size",ylab = "Run Time")

m1 <- lm(RunTime~RunSize)
m1
summary(m1)

abline(lsfit(RunSize, RunTime))

# Funcion = 149.74770 + 0.25924 *x
149.74770 + 0.25924 * 300
points(300, 227.5197, pch = 15, col = "red")

estimateTime <- function(x){
    res <- 149.74770 + 0.25924 * x
    print(res)
    abline(x, res)
    points(x,res, pch = 15, col = "red")
}

estimateTime(220)



# Otro Ejemplo ------------------------------------------------------------

grasas <- read.table("http://www.uam.es/joser.berrendero/datos/EdadPesoGrasas.txt", header = TRUE)
grasas
head(grasas)
names(grasas)
pairs(grasas) # Grafico de correlacion
cor(grasas)
# edad depende de grasas
# grasas variable dependiente
# edad variable independiente
regresion <- lm(grasas ~ edad, data = grasas)
summary(regresion)
# anova, muestra una correlacion diferente de 0
# 1.79e-07 ***

plot(grasas$edad, grasas$grasas)
# Multiple R-squared:  0.7012 (70% explicado)
abline(regresion)
par(mfrow=c(2,2))
plot(lm(grasas ~ edad, data = grasas))
plot(lm(grasas ~ edad, data = grasas))

grasas_reducidas <- grasas[-c(6,8,16)]
regresion2 <- lm(grasas, edad, data = grasas_reducidas)
plot(lm(grasas ~ edad, data = grasas_reducidas))

summary(regresion2)
anova(regresion2)

nuevas_edades <- data.frame(edad = seq(30,35))
nuevas_edades

predict(regresion, nuevas_edades)
predict(regresion,data.frame(nuevas_edades), level = 0.95, interval = "confidence")

confint(regresion, level = 0.95)
confint(regresion, level = 0.99)

plot(grasas$edad, grasas$grasas)
abline(regresion)

# Intervalos de confianza de prediccion
ic <- predict(regresion, nuevas_edades, interval = "confidence")
ic
nuevas_edades <- data.frame(edad = seq(20,60))
lines(nuevas_edades$edad, ic[,2], lty = 5, col ="blue")
lines(nuevas_edades$edad, ic[,3], lty = 5, col ="blue")

ic <- predict(regresion, nuevas_edades, interval = "prediction")
lines(nuevas_edades$edad, ic[,2], lty = 5, col ="red")
lines(nuevas_edades$edad, ic[,3], lty = 5, col ="red")



# otro ejemplo ------------------------------------------------------------
install.packages("car")
library(car)
outlierTest(regresion)
influenceIndexPlot(regresion,id.n = 3)
influencePlot(regresion, id.n = 3)
qqPlot(regresion)
ncvTest(regresion)



# Ejemplo Resumido --------------------------------------------------------

grasas <- read.table("http://www.uam.es/joser.berrendero/datos/EdadPesoGrasas.txt", header = TRUE)
grasas
names(grasas)
pairs(grasas)
cor(grasas)
regresion <- lm(grasas ~ edad, data = grasas)
anova(regresion)
plot(grasas$edad, grasas$grasas)
abline(regresion)
par(mfrow=c(2,2))
plot(lm(grasas ~ edad, data = grasas))
nuevas.edades <- data.frame(edad = seq(30,35))
nuevas.edades
predict(regresion, nuevas.edades)
predict(regresion, data.frame(nuevas.edades), level = 0.95, interval="confidence")

confint(regresion, level = .95)
confint(regresion, level = .99)

plot(grasas ~ edad, data = grasas)
abline(regresion)
nuevas.edades <- data.frame(edad = seq(20,60))
ic <- predict(regresion, nuevas.edades, interval = "confidence")
ic
lines(nuevas.edades$edad, ic[,2], ltv = 5, col = "blue")
lines(nuevas.edades$edad, ic[,3], ltv = 5, col = "blue")

ic <- predict(regresion, nuevas.edades, interval = "prediction")
lines(nuevas.edades$edad, ic[,2], ltv = 5, col = "red")
lines(nuevas.edades$edad, ic[,3], ltv = 5, col = "red")











