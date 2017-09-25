dados <- c(1,2,5,3,6,4,2,1,2,4,1,5,3,2,4,1,6,2,3,1,6,2,4,2,1)
# Calculo de frecuencia absoluta

dados <- c(1,2,5,3,6,4,2,1,2,4,1,5,3,2,4,1,6,2,3,1,6,2,4,2,1)

table(x)
table(dados)

# dados
# 1 2 3 4 5 6 
# 6 7 3 4 2 3 
# > 


# Calculo frecuencia relativa
table(dados) / length(dados) 
# dados
# 1    2    3    4    5    6 
# 0.24 0.28 0.12 0.16 0.08 0.12 
# > 

# Calculo del total de la variable
addmargins(table(dados))
# dados
# 1   2   3   4   5   6 Sum 
# 6   7   3   4   2   3  25 

# Calculo Frecuencia absoluta y el total de la variable
addmargins(table(dados)/length(dados))
# dados
# 1    2    3    4    5    6  Sum 
# 0.24 0.28 0.12 0.16 0.08 0.12 1.00 

# Calculo Frecuencia relativa y el total de la variable
cumsum(table(dados))
# 1  2  3  4  5  6 
# 6 13 16 20 22 25 

# Calculo Frecuencia absoluta acumulada
cumsum(table(dados)/length(dados))
# 1    2    3    4    5    6 
# 0.24 0.52 0.64 0.80 0.88 1.00

x <- scan()
12
13
12
13
12
15
14
12
13
15

x
# [1] 12 13 12 13 12 15 14 12 13 15

summary(x)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 12.00   12.00   13.00   13.10   13.75   15.00 

x = c(1:5, 5:1, 4,2,3,2)
x
fivenum(x)
# [1] 1 2 3 4 5

install.packages("pastecs")
library(pastecs)

edad <- c(22,22,23,24,26,27,28,29,29,29,
          31,33,34,35,35,35,36,38,39,42,
          44,44,45,45,45,47,48,52,59,66,67,69,69)
stat.desc(edad)
# > stat.desc(edad)
# nbr.val     nbr.null       nbr.na          min          max        range 
# 33.0000000    0.0000000    0.0000000   22.0000000   69.0000000   47.0000000 
# sum       median         mean      SE.mean CI.mean.0.95          var 
# 1317.0000000   36.0000000   39.9090909    2.4118409    4.9127592  191.9602273 
# std.dev     coef.var 
# 13.8549712    0.3471633 


# Medidas de tendencia central --------------------------------------------

# Media Aritmética

X <- c()
data.entry(x)
x <- c(12)
x
data.entry(x)
mean(x)

x <- c(1:5,4:1,3)
x
 # Calculamos la media geometrica calculando la media del logaritmo
mean(log(x))
# [1] 0.9064158
# para regresar al valor del la media original:
exp(mean(log(x)))
# [1] 2.475434

# Calculo de moda
max(table(edad))
install.packages("prettyR")
library(prettyR)
Mode(edad)
x <- c(1,2,3,4,5,5,3,2,1,3)
x
Mode(x)

# Vierifcar si son continuos o discretos
modad <- function(x) as.numeric(names(which.max(table(x))))
modac <- function(x){
    dd <- density(x)
    dd$x[which.max(dd$y)]
}

# x es discreta
x <- rpois(100,10)
x
modad(x)
table(x)

# x es continua

x <- rgamma(100, 3, 4)
x
modac(x)
dd <- density(x)
dd
plot(dd, type = 'l', las =1)
rug(x)
abline(v = dd$x[which.max(dd$y)], col = 2)



# Mediana -----------------------------------------------------------------

# Mediana = Percentil 50, 2do quartil, quinto decil

median(edad)
summary(edad)


# Quantiles ---------------------------------------------------------------

# 1 y 3 er cuartil
quantile(edad, probs = 0.25)
quantile(edad, probs = 0.75)
quantile(edad, probs = 0.3)


# Medidas de dispercion ---------------------------------------------------

range(edad)
summary(edad)

min(edad)
max(edad)
rango <- c(min(edad), max(edad))
rango
# Rango Medio
range(edad)
range(edad) -> rg
mean(rg)

# Rango Intercuartilico
quantile(edad,.75)-quantile(edad,.25)



# Varianza ----------------------------------------------------------------

var(edad)
sum((edad-mean(edad))^2)/length(edad)
sum((edad-mean(edad))^2)/(length(edad)-1)

# Poblacional
n <- length(edad)
var(edad)*(n-1)/n



# Desviacion typica -------------------------------------------------------
# Pag 86
sd(edad)
sqrt(var(edad))
# Poblacional
sd(edad)*(n-1)/n 
sqrt(var(edad))*(n-1)/n
# Pag 87
