library(ggplot2)
mtcars

# Bar Charts
qplot(
    mtcars$cyl,
    geom = "bar",
    fill = I("blue"),
    colour = I("green"),
    xlab = "Cylinders",
    ylab = "Number of vehicules",
    main = "Cylinders in mtcars",
    alpha = 0.8
)

# Histogram Charts
qplot(
    mtcars$hp,
    geom = "histogram",
    bindwidth = 25,
    fill = I("green"),
    colour = I("blue"),
    xlab = "Horse Power",
    ylab = "Number of Vehicules",
    main = "Horse Power per Vehicule",
    xlim = c(50,350),
    alpha = I(0.4)
)

# Pie Chart
barp <- ggplot(mtcars, aes(x = 1, y = sort(mtcars$carb), fill = sort(
    mtcars$carb))) + 
    geom_bar(stat = "identity")
    print(barp)

barpP <- barp + coord_polar(theta = 'y')
barpP <- barpP + theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank()) +
    labs(y = "Carburetos")

print(barpP)

ct <- table(mtcars$hp, mtcars$cyl)
barplot(ct)

qplot(
    mtcars$cyl, geom = "bar"
)

qplot(iris$Sepal.Length,
      geom = "histogram",
      fill = I("blue"),
      colour = I("green"),
      xlab = "Lenght",
      ylab = "Total Iris",
      main = "Total Iris per Lenght",
      alpha = 0.7
)

counting <- table(mtcars$cyl)
barplot(counting)



#  ------------------------------------------------------------------------
# Lab

count <- table(mtcars$cyl)
barplot(count)

library(ggplot2)
qplot(mtcars$cyl, geom = "bar")



