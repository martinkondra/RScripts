x <- c(9.8, 6.3, 10.5, 11.2, 3.4, 7.8, 10.8, 8.6, 12.2, 12.3)
y <- c(2.5, 2.4, 3.4, 3.3, 0.5, 2.6, 2.9, 1.9, 3.6, 3.5)
n <- length(x)

xbar <- mean(x)
ybar <- mean(y)

x2 <- sum(x**2)
y2 <- sum(y**2)

b <- (sum(x*y)-n*xbar*ybar)/(x2-n*xbar**2)
a <- ybar - b*xbar

# otra recta de regresión: cambiar el nombre de x por el de y
b1 <- (sum(x*y)-n*xbar*ybar)/(y2-n*ybar**2)
a1 <- xbar - b1*ybar

# calcular y para un x dado
xi = 15
yi = a + b * xi

yi = 5
xi = (yi-a)/b

# pearson correlation
pearson = cov(x,y)/(sd(x)*sd(y))
pearson = sqrt(b*b1) #multiplicamos el b para cada recta
pearson = cor(x,y)

# coeficiente de determinacion: porcentaje de los cambios en una variable explicados por cambios en la otra
cd = (pearson**2) * 100

# plot
plot(x,y)
abline(c(a,b), lwd=2)

# usando lm
mod1 <- lm(formula=y ~ x)
plot(x, y, xlim=c(0, max(x)+5), ylim=c(0, max(y)+10))
abline(mod1, lwd=2)
cor(x,y, method='pearson')


