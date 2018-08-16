# ASOCIACION ENTRE VARIABLES

# 1) CUALITATIVAS
# a. crear tabla de contingencia con totales
# b. tabla de contingencia con porcentajes
# c. tabla con porcentajes de cada categoría

# Coeficiente diferencia de proporciones
# D = p11 - p12 = p12 - p22

data <- read.csv("table.csv")

# 2) CUALITATIVAS Y CUANTITATIVA
# obtener valores resumen: n, xmin, xmax, xbar, Me, sigma, CV para cada categoria

# VI dispersion dentro de cada grupo (suma de los cuadrados de los desvios de los valores respecto a la media de cada grupo)
# VE dispersion entre los grupos (suma de los cuadrados de los desvios de los valores respecto a la media general)
# VT = VI + VE

# Coeficiente razón de correlación RC = VE / VT


# 3) CUANTITATIVAS