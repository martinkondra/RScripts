# ASOCIACION ENTRE VARIABLES

# 1) CUALITATIVAS
# a. crear tabla de contingencia con totales

# b. tabla de contingencia con porcentajes
#    a cada celda la dividimos por n(cantidad de casos) y multiplicamos por 100

# c. tabla con porcentajes de cada categoría X
#    a cada porcentaje lo dividimos por el porcentaje total de la fila y multiplicamos por 100

# Coeficiente diferencia de proporciones
# D = p11 - p21 = p12 - p22 -> incidencia porcentual de la variable X sobre la variable Y

# d. tabla con porcentajes de cada categoría Y
#    a cada porcentaje lo dividimos por el porcentaje total de la columna y multiplicamos por 100

# D = p11 - p12 = p21 - p22 -> incidencia porcentual de la variable Y sobre la variable X

# Siempre 0 < D < 100.
# 0 = independencia, 100 = total asociación

# 24 86 | 110
#  5 21 | 26
# ______
# 29 107  136

# 21 79 | 100
# 19 81 | 100
# ______
# 29 107  136

# 21-19 = 79-81
# La fila o columna de la que queremos evaluar si tiene efecto sobre la otra variable es la que debe sumar 100!
# Luego, la resta se realiza sobre la otra (si la fila suma 100, sumamos las columnas y viceversa)

n = 136
celdas = c(24, 86, 110, 5, 21, 26, 29, 107, 136) #incluyendo marginales
porcentajes = celdas/n*100

# 2) CUALITATIVAS Y CUANTITATIVA
# obtener valores resumen: n, xmin, xmax, xbar, Me, sigma, CV para cada categoria

# VI dispersion dentro de cada grupo (suma de los cuadrados de los desvios de los valores respecto a la media de cada grupo)
# VE dispersion entre los grupos (suma de los cuadrados de los desvios de los valores respecto a la media general)
# VT = VI + VE

# Coeficiente razón de correlación RC = VE / VT


