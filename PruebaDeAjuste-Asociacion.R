# PRUEBA DE AJUSTE
# h0 f(x) = f(x)0

n = 50
fObservadas = c(2, 9, 16, 10, 8, 5)
fTotal = sum(fObservadas) 
  
# Depende de la distribución supuesta: Uniforme (dado), Normal, etc
# 1) uniforme
fEsperadas = rep(10, n) 
estadistico = sum((fObservadas-fEsperadas)**2/fEsperadas)
valorCritico = qchisq(1-alpha, df=n-1)

if (estadistico > valorCritico){
  print("Se rechaza h0")
} else {
  print("No se rechaza h0")
}

# 2) normal
alpha = 0.05
xbar = 7.5 #mean(fObservadas)
ds = 0.5 #sd(fObservadas)
val = c(6.5, 7.0, 7.5, 8.0, 8.5, 9.0)
val2 = c(0, 6.5, 7.0, 7.5, 8.0, 8.5)
prob_fEsperadas = pnorm(val, xbar, ds) - pnorm(val2, xbar, ds)
fEsperadas = fTotal * prob_fEsperadas

estadistico = sum((fObservadas-fEsperadas)**2/fEsperadas)
valorCritico = qchisq(1-alpha, df=5)
# los df acá son el numero de categorias - 1 !

if (estadistico > valorCritico){
  print("Se rechaza h0")
} else {
  print("No se rechaza h0")
}

# PRUEBA DE ASOCIACION
setwd("/home/kondra/R")
library(dplyr)

# h0 = las variables son independientes
alpha = 0.01
data <- read.csv("table.csv")
rownames(data) <- data$X
# remove the first column
data <- data %>% select(-X)

rowsTotal = rowSums(data)
colsTotal = colSums(data)
total = sum(rowsTotal)
df = (length(rowsTotal)-1)*(length(colsTotal)-1)

valores_esperados = c()
for(i in rowsTotal){
  for(j in colsTotal){
    e = (i*j/total)
    valores_esperados <- append(valores_esperados, e)
    }
}
valores_observados = c(t(data))
estadistico = sum((valores_observados-valores_esperados)**2/valores_esperados)

valorCritico = qchisq(1-alpha, df=df)
if (estadistico > valorCritico){
  print("Se rechaza h0")
} else {
  print("No se rechaza h0")
}
