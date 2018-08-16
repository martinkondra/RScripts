# http://www.r-tutor.com/elementary-statistics/hypothesis-testing
# rm(list=ls())

## DATOS ##

xbar <- p             # sample mean | (media/n)*N para total | p*N para casos favorables | p para proporcion
sigma <- 3               # population standard deviation 
n <- 100.0                  # sample size 
N = FALSE
p = 14/100
q = 1-p

h0 <- .05           # hypothesized value 
alpha <- .10

if(N){
  cvpf <- sqrt((N-n)/(N-1))
} else {
cvpf <- 1
}

desvio.media = sigma/sqrt(n) * cvpf
desvio.total = N * (sigma/sqrt(n)) * cvpf
desvio.proporcion = sqrt(p*q/n) * cvpf
desvio.favorables = N * desvio.proporcion
desvio.variancia = FALSE


## NORMAL ##

# upper tail h1>h0
z.alpha <- qnorm(1-alpha) 
xc = h0 + z.alpha * desvio.media

cat("xc:", xc)
cat("xbar:", xbar)
if(xbar>xc){
  print("Se rechaza h0")
} else {
  print("No se rechaza h0")
}


# lower tail h1<h0
z.alpha <- qnorm(alpha) 
xc = h0 + z.alpha * desvio.proporcion

if(xc>xbar){
  print("Se rechaza h0")
} else {
  print("No se rechaza h0")
}


# bidireccional
z.half.alpha = qnorm(1-alpha/2) 
z.alpha = c(-z.half.alpha, z.half.alpha) 
xc1 = h0 + z.alpha[1] * desvio.proporcion
xc2 = h0 + z.alpha[2] * desvio.proporcion

if((xc1>xbar) | (xbar>xc2)){
  print("Se rechaza h0")
} else {
  print("No se rechaza h0")
}


## STUDENT ##

# upper tail
t.alpha <- qt(1-alpha, df<-n-1) 
xc = h0 + t.alpha * desvio.media

if(xc<xbar){
  print("Se rechaza h0")
} else {
  print("No se rechaza h0")
}

# lower tail
t.alpha <- qt(alpha, df<-n-1)             # critical value 
xc = h0 + t.alpha * desvio.media

if(xbar<xc){
  print("Se rechaza h0")
} else {
  print("No se rechaza h0")
}

# bidireccional
t.half.alpha = qt(1-alpha/2, df=n-1) 
t.alpha = c(-z.half.alpha, z.half.alpha) 
xc1 = h0 + t.alpha[1] * desvio.media
xc2 = h0 + t.alpha[2] * desvio.media

if((xc1>xbar) | (xbar>xc2)){
  print("Se rechaza h0")
} else {
  print("No se rechaza h0")
}


## VARIANCIA / DESVIO ESTANDAR ##

# upper tail (solo se realiza esta prueba, una dispersión menor es beneficiosa) h0 sd = sd0

s2 = sigma**2
chi.alpha = qchisq(1-alpha, df=n-1)
xc = h0 * chi.alpha/(n-1)

if(xc<s2){
  print("Se rechaza h0")
} else {
  print("No se rechaza h0")
}


## PLOT ##
x <- seq(-4,4,length=100)*desvio.total + xbar
hx <- dnorm(x, xbar, desvio.total)
plot(x, hx, type="n", xlab="", ylab="",
     main="Normal Distribution", axes=TRUE)
lines(x, hx)
