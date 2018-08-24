## DATOS ##

xbar <- p             # sample mean | (media/n)*N para total | p*N para casos favorables | p para proporcion
sigma <- 6             # population standard deviation 
n <- 600             # sample size 
N = FALSE
p = 80/600
q = 1-p

intervalo = .01

if(N){
  cvpf <- sqrt((N-n)/(N-1))
} else {
  cvpf <- 1
}

desvio.media = sigma/sqrt(n) * cvpf
desvio.total = N * (sigma/sqrt(n)) * cvpf
desvio.proporcion = sqrt(p*q/n) * cvpf
desvio.favorables = N * desvio.proporcion


## ESTIMACIÓN POR INTERVALOS DE CONFIANZA ##
# est - k * sigma.est <   < est + k * sigma.est

#Normal
z = qnorm(1-intervalo/2)
int1 = xbar - z * desvio.proporcion
int2 = xbar + z * desvio.proporcion

#Student
t = qt(1-alpha/2, df=n-1)  #student
int1 = xbar - t * desvio.media
int2 = xbar + t * desvio.media


# estimador de la variancia
n = 400
S2 = 144
intervalo = .05
int1 = ((n-1)*S2) / qchisq(1-intervalo/2, df=n-1)
int2 = ((n-1)*S2) / qchisq(intervalo/2, df=n-1)


