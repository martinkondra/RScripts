## TAMAÑO DE LA MUESTRA ##
d <- .05 #amplitud/2
sigma <- 50
N <- 4000
p = 0.3
q = 1-p
intervalo = .10
z <- qnorm(1-intervalo/2)


# media
n0 = (z**2 * sigma**2) / d**2
n = (z**2 * sigma**2) / (d**2 + ((z**2 * sigma**2) / N))

# total
n0 = (N**2 * z**2 * sigma**2) / d**2
n = (N**2 * z**2 * sigma**2) / (d**2 + (N * z**2 * sigma**2))

# proporcion
n0 = (z**2 * p * q) / d**2
n = (z**2 * p * q) / (d**2 + (z**2 * p * q)/N)

# casos favorables
n0 = (N**2 * z**2 * p * q) / d**2
n = (N**2 * z**2 * p * q) / (d**2 + (N * z**2 * p * q))
