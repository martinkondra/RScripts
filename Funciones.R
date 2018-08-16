# FUNCIONES #####
#################*

PROBABILIDAD <- function(x1, x2, actividad = pro, prop = 0, media = 0,desvio = 1, curva = "nor", df = 1){
  POS <- ifelse(lado == "izq", TRUE, FALSE)
  POS <- ifelse(lado == "amb", "amb", POS)
  
  u <- media
  s <-  desvio  
  z <- (x1-u)/s
  if(curva == "nor"){
    if (actividad == "pro"){
      if (POS == "amb"){
        z1 <- z
        z2 <- (x2-u)/s
        
        p1 <- pnorm(x1, mean = u, sd = s, lower.tail = T)*100
        p2 <- pnorm(x2, mean = u, sd = s, lower.tail = T)*100
        
        if (pnorm(z1, mean = 0, sd = 1, lower.tail = T)*100 == p1 & pnorm(z2, mean = 0, sd = 1, lower.tail = T)*100 == p2){
          p <- p2-p1
          
          print(paste("porcentaje = ", p))
          print(paste("z1 = ", z1))
          print(paste("z2 = ", z2))  
        }
      }else{
        p <- pnorm(x1, mean = u, sd = s, lower.tail = POS)*100
        if(pnorm(z, mean = 0, sd = 1, lower.tail = POS)*100 == p){
          print(paste("porcentaje = ", p))
          print(paste("z = ", z))
        }
      }
    }else{ 
      if  (actividad == "val"){
        zq <- qnorm(prop, mean = 0, sd = 1, lower.tail = POS)
        xq <- qnorm(prop, mean = u, sd = s, lower.tail = POS)
        print(paste("VALORES PARA UNA PROBABILIDAD DE ", prop*100, "%"))
        print(paste("Valor de x = ",  xq))
        print(paste("Valor de z = ",  zq))
      }
    }
  }else{
    if (curva == "tst"){
      print("T de STUDENT")
      if (actividad == "pro"){
        print(paste("Probabilidad para un valor ", x1))
        print(pt (x1, df, lower.tail = POS))
      }else{
        if (actividad == "val"){
          print(paste("Valor t para una probabilidad ", p*100, "%. Grados de libertad = ", df))
          qt (p, df, lower.tail = POS)
        }
      }
      
    }
  }
}    

FC <- function(N, n){
  if (N == Inf){
      print ("N es infinito, FC = 1")
      FC <- 1
  }else{
      
      return((N-n)/ (N-1))
      print (paste("Factor de corrección= ", FC) )
  }
  
}

Sd_m <- function(sd, N, n){
    FC (N, n)
    S <- sd
    if (N == Inf){
        Sx <- S / sqrt(n)
    }else{
        Sx <- S / sqrt(n) * sqrt((N-n)/ (N-1))  
    }
print(paste("Desvio de la media muestral: ", Sx) )  
}

Sd_p <- function(p, N, n){
    FC (N, n)
    q <- 1-p
    if (N == Inf){
      Sx <- sqrt(p*q/n)
    }else{
      Sx <- sqrt(p*q/n) * sqrt((N-n)/ (N-1))  
    }
    print(paste("Desvio de la proporcion muestral: ", Sx) )  
}

PROB_Med_Muest <- function(x1, x2, actividad = pro, prop = 0, media = 0, desvio = 1, muestra, poblacion = Inf, sd_N = T){
  S <- desvio
  N <- poblacion
  n <- muestra
  FC <- (N-n) / (N-1)
  curva <- ifelse((n >= 100 | sd_N == T), "nor", "tst")
  df <- n-1
  
  if (N == Inf){
    Sx <- S / sqrt(n)
  }else{
    Sx <- S / sqrt(n) * sqrt(FC)  
  }
  PROBABILIDAD (x1, x2, actividad = actividad, prop = prop, media = media, desvio = Sx, curva = curva, df = df)  
  print(paste("Desvio del estimador muestral: ", Sx) )
}

INTERVALO <- function(alfa = 0.95, est = 0, sd = 1, n, N, ESTIM = "media"){
  if (n < 100) { print("n muy pequeño! cuidado con distribución"); print("")}
  prob <- (1 - alfa)/2
  z <- qnorm (prob, lower.tail = F)
  S <- sd
  
  if (N == Inf){
     FC <- 1
  }else{
     FC <- (N-n)/(N-1)  
  }
  
  if (ESTIM %in% c("media", "tot" )) {
          Sx <- S / sqrt(n) * sqrt(FC)  
          if (ESTIM == "tot"){
              Sx <- Sx * N
          }
  }
  
  if (ESTIM %in% c("prop", "fav") ){
        Sx <- sqrt (S * (1-S) / n ) * sqrt(FC)
        est <- S
        if (ESTIM == "fav"){
            Sx <- N * Sx
            est <- est * N
        }
  }
    
    L <- z * Sx
    Li <- est - L
    Ls <- est + L
    print(paste("Intervalo con una confianza para ", ESTIM, "(",est,")" ))
    print(paste("P ( ", Li, " < E < ", Ls," ) = ",  alfa * 100,  " %"))
    print(paste("Varianza muestral: ", Sx))
    print(paste("z: ", z))
    print(paste("Amplitud (A) = ", L*2))
    print(paste("Diferencia con el estimador (d)  = ", L))
}  
  
MUESTRA  <- function(N, alfa = 0.95, d, sd, ESTIM = "media"){
  z <- qnorm((1-alfa)/2, lower.tail = F)
  
  if (ESTIM == "media"){
      x0 <- z^2 * sd^2    
  }else{
      if (ESTIM == "tot"){
         x0 <- z^2 * sd^2 * N^2
      }
  }
  if (ESTIM == "prop"){
      x0 <- sd*(1-sd) * z^2
  }else{
      if (ESTIM == "fav"){
          x0 <- sd*(1-sd) * z^2 * N^2
      }
  }
  
  
  if (N == Inf){
      FCM <- 0
  }else{
     FCM <- x0 / N
  }
  
  n <- x0/(d^2+FCM)
  
  
  print(paste("CALCULO PARA ", ESTIM))
  print(paste("Valor de z = ", z))
  print(paste("Tamaño de la muestra = ", n))
  print(paste("Factor de correccion = ", FCM))
  print(paste("Numerador z^2 * sd^2 [* N^2] = ", x0))
  
  
}
  

# DATOS #########
#################*
      
      media     <- 0
      desvio    <- 50
      poblacion <- 4000
      muestra    <- 0
      x1        <- 0
      x2        <- 0  
      p         <- 0
      prop      <- 0
      df        <- 0
      alfa      <- 0.95
      d         <- 72000
      
      actividad <- "pro"
      lado      <- "der"
      curva     <- "nor"
      # varianza  <- 0
      # desvio <- sqrt( varianza)

# valores de actividad: pro | val ####
#          pro: saca la probabilidad de que algo ocurra para ciertos valores (x1 + x2)
#          val: saca los valores para los que se da cierta probabilidad

# valores de lado: izq | der | amb ####
#          izq: X <=  x1
#          der: X >=  x1  
#          amb: x1 <= X <= x2

# valores de curva: nor | tst | chi ####
#          nor: normal
#          tst: t student
#          chi: chi cuadrado

      
# valores de estimador: media | tot | prop | fav
      

# CALCULOS #########
####################*

### Probabilidad de x1 y x2 ####################################################
#####
media     <- 20000
desvio    <- 3000
poblacion <- Inf
muestra    <- 100
x1        <- 25000
x2        <- 0  
prop      <- 0
df        <- 0
actividad <- "pro"      
lado      <- "izq"
curva     <- "nor"
#####
                  
      PROBABILIDAD (x1, x2, actividad = actividad,  prop = prop, media = media, 
                    desvio = desvio, curva = curva, df = df)
    
      rm(x1, x2, actividad, prop, media, desvio, curva, df, lado)


### PROBABILIDAD DE MEDIA MUESTRAL ##############################################
#####
      media     <- 0
      desvio    <- 50
      poblacion <- 4000
      muestra   <- 0
      x1        <- 0
      x2        <- 0  
      p         <- 0
      prop      <- 0
      df        <- 0

      actividad <- "pro"
      lado      <- "der"
      curva     <- "nor"
#####
      PROB_Med_Muest (x1, x2, actividad = actividad, prop = prop, media = media, 
                      desvio = desvio, muestra = muestra, poblacion = poblacion,
                      sd_N = T)
      
      rm(x1, x2, actividad, prop, media, desvio, muestra, poblacion)
### FACTOR DE CORRECCION ######################################################

poblacion <- 200000
muestra    <- 500

      FC (N = poblacion, n = muestra)
      rm(poblacion, muestra)
### DESVIO DEL ESTIMADOR DE LA MEDIA/TOTAL ###################################
    
desvio      <- 0
poblacion   <- 0
muestra     <- 0

      Sd_m (sd = desvio, N =  poblacion, n = muestra)
      rm(poblacion, desvio, muestra)

### DESVIO DEL ESTIMADOR DE PROPORCION/CASOS FAVORABLES ######################    

poblacion <- 4000
muestra    <- 0
p         <- 0

      Sd_p (p = p, N = poblacion, n = muestra)
      rm(poblacion,muestra, p)

### INTERVALO DE CONFIANZA ###################################################    

#####            
media     <- 10000
desvio    <- 0.45
# desvio  <- p 
poblacion <- Inf
muestra   <- 100
alfa      <- 0.95
#####

    # valores de estimador: media | tot | prop | fav
    INTERVALO (alfa = alfa, est = media, sd = desvio, n = muestra, N = poblacion, ESTIM = "prop")

    rm(poblacion, alfa, desvio, muestra, media)



### TAMAÑO DE LA MUESTRA ###################################*      

desvio    <- 0.02
# para proporcion colocar p
poblacion <- 6000
alfa      <- 0.99
d         <- 0.1

    # valores de estimador: media | tot | prop | fav
    MUESTRA (N = poblacion, alfa = alfa, d = d, sd = desvio, ESTIM = "prop")

    rm(poblacion, alfa, d, desvio)
