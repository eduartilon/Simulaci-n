library(testit)

knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso)
  assert(n == length(valor))
  vt <- sum(valor)
  if (pt < cap) {
    return(vt)
  } else {
    filas <- cap + 1
    cols <- n + 1
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols)
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0
    }
    rownames(tabla) <- 0:cap
    colnames(tabla) <- c(0, valor)
    for (objeto in 1:n) {
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        tabla[acum, objeto + 1] <- tabla[acum, objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

#Modificacion para la regla 2
generador.pesos <- function(valores, min, max) {
  n <- length(valores)
  pesos <- double()
  for (i in 1:n) {
    media <- valores[i]
    desv <- runif(1, max=.1)
    ruido <- rnorm(1, sd=.1)
    pesos <- c(pesos, rnorm(1, (1/media), desv) + ruido)
  }
  pesos <- normalizar(pesos) * (max - min) + min
  return(pesos)
}

generador.valores <- function(cuantos, min, max) {
  return(sort(round(normalizar(rexp(cuantos)) * (max - min) + min)))
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(round(runif(tam * n)), nrow = tam, ncol = n)
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

datos = data.frame()

poblacion<-c(20, 30, 40)
j<- 1:3
for (init in poblacion){
  for (replicas in j){
    
    n <- 50
    pesos <- generador.pesos(valores, 15, 80)
    valores <- generador.valores(n, 10, 500)
    capacidad <- round(sum(pesos) * 0.65)
    optimo <- knapsack(capacidad, pesos, valores)
    
    p <- poblacion.inicial(n, init)
    tam <- dim(p)[1]
    assert(tam == init)
    pm <- 0.05
    rep <- 10
   
    mejores <- double()



timer = 5
start = Sys.time()



while(TRUE) {
  elapsed = as.numeric(difftime(Sys.time(), start, units = 'secs'))
  remaining = timer - round(elapsed) 
  Sys.sleep(.1)

  
  print(remaining)

  p$obj <- NULL
  p$fact <- NULL
  for (i in 1:tam) { # cada objeto puede mutarse con probabilidad pm
    if (runif(1) < pm) {
      p <- rbind(p, mutacion(p[i,], n))
    }
    
  }
  
  

  for (i in 1:rep) { # una cantidad fija de reproducciones
    padres <- sample(1:tam, 2, replace=FALSE)
    hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
    p <- rbind(p, hijos[1:n]) # primer hijo
    p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
  }
  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  for (i in 1:tam) {
    obj <- c(obj, objetivo(p[i,], valores))
    fact <- c(fact, factible(p[i,], pesos, capacidad))
  }
  p <- cbind(p, obj)
  p <- cbind(p, fact)
  mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
  p <- p[mantener,]
  tam <- dim(p)[1]
  assert(tam == init)
  factibles <- p[p$fact == TRUE,]
  mejor <- max(factibles$obj)
  mejores <- c(mejores, mejor)
  print(paste(mejor, (optimo - mejor) / optimo))
  
  
  faltaaloptimo<-(optimo - mejor) / optimo
  segundos<-round(elapsed)
  
  if (remaining <= 0) break
  
  
  resultado = c(replicas, segundos, mejor,faltaaloptimo,init,optimo)
  datos = rbind(datos, resultado)
  names(datos) = c("replicas", "segundo", "mejor", "%optimo", "poblacion","Optimo")
}
}
}

library(ggplot2)
datos$segundo = as.factor(datos$segundo)
datoss = split.data.frame(datos, f = datos$poblacion)

ggplot(datoss$`20`, aes(x= segundo, y= mejor)) + 
  geom_boxplot(fill = "#C77CFF", colour = "#1F3552")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  geom_hline(aes(yintercept=Optimo), colour="green")+
  labs(x = "Segundos", y = "Mayor valor")+
  coord_cartesian(ylim = c(3000, 7000))

ggplot(datoss$`30`, aes(x= segundo, y= mejor)) + 
  geom_boxplot(fill = "#C77CFF", colour = "#1F3552")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  geom_hline(aes(yintercept=Optimo), colour="green")+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  labs(x = "Segundos", y = "Mayor valor")+
  coord_cartesian(ylim = c(3000, 7000))

ggplot(datoss$`40`, aes(x= segundo, y= mejor)) + 
  geom_boxplot(fill = "#C77CFF", colour = "#1F3552")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  geom_hline(aes(yintercept=Optimo), colour="green")+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  labs(x = "Segundos", y = "Mayor valor")+
  coord_cartesian(ylim = c(3000, 7000))

cshapiro<-tapply(datos$mejor, datos$segundo, shapiro.test)
cat(paste0(capture.output(cshapiro),collapse = '\n'),file = 'cshapiror2pob.txt')

cwilcox<-pairwise.wilcox.test(datos$mejor, datos$segundo)
cat(paste0(capture.output(cwilcox),collapse = '\n'),file = 'cwilcoxr2pob.txt')

library(writexl)
write_xlsx(x=datos,path="datosr2pob.xlsx")
