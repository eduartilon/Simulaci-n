library(ggplot2)
library(tidyverse)

l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30
vac<- seq(0.1,0.9,0.1)
g<-(1:30)

datos<-data.frame()

for (pvac in vac) {
  for (rep in g) {
    
    
    agentes <- data.frame(x = double(), y = double(),
                          dx = double(), dy = double(),
                          estado  = character())
    for (i in 1:n){
      if (runif(1)<pvac){
        e<-"R"} 
      else if (runif(1) < pi) {
        e <- "I"}
      else{ e <- "S"
      }
      
      
      agentes <- rbind(agentes, data.frame(x = runif(1, 0, l),
                                           y = runif(1, 0, l),
                                           dx = runif(1, -v, v),
                                           dy = runif(1, -v, v),
                                           estado = e))
      
      levels(agentes$estado) <- c("S", "I", "R")
    }
    
    epidemia <- integer()
    r <- 0.1
    tmax <- 100
    digitos <- floor(log(tmax, 10)) + 1
    maxinf <- 0
    
    for (tiempo in 1:tmax) {
      
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      
      if (infectados == 0) {
        iteracion=tiempo
        break
      }
      if (max(epidemia)>maxinf){
        maxinf=max(epidemia)
        iteracion=tiempo
      }
      
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r) { # umbral
                  p <- (r - d) / r
                  if (runif(1) < p) {
                    contagios[j] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
      for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
          }
        }
        a$x <- a$x + a$dx
        a$y <- a$y + a$dy
        if (a$x > l) {
          a$x <- a$x - l
        }
        if (a$y > l) {
          a$y <- a$y - l
        }
        if (a$x < 0) {
          a$x <- a$x + l
        }
        if (a$y < 0) {
          a$y <- a$y + l
        }
        agentes[i, ] <- a
      }
    }
    
    porcentaje<-(maxinf/n)*100
    datos <- rbind(datos,c(pvac,maxinf,porcentaje,iteracion))
    print(pvac)
    
  
  }
}

names(datos)<-c("probabilidad", "maximo", "porcentaje","tiempo")
print(datos)

datos$probabilidad = as.factor(datos$probabilidad)

ggplot(datos, aes(x=probabilidad , y= porcentaje , fill= rep)) + # fill=name allow to automatically dedicate a color for each group
  geom_boxplot(fill = "#F8766D", colour = "#1F3552")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  coord_cartesian(ylim = c(0,70))+
  labs(x="Probabilidad de vacunación al inicio", y= "Porcentaje de infectados")

ggplot(datos, aes(x=probabilidad , y= tiempo , fill= rep)) + # fill=name allow to automatically dedicate a color for each group
  geom_boxplot(fill = "#F8766D", colour = "#1F3552")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  coord_cartesian(ylim = c(0,100))+
  labs(x="Probabilidad de vacunación al inicio", y= "Iteración del máximo")

datos%>%
  group_by(probabilidad) %>%
  summarise(
   
    promedio = mean(porcentaje, na.rm = TRUE),
    desviacion_std = sd(porcentaje, na.rm = TRUE),
    varianza = sd(porcentaje, na.rm = TRUE)^2,
    mediana = median(porcentaje, na.rm = TRUE),
    rango_intercuartil = IQR(porcentaje, na.rm = TRUE)
  )


shapiro.test(datos$porcentaje)
kruskal.test(porcentaje~probabilidad, data=datos)
pairwise.wilcox.test(datos$porcentaje, datos$probabilidad)

datos%>%
  group_by(probabilidad) %>%
  summarise(
   
    promedio = mean(tiempo, na.rm = TRUE),
    desviacion_std = sd(tiempo, na.rm = TRUE),
    varianza = sd(tiempo, na.rm = TRUE)^2,
    mediana = median(tiempo, na.rm = TRUE),
    rango_intercuartil = IQR(tiempo, na.rm = TRUE)
  )

shapiro.test(datos$tiempo)
kruskal.test(tiempo~probabilidad, data=datos)
pairwise.wilcox.test(datos$tiempo, datos$probabilidad)
