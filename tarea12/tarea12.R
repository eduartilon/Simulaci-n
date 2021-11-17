binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

neg<-c(0.3,0.6,0.99)
gri<-c(0.3,0.6,0.9)
bla<-c(0.1,0.01,0.001)
datos=data.frame()

j<-15

for(ne in neg){
for(g in gri){
for(b in bla){
  
for(rep in 1:j){
    


modelos <- read.csv("digits.txt", sep=" ", header=FALSE, stringsAsFactors=F)
modelos[modelos=='n'] <- ne
modelos[modelos=='g'] <- g
modelos[modelos=='b'] <- b

r <- 5
c <- 3
dim <- r * c

tasa <- 0.15
tranqui <- 0.99

tope <- 9
digitos <- 0:tope
k <- length(digitos)
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}

for (t in 1:300) { # prueba
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- binario(d, n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
  }
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
  contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
}
print(contadores)

precision <- diag(contadores) / colSums(contadores[,1:10])
recall <- diag(contadores) / rowSums(contadores)
fscore <- (2 * precision * recall) / (precision + recall) 
datos=rbind(datos,c(rep,ne,g,b,fscore))

}
}
}
}

names(datos) <- c("Replica", "Negro","Gris","Blanco","0", "1","2","3","4","5","6","7","8","9")

library(ggplot2)
library(tidyr)

etiquetas=rep(c("n1g1b1","n1g1b2","n1g1b3","n1g2b1","n1g2b2","n1g2b3","n1g3b1","n1g3b2","n1g3b3",
              "n2g1b1","n2g1b2","n2g1b3","n2g2b1","n2g2b2","n2g2b3","n2g3b1","n2g3b2","n2g3b3",
              "n3g1b1","n3g1b2","n3g1b3","n3g2b1","n3g2b2","n3g2b3","n3g3b1","n3g3b2","n3g3b3"),
            times=c(j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j,j))

datos$combo <- etiquetas 

library(reshape2)
dat.m <- melt(datos,id.vars='combo', measure.vars=c('0','1','2','3','4','5','6','7','8','9'))
library(ggplot2)

ggplot(dat.m, aes(x= combo, y= value, fill= combo)) + # fill=name allow to automatically dedicate a color for each group
  geom_boxplot()+ labs(x="Combo", y= "Puntaje F") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_violin(fill="green", color="yellow",alpha = 5/10)

library(tidyverse)
library(writexl)

valorf<-dat.m%>%
  group_by(combo) %>%
  summarise(
    
    promedio = mean(value, na.rm = TRUE),
    desviacion_std = sd(value, na.rm = TRUE),
    varianza = sd(value, na.rm = TRUE)^2,
    mediana = median(value, na.rm = TRUE),
    rango_intercuartil = IQR(value, na.rm = TRUE)
  )

write_xlsx(x=valorf,path="valorf.xlsx")

fshapiro<-tapply(dat.m$value, dat.m$combo, shapiro.test)
cat(paste0(capture.output(fshapiro),collapse = '\n'),file = 'fshapiro.txt')

kruskal.test(value~combo, data=dat.m)

fwilcox<-pairwise.wilcox.test(dat.m$value, dat.m$combo)
cat(paste0(capture.output(fwilcox),collapse = '\n'),file = 'fwilcox.txt')

write_xlsx(x=datos,path="datos12.xlsx")
write_xlsx(x=dat.m,path="datos12reorden.xlsx")
