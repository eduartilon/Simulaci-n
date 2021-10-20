library(testit) # para pruebas, recuerda instalar antes de usar
taman <- c(100, 200, 400)
n <- 100000
j<- 1:30

datos = data.frame()


for (k in taman){
 for (replicas in j){
    originales <- rnorm(k)
    cumulos <- originales - min(originales) + 1
    cumulos <- round(n * cumulos / sum(cumulos))
    assert(min(cumulos) > 0)
    diferencia <- n - sum(cumulos)
    if (diferencia > 0) {
      for (i in 1:diferencia) {
        p <- sample(1:k, 1)
        cumulos[p] <- cumulos[p] + 1
      }
    } else if (diferencia < 0) {
      for (i in 1:-diferencia) {
        p <- sample(1:k, 1)
        if (cumulos[p] > 1) {
          cumulos[p] <- cumulos[p] - 1
        }
      }
    }
    
    png("p8_init.png")
    plot(hist(cumulos), main="Estado inicial",
         xlab="Tamaño de cúmulos", ylab="Frecuencia absoluta")
    graphics.off()
    
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    assert(sum(cumulos) == n)
    c <- median(cumulos) # tamaño critico de cumulos
    d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
    
    uno <- as.data.frame(table(cumulos))
    names(uno) <- c("tam", "num")
    uno$tam <- as.numeric(levels(uno$tam))[uno$tam]
    assert(sum(uno$num * uno$tam) == n)
    
    filtro1 = uno[uno$tam >= c,]
    filtro1$cont = filtro1$tam * filtro1$num
    f1 = sum(filtro1$cont) 
    porcent1 = 100 * f1/n 
    paso1 = 0
    resultado1 = c(k, replicas, paso1, porcent1, c)
    datos = rbind(datos, resultado1)
    names(datos) = c("k", "Replicas", "Iteracion", "filtrado", "c")
    assert(sum(abs(cumulos)) == n)
    
    rotura <- function(x) {
      return (1 / (1 + exp((c - x) / d)))
    }
    union <- function(x) {
      return (exp(-x / c))
    }
    romperse <- function(tam, cuantos) {
      romper <- round(rotura(tam) * cuantos) # independientes
      resultado <- rep(tam, cuantos - romper) # los demas
      if (romper > 0) {
        for (cumulo in 1:romper) { # agregar las rotas
          t <- 1
          if (tam > 2) { # sample no jala con un solo valor
            t <- sample(1:(tam-1), 1)
          }
          resultado <- c(resultado, t, tam - t)
        }
      }
      assert(sum(resultado) == tam * cuantos) # no hubo perdidas
      return(resultado)
    }
    unirse <- function(tam, cuantos) {
      unir <- round(union(tam) * cuantos) # independientes
      if (unir > 0) {
        division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
        assert(sum(abs(division)) == tam * cuantos)
        return(division)
      } else {
        return(rep(tam, cuantos))
      }
    }
    freq <- as.data.frame(table(cumulos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    duracion <- 50
    digitos <- floor(log(duracion, 10)) + 1
    for (paso in 1:duracion) {
      assert(sum(cumulos) == n)
      cumulos <- integer()
      for (i in 1:dim(freq)[1]) { # fase de rotura
        urna <- freq[i,]
        if (urna$tam > 1) { # no tiene caso romper si no se puede
          cumulos <- c(cumulos, romperse(urna$tam, urna$num))
        } else {
          cumulos <- c(cumulos, rep(1, urna$num))
        }
      }
      assert(sum(cumulos) == n)
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      freq <- as.data.frame(table(cumulos)) # actualizar urnas
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      assert(sum(freq$num * freq$tam) == n)
      cumulos <- integer()
      for (i in 1:dim(freq)[1]) { # fase de union
        urna <- freq[i,]
        cumulos <- c(cumulos, unirse(urna$tam, urna$num))
      }
      assert(sum(abs(cumulos)) == n)
      assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
      juntarse <- -cumulos[cumulos < 0]
      cumulos <- cumulos[cumulos > 0]
      assert(sum(cumulos) + sum(juntarse) == n)
      nt <- length(juntarse)
      if (nt > 0) {
        if (nt > 1) {
          juntarse <- sample(juntarse)
          for (i in 1:floor(nt / 2) ) {
            cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
          }
        }
        if (nt %% 2 == 1) {
          cumulos <- c(cumulos, juntarse[nt])
        }
      }
      assert(sum(cumulos) == n)
      freq <- as.data.frame(table(cumulos))
      names(freq) <- c("tam", "num")
      freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
      assert(sum(freq$num * freq$tam) == n)
      tl <- paste(paso, "", sep="")
      while (nchar(tl) < digitos) {
        tl <- paste("0", tl, sep="")
      }
      if (replicas == 1){
        png(paste("p8_ct", tl, "k=", k, "rep=", replicas, ".png", sep=""), width=300, height=300)
        tope <- 50 * ceiling(max(cumulos) / 50)
        hist(cumulos, breaks=seq(0, tope, 50), 
             main=paste("Paso", paso, "con ambos fenómenos"), freq=FALSE,
             ylim=c(0, 0.02), xlab="Tamaño", ylab="Frecuencia relativa")
        graphics.off() 
      }
      freq
      filtros = freq[freq$tam >= c,]
      filtros$cont = filtros$tam * filtros$num
      f = sum(filtros$cont)
      porcent = 100 * f/n 
      resultado = c(k, replicas, paso, porcent, c)
      datos = rbind(datos, resultado)
      
      assert(sum(abs(cumulos)) == n)
    }  
  }
}


png("p8_norm.png")
par(mfrow = c(2, 2)) # juntamos graficas
plot(density(originales)) # lo generado que era normal
print(shapiro.test(originales))
qqnorm(originales)
qqline(originales, col = 2)
plot(density(cumulos)) # lo nuestro que hemos modificado
print(shapiro.test(cumulos))
qqnorm(cumulos)
qqline(cumulos, col = 2)
graphics.off()

library(ggplot2)
datos$Iteracion = as.factor(datos$Iteracion)
datoss = split.data.frame(datos, f = datos$k)

ggplot(datoss$`100`, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#F8766D", colour = "#1F3552")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  labs(x = "Iteración", y = "% Filtrado")

ggplot(datoss$`200`, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#3FCF30",colour = "#1F3552")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  labs(x = "Iteración", y = "% Filtrado")

ggplot(datoss$`400`, aes(x= Iteracion, y= filtrado)) + 
  geom_boxplot(fill = "#FAFA2D",colour = "#1F3552")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  labs(x = "Iteración", y = "% Filtrado")

library(tidyverse)
library(writexl)
options(max.print=999999)

resul100<-datoss$`100`%>%
  group_by(Iteracion) %>%
  summarise(
    
    promedio = mean(filtrado, na.rm = TRUE),
    desviacion_std = sd(filtrado, na.rm = TRUE),
    varianza = sd(filtrado, na.rm = TRUE)^2,
    mediana = median(filtrado, na.rm = TRUE),
    rango_intercuartil = IQR(filtrado, na.rm = TRUE)
  )

resul200<-datoss$`200`%>%
  group_by(Iteracion) %>%
  summarise(
    
    promedio = mean(filtrado, na.rm = TRUE),
    desviacion_std = sd(filtrado, na.rm = TRUE),
    varianza = sd(filtrado, na.rm = TRUE)^2,
    mediana = median(filtrado, na.rm = TRUE),
    rango_intercuartil = IQR(filtrado, na.rm = TRUE)
  )


resul400<-datoss$`400`%>%
  group_by(Iteracion) %>%
  summarise(
    
    promedio = mean(filtrado, na.rm = TRUE),
    desviacion_std = sd(filtrado, na.rm = TRUE),
    varianza = sd(filtrado, na.rm = TRUE)^2,
    mediana = median(filtrado, na.rm = TRUE),
    rango_intercuartil = IQR(filtrado, na.rm = TRUE)
  )

write_xlsx(x=resul100,path="resul100.xlsx")
write_xlsx(x=resul200,path="resul200.xlsx")
write_xlsx(x=resul400,path="resul400.xlsx")

shapiro100<-tapply(datoss$`100`$filtrado, datoss$`100`$Iteracion, shapiro.test)
shapiro200<-tapply(datoss$`200`$filtrado, datoss$`200`$Iteracion, shapiro.test)
shapiro400<-tapply(datoss$`400`$filtrado, datoss$`400`$Iteracion, shapiro.test)

cat(paste0(capture.output(shapiro100),collapse = '\n'),file = 'shaphiro100.txt')
cat(paste0(capture.output(shapiro200),collapse = '\n'),file = 'shaphiro200.txt')
cat(paste0(capture.output(shapiro400),collapse = '\n'),file = 'shaphiro400.txt')

one.way1<-aov(filtrado~Iteracion, data=datoss$`100`)
one.way11<-summary(one.way1)
write_xlsx(x=one.way11[[1]],path="anova100.xlsx")

one.way2<-aov(filtrado~Iteracion, data=datoss$`200`)
one.way22<-summary(one.way2)
write_xlsx(x=one.way22[[1]],path="anova200.xlsx")

one.way4<-aov(filtrado~Iteracion, data=datoss$`400`)
one.way44<-summary(one.way4)
write_xlsx(x=one.way44[[1]],path="anova400.xlsx")

one.wayt<-aov(filtrado~Iteracion, data=datos)
one.waytt<-summary(one.wayt)
write_xlsx(x=one.waytt[[1]],path="anovatodos.xlsx")

pw100<-pairwise.wilcox.test(datoss$`100`$filtrado, datoss$`100`$Iteracion)
cat(paste0(capture.output(pw100),collapse = '\n'),file = 'pw100.txt')

pw200<-pairwise.wilcox.test(datoss$`200`$filtrado, datoss$`200`$Iteracion)
cat(paste0(capture.output(pw200),collapse = '\n'),file = 'pw200.txt')

pw400<-pairwise.wilcox.test(datoss$`400`$filtrado, datoss$`400`$Iteracion)
cat(paste0(capture.output(pw400),collapse = '\n'),file = 'pw400.txt')
