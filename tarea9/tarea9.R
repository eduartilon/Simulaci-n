library(ggplot2)
    
n <- 50

datos<- data.frame()

repeticiones<- 1:15
  
for (repe in repeticiones) {
p <- data.frame(repe, x = rnorm(n), y=rnorm(n), c=rnorm(n), m=rnorm(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <-  2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
mmax=max(p$m)
mmin=min(p$m)
p$m=(p$m - mmin) / (mmax - mmin) + 0.01

paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
eps <- 0.001
fuerza <- function(i) {
 xi <- p[i,]$x
 yi <- p[i,]$y
 ci <- p[i,]$c
 mi <- p[i,]$m 
 fx <- 0
 fy <- 0
 for (j in 1:n) {
  cj <- p[j,]$c
  dir <- (-1)^(1 + 1 * (ci * cj < 0))
  dx <- xi - p[j,]$x
  dy <- yi - p[j,]$y
  factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
  fx <- fx - dx * factor
  fy <- fy - dy * factor
  }
  return(c(fx, fy)/(mi)) #Interacción de la masa con la fuerza de las partículas
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

tmax <- 50
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}



#Estado inicial
png(paste("t9_t", 0, tl, ".png", sep=""),width = 800,height = 700)
print( ggplot(data=p, aes(x=x ,y=y, size=m, col=colores[p$g+6]) )
       +geom_point(show.legend =  TRUE)+xlim(c(0,1))+ylim(c(0,1))+  
         ggtitle(paste("Estado inicial"))
       + scale_shape_discrete(name  ="Carga")+ 
         scale_colour_discrete(name  ="Carga", labels=seq(-5,5)))
graphics.off()


p$vel=numeric(n)
for (iter in 1:tmax) {
 f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
 delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
 p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
 p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
 v <- foreach(i=1:n,.combine=c)%dopar% sqrt((delta * f[c(TRUE, FALSE)][i])^2 + (delta * f[c(FALSE, TRUE)][i])^2)
 p$vel=p$vel+v
  
 tl <- paste(iter, "", sep="")
 while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
 png(paste("t9_t", 0, tl, ".png", sep=""),width = 800,height = 700)
 print( ggplot(data=p, aes(x=x ,y=y, size=m, col=colores[p$g+6]) )
        +geom_point(show.legend =  TRUE)+xlim(c(0,1))+ylim(c(0,1))+  
        scale_shape_discrete(name  ="Carga")+ 
        ggtitle(paste(tl))+
        scale_colour_discrete(name  ="Carga", labels=seq(-5,5)))
 graphics.off()
}
stopImplicitCluster()
datos<-rbind(datos,p)
}
#carga contra velocidad
ggplot(data = p, aes(x= c, y= vel, color= m))+
  geom_point(size=2)+
  geom_smooth(method = "loess", se=FALSE, formula =y ~ x)+
  stat_summary(fun = mean, geom = "point",
               size = 0.7, fill = "black")+
  guides(scale = "none", color=guide_legend(title = "Masa"))+
  scale_x_continuous(name = "Carga")+
  scale_y_continuous(name = "Velocidad")+
  theme(plot.title = element_text(hjust = 0.5))

#masa contra velocidad
ggplot(data = p, aes(x= m, y= vel, color= c))+
  geom_point(size=2)+
  geom_smooth(method = "loess", se=FALSE, formula =y ~ x)+
  stat_summary(fun = mean, geom = "point",
               size = 0.7, fill = "black")+
  guides(scale = "none", color=guide_legend(title = "carga"))+
  scale_x_continuous(name = "Masa")+
  scale_y_continuous(name = "Velocidad")+
  theme(plot.title = element_text(hjust = 0.5))

datos$g = as.factor(datos$g)

ggplot(datos, aes(x=g , y=vel , fill= repe)) + # fill=name allow to automatically dedicate a color for each group
  geom_boxplot(fill = "#F8766D", colour = "#1F3552")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  coord_cartesian(ylim = c(0,1.4))+
  labs(x="Carga en g", y= "Velocidad")

library(tidyverse)
library(writexl)

resulcarg<-datos%>%
  group_by(g) %>%
  summarise(
    
    promedio = mean(vel, na.rm = TRUE),
    desviacion_std = sd(vel, na.rm = TRUE),
    varianza = sd(vel, na.rm = TRUE)^2,
    mediana = median(vel, na.rm = TRUE),
    rango_intercuartil = IQR(vel, na.rm = TRUE)
  )
write_xlsx(x=resulcarg,path="resulcarg.xlsx")

cshapiro<-tapply(datos$vel, datos$g, shapiro.test)
cat(paste0(capture.output(cshapiro),collapse = '\n'),file = 'cshapiro.txt')

kruskal.test(vel~g, data=datos)

cwilcox<-pairwise.wilcox.test(datos$vel, datos$g)
cat(paste0(capture.output(cwilcox),collapse = '\n'),file = 'cwilcox.txt')
