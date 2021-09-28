library(ggplot2)
desde <- 3
hasta <- 7
cuan <- c(100, 1000, 10000, 100000, 1000000, 10000000)
entero<- c(0.0488341111)
compar <- data.frame()
n = seq( 1, 10, 1)
j = c(1:50)

for (cuantos in cuan) {
for (rep in j) {
  f <- function(x) { return(1 / (exp(x) + exp(-x))) } # funcion que piden
  g <- function(x) { return((2 / pi) * f(x)) } # normalizado a distr
  suppressMessages(library(distr)) # paquete
  generador  <- r(AbscontDistribution(d = g)) # creamos un generador
  valores <- generador(cuantos) # generamos valores
  montecarlo = sum(valores >= desde & valores <= hasta) # checamos
  integral <- sum(montecarlo) / cuantos # tasa: integral para g(x)
decimales =((pi / 2) * integral)
print(decimales)


compa<- c(decimales)

for (i in n) {
  e = trunc(entero*10^i)/10^i
  co = trunc(compa*10^i)/10^i
  if (co == e) {num = i
  } else {break;}
}

values<-c(cuantos, num)

compar=rbind(compar, values)
}
}
names(compar) <- c("puntos", "decimales")
compar$puntos = as.factor(compar$puntos)

ggplot(compar, aes(x=puntos , y= decimales , fill= rep)) + # fill=name allow to automatically dedicate a color for each group
  geom_boxplot(fill = "#F8766D", colour = "#1F3552")+
  stat_boxplot(geom = "errorbar", width = 0.9)+
  theme(axis.line = element_line(colour = "black", size = 0.25))+
  coord_cartesian(ylim = c(0,8))+
  labs(x="Puntos", y= "Decimales")
