library(ggplot2)
entero = 3.141592 
n = seq(1, 6, 1)
cuan = c(1000, 10000, 1000000)
compar = data.frame()
j = c(1:15)

for (cuantos in cuan) {
  for (rep in j){
    interior = 0
    for (r in 1:cuantos) {
      x = runif(1, -1, 1)
      y = runif(1, -1, 1)
      d = sqrt(x*x + y*y)
      if (d < 1) {
        interior = interior + 1
      }
    }
    
    tasa = interior / cuantos
    pi = 4 * tasa
    
    print(pi)
    for (i in n) {
      e = trunc(entero*10^i)/10^i
      co = trunc(pi*10^i)/10^i
      
      if (co == e) {
        num = i
      } else {
        break
      }
    }
    datos <- c(cuantos, num)
    compar = rbind(compar, datos)
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

shapiro.test(compar$decimales)

kruskal.test(decimales~puntos, data=compar)
