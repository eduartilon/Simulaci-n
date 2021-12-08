molecula<-c(78)#Número atómico
superficie_cubierta<-c(20,30,40,50,60,70) #Norma de área
separacion<-c(5,10,15)#Norma de separación

j<-1:2#Repeticiones

datos<-data.frame()

for (rep in j) {

for (mole in molecula) {
  
  for (superficie in superficie_cubierta) {
    
    for (sepa in separacion) {
      
      if (sepa<=5){ #Genera para distancia de 5
        resistividad<-((mole*superficie)*0.2)/(1/sepa)}
      
      else if(sepa<=10){ #Genera para distancia de 10
        resistividad<-((mole*superficie)*0.2)/(1/sepa)}
      
      else if(sepa<=15){ #Genera para distancia de 15
         resistividad<-((mole*superficie)*0.52)/(1/sepa)}
      
      resultado<-c(mole,superficie,sepa,resistividad)# Añadir al data.frame
      datos=rbind(datos,resultado)
    }
  }
}
}

area<-data.frame()

nsim <- 10 #Cuantos rbinom existen
n <- 10 #Largo de lado de matriz
size = 1 #Solo 0 y 1
secuen = c(0.2,0.2,0.2,0.3,0.3,0.3,0.4,0.4,0.4,0.5,0.5,0.5,0.6,0.6,0.6,0.7,0.7,0.7) # probabilidades de llenado de matriz

for (repe in j) {
  
for (prob in secuen) {
  
  matriz <- matrix(ncol = nsim, nrow = n)#Genera matriz
  for(i in seq(nsim)) #Itera de 1 a nsim
    matriz[, i] <- rbinom(n, size = size, prob = prob)
  suma<-sum(matriz)#"Superficie cubierta"
  
  resul<-c(suma)
  area=rbind(area,resul)#Guardar resultados en data.frame
  
  print(suma)
  
}
}
df<-cbind(datos, area) #Combinar los data.frames

names(df) = c("molecula", "superficie", "separacion", "resistencia","recubierta")



library(ggplot2)#Gráficas
library(scales)#Personalizar etiquetas


ggplot(df, aes(x= recubierta, y= resistencia, color=separacion)) + 
  geom_point(size=2)+
  geom_smooth(aes(group = separacion),method = "loess", se=FALSE, formula =y ~ x)+#Línea de tendencia
  guides(scale = "10", color=guide_legend(title = "Separación"))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(label = unit_format(unit = "%", scale = 1, sep = ""),limits=c(0, 80)) + # % en las etiquetas 
  scale_y_continuous(label = unit_format(unit = "k", scale = 1e-3, sep = ""),limits = c(0,48000))+# k en las etiquetas
  labs(x="Superficie cubierta de nanopartículas")+
  ylab(expression("Resistencia"~"/"*Omega*"")) #Símbolo Omega

#Fin de la simulación

#¡Correr después de obtener la gráfica anterior!

#Inicio de datos estadísticosw

df$recubierta = as.factor(df$recubierta) #Obtener factor para las x
datoss = split.data.frame(df, f = df$separacion) #Separar en grupos

#Gráfica de 5 de separación
ggplot(datoss$`5`, aes(x= recubierta, y= resistencia)) + 
  geom_boxplot(fill = "#F8766D", colour = "#1F3552")+ 
  labs(x="Superficie cubierta de nanopartículas en %") +
  geom_violin(fill="green", color="yellow",alpha = 5/10)+
  ylab(expression("Resistencia"~"/"*Omega*""))+
  scale_y_continuous(label = unit_format(unit = "k", scale = 1e-3, sep = ""),limits = c(1000,5800))

#Gráfica de 10 de separación
ggplot(datoss$`10`, aes(x= recubierta, y= resistencia)) + 
  geom_boxplot(fill = "#3FCF30", colour = "#1F3552")+ 
  labs(x="Superficie cubierta de nanopartículas en %") +
  geom_violin(fill="green", color="yellow",alpha = 5/10)+
  ylab(expression("Resistencia"~"/"*Omega*""))+
  scale_y_continuous(label = unit_format(unit = "k", scale = 1e-3, sep = ""),limits = c(3000,10000))

#Gráfica de 15 de separación
ggplot(datoss$`15`, aes(x= recubierta, y= resistencia)) + 
  geom_boxplot(fill = "#FAFA2D", colour = "#1F3552")+ 
  labs(x="Superficie cubierta de nanopartículas en %") +
  geom_violin(fill="green", color="yellow",alpha = 5/10)+
  ylab(expression("Resistencia"~"/"*Omega*""))+ 
  scale_y_continuous(label = unit_format(unit = "k", scale = 1e-3, sep = ""),limits = c(10000,45000))
  
library(tidyverse)

dat5<-datoss$`5`%>%
  group_by(recubierta) %>%
  summarise(
    
    promedio = mean(resistencia, na.rm = TRUE),
    desviacion_std = sd(resistencia, na.rm = TRUE),
    varianza = sd(resistencia, na.rm = TRUE)^2,
    mediana = median(resistencia, na.rm = TRUE),
    rango_intercuartil = IQR(resistencia, na.rm = TRUE)
  )

dat10<-datoss$`10`%>%
  group_by(recubierta) %>%
  summarise(
    
    promedio = mean(resistencia, na.rm = TRUE),
    desviacion_std = sd(resistencia, na.rm = TRUE),
    varianza = sd(resistencia, na.rm = TRUE)^2,
    mediana = median(resistencia, na.rm = TRUE),
    rango_intercuartil = IQR(resistencia, na.rm = TRUE)
  )

dat15<-datoss$`15`%>%
  group_by(recubierta) %>%
  summarise(
    
    promedio = mean(resistencia, na.rm = TRUE),
    desviacion_std = sd(resistencia, na.rm = TRUE),
    varianza = sd(resistencia, na.rm = TRUE)^2,
    mediana = median(resistencia, na.rm = TRUE),
    rango_intercuartil = IQR(resistencia, na.rm = TRUE)
  )

tapply(df$resistencia, df$separacion, shapiro.test)
kruskal.test(resistencia~separacion, data=df)
pairwise.wilcox.test(df$resistencia, df$separacion)
