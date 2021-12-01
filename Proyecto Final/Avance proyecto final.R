molecula<-c(79)
superficie_cubierta<-c(20,30,40,50,60,70,20,30,40,50,60,70)
separacion<-c(5,10,15)

datos<-data.frame()

for (mole in molecula) {
  
  for (superficie in superficie_cubierta) {
    
    for (sepa in separacion) {
      if (sepa<=5){
        resistividad<-((mole*superficie)*0.2)/(1/sepa)}
      else if(sepa<=10){
        
        resistividad<-((mole*superficie)*0.2)/(1/sepa)}
      else if(sepa<=15){
        
        resistividad<-((mole*superficie)*0.52)/(1/sepa)}
      
      resultado<-c(mole,superficie,sepa,resistividad)
      datos=rbind(datos,resultado)
    }
  }
}

names(datos) = c("molecula", "superficie", "separacion", "resistencia")

library(tidyr)

etiquetas=rep(c("a","b","c","a","b","c","d","e","f","d","e","f","h","i","j","h","i","j","a","b","c","a","b","c","d","e","f","d","e","f","h","i","j","h","i","j"))
datos$grupo <- etiquetas 

df2 <- lapply(split(datos, datos$grupo),
              function(subdf) subdf[sample(1:nrow(subdf), 4),]
)
af2<-do.call('rbind', df2)

nuevasup=rep(c("20","30","20","30","20","30","20","30","20","30","20","30",
               "40","50","40","50","40","50","40","50","40","50","40","50",
               "60","70","60","70","60","70","60","70","60","70","60","70"))

af2$superficial <- nuevasup


library(ggplot2)
af2$superficial = as.numeric(af2$superficial)

ggplot(af2, aes(x= superficial, y= resistencia, color=separacion)) + 
  geom_point(size=2)+
  geom_smooth(aes(group = separacion),method = "loess", se=FALSE, formula =y ~ x)+
  guides(scale = "10", color=guide_legend(title = "Separación"))+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(0, 100)
