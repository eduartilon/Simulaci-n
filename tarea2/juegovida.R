library(parallel)
library(ggplot2)
dim <- 10
num <-  dim^2
p <- c(0.1, 0.3, 0.5,0.7,0.9)
datos = data.frame()
datos1 = data.frame()
for (inicio in p) {
  for (rep in 1:30) {
actual <- matrix(round(runif(num) < p), nrow=dim, ncol=dim, byrow=TRUE)
paso <- function(pos) {
    fila <- floor((pos - 1) / dim) + 1
    columna <- ((pos - 1) %% dim) + 1
    vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                        max(columna - 1, 1): min(columna + 1, dim)]
    return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}
  
cluster <- makeCluster(detectCores() - 2)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

for (iteracion in 1:50) {
     clusterExport(cluster, "actual")
     siguiente <- parSapply(cluster, 1:num, paso)
     vivos = sum(siguiente)
     cat(inicio, rep, iteracion, vivos, '\n')
     actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
     }
     if (vivos == 0) { 
     vivos = 0 # todos murieron
     print("Ya no queda nadie vivo.")
     
     }else { vivos = 1
     print("hay vida")
 }

respuesta = c(inicio, vivos)
datos = rbind(datos, respuesta) 
stopCluster(cluster)
}

}
names(datos)<-c("probabilidad", "vida")
datos1 = aggregate(vida~probabilidad, datos, mean)
ggplot(datos1, aes(x = probabilidad, y = (vida*100) )) +
geom_line()+geom_point()+
  labs(x="Probabilidad al inicio", 
       y="% de probabilidad de vida", 
       title = "Probabilidad de vida a partir de un estado inicial dado")+
  scale_x_continuous(limits = c(0,1))+
  coord_cartesian(ylim = c(0,10))
