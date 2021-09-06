library(parallel)
dim <- 10
num <-  dim^2
p <- c(0.4, 0.7, 0.9)
datos = data.frame()

for (inicio in p) {
  for (rep in 1:3) {
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

for (iteracion in 1:30) {
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
 stopCluster(cluster)
}
nvivos = c(vivos)
print(nvivos)
prob= (sum(nvivos))/3
respuesta = c(inicio, prob)
datos = rbind(datos, respuesta)
}
