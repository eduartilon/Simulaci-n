
area<-data.frame()

nsim <- 10 #how many rbinom are w
n <- 10
size = 1
secuen = seq(0.2,0.7,0.1)
repe<-1:3
for (rep in repe) {
for (prob in secuen) {
  
sim_data_vill_for_loop <- matrix(ncol = nsim, nrow = n)
for(i in seq(nsim)) #iterate from 1 to nsim
  sim_data_vill_for_loop[, i] <- rbinom(n, size = size, prob = prob)
suma<-sum(sim_data_vill_for_loop)

resultado<-c(suma,rep)
area=rbind(area,resultado)

print(suma)

}
}
