desde <- 3
hasta <- 7
cuan <- c(10, 100, 1000)
entero<- c(0.04883411)
datas=data.frame()
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

valores <- c(cuantos, decimales)

datas = rbind(datas, valores)


compa<- c(decimales)

for (i in n) {
  e = trunc(entero*10^i)/10^i
  co = trunc(compa*10^i)/10^i
  if (co == e) {num = i
  } else {break;}
}

compar=rbind(compar, num)
}
}

