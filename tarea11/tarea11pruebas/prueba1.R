poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- sample(1:varcount, 1)
    deg <- sample(0:maxdeg, 1)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars) {
  value <- 0.0
  terms = dim(pol)[1]
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

## NUEVO!!!
domin.by <- function(target, challenger) {
  # sum sobre los TRUE/FALSE
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora (en maximizar)
  } # si no hay empeora, vemos si hay mejora
  # sum sobre los TRUE/FALSE
  return(sum(challenger > target) > 0)
}

datos = data.frame()

vc <- 4
md <- 3
tc <- 5
ka <- c(2,3,4,5) # cuantas funciones objetivo
obj <- list()
je<- 1:20


for (k in ka) {
for (repe in je) {

for (i in 1:k) {
  obj[[i]] <- poli(md, vc, tc)
}
minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim) # neg -> min, pos -> max
n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)
for (i in 1:n) { # evaluamos las soluciones
  for (j in 1:k) { # para todos los objetivos
    val[i, j] <- eval(obj[[j]], sol[i,])
  }
}
mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")

no.dom <- logical()# TRUE/FALSE segun si nadie lo domina
dominadores <- integer()
for (i in 1:n) { # para cada asignacion
  d <- logical() # quienes le dominan (si / no)
  for (j in 1:n) { # para todos los demas
    # i es a quien le retan, j es quien esta retando
    # lo comparamos como si todo fuese max (min f = max -f)
    d <- c(d, domin.by(sign * val[i,], sign * val[j,]))
  }
  
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  no.dom <- c(no.dom, sum(d) == 0) # nadie le domina (puros FALSE)
}
# agarra solo los que tienen TRUE en no.dom
frente <- subset(val, no.dom) # solamente las no dominadas

porcentaje = (length(frente[,1])/n)*100



resultado = c(k, repe, porcentaje)
datos = rbind(datos, resultado)
names(datos) = c("k", "Replica", "Porcentaje")
}
}


library(ggplot2) # recordar instalar si hace falta

datos$k = as.factor(datos$k)
gr <- ggplot(datos, aes(x=k, y=Porcentaje)) + geom_violin(fill="green", color="yellow")
gr + geom_boxplot(width=0.2, fill="red", color="blue", lwd=0.5) +
  labs(x = "Cantidad de funciones objetivo", y = "% de soluciones Pareto")




library(ggplot2)# recordar instalar si hace falta

png("p11_violin.png")
ggplot(datos, aes(x=k, y=Porcentaje)) + geom_violin(fill="orange", color="red") + geom_boxplot(width=0.2, fill="blue", color="green", lwd=2) +
  xlab("Cantidad de funciones objetivo") +
  ylab("% de soluciones Pareto") +
  ggtitle("% de soluciones Pareto")
graphics.off()
