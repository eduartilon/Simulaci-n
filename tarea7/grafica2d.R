g <- function(x, y) {
  return((10* cos(x^2) - 10 * x + 12 * cos(y^2) -5* y))
}
png("p7_2d.png", width=700, height=700)
x <- seq(-1, 6, 0.4)
y <-  x
z <- outer(x, y, g)
dimnames(z) <- list(x, y)
library(reshape2) # recuerda instalar paquetes antes de intentar su uso
d <- melt(z)
names(d) <- c("x", "y", "z")
library(lattice) # lo mismo aplica con este paquete
png("p7_flat_2.png", width=500, height=500)
levelplot(z ~ x * y, data = d)
graphics.off()
