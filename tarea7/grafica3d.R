g <- function(x, y) {
  return((10* cos(x^2) - 10 * x + 12 * cos(y^2) -5* y))
}
x <- seq(-1, 6, 0.4)
y <-  x
z <- outer(x, y, g)
png("p7_2d.png", width=700, height=700)
persp(x, y, z, shade=0.2, col='orange', theta=40, phi=30)
graphics.off()
