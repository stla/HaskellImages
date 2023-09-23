library(RcppColors)
library(Bessel)
x <- y <- seq(-1.2, 1.2, len = 512)
# complex grid
W <- outer(y, x, function(x, y) complex(real = x, imaginary = y))
# computes Bessel values
Z <- matrix(BesselY(W, nu = -1), nrow = nrow(W), ncol = ncol(W))
# maps them to colors
image <- colorMap1(1/Z)
# plot
opar <- par(mar = c(0,0,0,0), bg = "#15191E")
plot(
  c(-100, 100), c(-100, 100), type = "n", 
  xlab = "", ylab = "", axes = FALSE, asp = 1
)
rasterImage(image, -100, -100, 100, 100, angle=45)
par(opar)

######
sprintf("  %f :+ %f", c(12.23456, 13.5), c(12.23456, 13.5)) |> 
  cat(sep = ",\n")

######
library(Bessel)
x <- y <- seq(-1.2, 1.2, len = 512)
Grid <- expand.grid(X = x, Y = y)
B <- with(Grid, 1/BesselY(complex(real = X, imaginary = Y), nu = -1))

dat <- data.frame(re = Re(B), im = Im(B))
write.csv(dat, "Bessel.csv", row.names = FALSE)

sprintf("  %f :+ (%f)", Re(B), Im(B)) |> 
  cat(sep = ",\n", file = "BesselData.hs")

# complex grid
W <- outer(y, x, function(x, y) complex(real = x, imaginary = y))
Z <- matrix(BesselY(W, nu = -1), nrow = nrow(W), ncol = ncol(W))
Array <- character(512L * 512L)
counter <- 1L
for(i in 0L:511L) {
  for(j in 0L:511) {
    z <- Z[i+1L, j+1L]
    Array[counter] <- 
      sprintf("  ((%d, %d), (%.8f) :+ (%.8f))", i, j , Re(z), Im(z))
    counter <- counter + 1L
  }
}
cat(Array, sep = ",\n", file = "BesselArray.hs")


# separate x and y 
sprintf("  %.8f", Re(B)) |> 
  cat(sep = ",\n", file = "BesselDataX.hs")
sprintf("  %.8f", Im(B)) |> 
  cat(sep = ",\n", file = "BesselDataY.hs")
