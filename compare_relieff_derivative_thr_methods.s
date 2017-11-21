library(tidyverse)
library(CORElearn)
library(numDeriv)
library(quantmod)
library(features)

load("gm_rf_weights.RData")
x <- seq(1, length(rf_weights))
y <- sort(rf_weights, decreasing = TRUE) 

#first method
#in house function plus double smoothing

deriv <- function(x,y) {
  output <- diff(y)/diff(x)
  return(output)
}

middle_pts <- function(x){
  pts <- x[-1] - diff(x) / 2
  return(pts)
}


smoothed_y <- predict(smooth.spline(y))$y

second_d <- deriv(middle_pts(x), deriv(x, smoothed_y))

smooth_second_d <- loess(second_d ~ midpts,
                         data.frame(second_d = second_d, midpts = seq(1:length(second_d)), model = T))

smooth_second_d_other <- predict(smooth.spline(second_d))$y

otp <- predict(smooth_second_d)

thr <- findValleys(otp)[1]

pdf("first_method.pdf", w = 8, h = 6)
par(mfrow = c(2,2))

plot(y)
points(findValleys(otp)[1],y[findValleys(otp)[1]], pch = 15, cex = 2, col = "magenta")
plot(smoothed_y)
points(findValleys(otp)[1],y[findValleys(otp)[1]], pch = 15, cex = 2, col = "magenta")
plot(y = second_d, x = middle_pts(middle_pts(x)))
points(findValleys(otp)[1],0, pch = 15, cex = 2, col = "magenta")
plot(y = otp, x = middle_pts(middle_pts(x)))
points(findValleys(otp)[1],0, pch = 15, cex = 2, col = "magenta")
dev.off()

pdf("first_method_spline_smooth_second_d.pdf", w = 8, h = 6)
par(mfrow = c(3,3))
#second plot with splinesmooth of the second derivative
plot(y)
points(findValleys(smooth_second_d_other)[1],y[findValleys(smooth_second_d_other)[1]], pch = 15, cex = 2, col = "magenta")
plot(smoothed_y)
points(findValleys(smooth_second_d_other)[1],y[findValleys(smooth_second_d_other)[1]], pch = 15, cex = 2, col = "magenta")
plot(y = second_d, x = middle_pts(middle_pts(x)))
points(findValleys(smooth_second_d_other)[1],0, pch = 15, cex = 2, col = "magenta")
plot(y = smooth_second_d_other, x = middle_pts(middle_pts(x)))
points(findValleys(smooth_second_d_other)[1],0, pch = 15, cex = 2, col = "magenta")
plot(smoothed_y[1:1000])
points(findValleys(smooth_second_d_other)[1],y[findValleys(smooth_second_d_other)[1]], pch = 15, cex = 2, col = "magenta")
plot(y = smooth_second_d_other[1:1000], x = 1:1000)
points(findValleys(smooth_second_d_other)[1],0, pch = 15, cex = 2, col = "magenta")
dev.off()


#second method, ready made second derivative
d2 <- features(seq(1,length(smoothed_y)), smoothed_y) %>%
  attributes(.) %>%
  `$`(.,"fits") %>%
  `$`(.,"d2")
smooth_d2 <- predict(smooth.spline(d2))$y
pdf("second_method.pdf", w = 8, h = 6)
par(mfrow = c(3,3))
#second plot with splinesmooth of the second derivative
plot(y)
points(findValleys(smooth_d2)[1],y[findValleys(smooth_d2)[1]], pch = 15, cex = 2, col = "magenta")
plot(smoothed_y)
points(findValleys(smooth_d2)[1],y[findValleys(smooth_d2)[1]], pch = 15, cex = 2, col = "magenta")
plot(y = d2, x = x)
points(findValleys(smooth_d2)[1],0, pch = 15, cex = 2, col = "magenta")
plot(y = smooth_d2, x = x)
points(findValleys(smooth_d2)[1],0, pch = 15, cex = 2, col = "magenta")
plot(smoothed_y[1:1000])
points(findValleys(smooth_d2)[1],y[findValleys(smooth_d2)[1]], pch = 15, cex = 2, col = "magenta")
plot(y = smooth_d2[1:1000], x = 1:1000)
points(findValleys(smooth_d2)[1],0, pch = 15, cex = 2, col = "magenta")
dev.off()

first_method_thr <- thr <- findValleys(otp)[1]

first_method_bis_thr <- findValleys(smooth_second_d_other)[1]

second_method_thr <- findValleys(smooth_d2)[1]

