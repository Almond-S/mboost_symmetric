library(mboost)

set.seed(239)
n <- 10
d_symm <- data.frame(x = rnorm(n))
d_symm$mu <- dnorm(d_symm$x)
d_symm$y <- d_symm$mu + rnorm(n, sd = .1)
d_symm <- d_symm[order(d_symm$x), ]

x <- seq(-2,2, len = 100)
plot(x, dnorm(x), t = "l", col = "grey", ylim = range(c(d_symm$y, dnorm(x))))
points(d_symm[, c("x", "y")], pch = 4)

m <- mboost(y ~ bbs(x), offset = 0, data = d_symm)
lines(m, t = "b", lty = "dotted")

m_symm <- mboost(y ~ bbs(x, constraint = "symmetric", cyclic = TRUE, df = 2,
                         boundary.knots = c(-2,2)), offset = 0, data = d_symm)
lines(m_symm, t = "p", lty = "dashed", col = "cornflowerblue")
d_symm_pred <- data.frame(x = x[findInterval(x, range(d_symm$x)) == 1])
d_symm_pred$y <- predict(m_symm, newdata = d_symm_pred)
lines(d_symm_pred, 
      lty = "dashed", col = "cornflowerblue")

m_anti <- mboost(y ~ bbs(x, constraint = "antisymmetric", cyclic = TRUE, df = 2,
                         boundary.knots = c(-2,2)), offset = 0, data = d_symm)
lines(m_anti, t = "p", pch = 3, col = "darkred")
d_symm_pred$y_anti <- predict(m_anti, newdata = d_symm_pred)
lines(d_symm_pred$x, d_symm_pred$y_anti, 
      lty = "dotdash", col = "darkred", xpd = TRUE)



# axissymmetric plane curves --------------------------------------------------------

draw <- function(t) {
  x <- function(t) sin(t*2*pi) * t * (t - .5)
  y <- function(t) sqrt(t)
  cbind(
    x = ifelse(t < .5, x(t), -x(1-t)),
    y = ifelse(t < .5, y(t), y(1-t)))
}

t_grid <- seq(0,1, len = 100)
plot(draw(t_grid), type = "l", col = "grey")

set.seed(8932)
d <- data.frame(t = runif(30))
d <- cbind(d, draw(d$t))
points(d[, -1])

mods <- list()
mods$x <- mboost(x ~ bbs(t, boundary.knots = c(0,1), 
                         cyclic = TRUE, constraint = "antisymmetric"), data = d)
mods$y <- mboost(y ~ bbs(t, boundary.knots = c(0,1), 
                         cyclic = TRUE, constraint = "symmetric"), data = d)
preds <- lapply(mods, predict, newdata = data.frame(t = t_grid))
lines(preds)


d2 <- data.frame( 
  t = rep(d$t, 2), 
  dim = rep(c("x", "y"), each = length(d$t)),
  value = c(d$x, d$y), 
  dim_x = rep(c(1, 0), each = length(d$t)), 
  dim_y = rep(c(0, 1), each = length(d$t)))

mod2 <- mboost(value ~ bbs(t, by = dim_x, cyclic = TRUE, constraint = "antisymmetric", boundary.knots = c(0,1)) %+% 
                 bbs(t, by = dim_y, cyclic = TRUE, constraint = "symmetric", boundary.knots = c(0,1)), 
               data = d2)
pred2 <- cbind(
  x = predict(mod2, newdata = data.frame(t = t_grid, dim_x = 1, dim_y = 0)),
  y = predict(mod2, newdata = data.frame(t = t_grid, dim_x = 0, dim_y = 1)))
plot(pred2, col ="darkseagreen", t = "l")

bx <- with(d2, bbs(t, by = dim_x, cyclic = TRUE, constraint = "antisymmetric", boundary.knots = c(0,1)))
Xx <- extract(bx, "design")
View(Xx)
