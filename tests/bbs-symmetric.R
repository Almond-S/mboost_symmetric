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
d <- d[order(d$t), ]
points(d[, -1])

mods <- list()
mods$x <- mboost(x ~ bbs(t, boundary.knots = c(0,1), #offset = 0,
                         cyclic = TRUE, constraint = "antisymmetric"), data = d)
mods$y <- mboost(y ~ bbs(t, boundary.knots = c(0,1), #offset = 0,
                         cyclic = TRUE, constraint = "symmetric"), data = d)
nd <- data.frame(t = t_grid, `0` = 0, check.names = F)
preds <- lapply(mods, predict, newdata = nd)
preds$x <- preds$x - extract(mods$x, "offset")
lines(preds)


d2 <- data.frame( 
  t = rep(d$t, 2), 
  dim = factor(rep(c("x", "y"), each = length(d$t))),
  value = c(d$x, d$y))
d2$dim_x <- d2$dim_y <- d2$dim
contrasts(d2$dim_x) <- contrasts(d2$dim, contrasts = FALSE)[,"x",drop = FALSE]
contrasts(d2$dim_y) <- contrasts(d2$dim, contrasts = FALSE)[,"y",drop = FALSE]
contrasts(d2$dim, 2) <- contrasts(d2$dim, contrasts = FALSE)

mod2 <- mboost(value ~ bbs(t, by = dim, boundary.knots = c(0,1), cyclic = TRUE, df = 20), 
               data = d2)
newdat <- data.frame(t = rep(t_grid, 2), dim = factor(rep(c("x", "y"), each = length(t_grid))))
contrasts(newdat$dim, 2) <- contrasts(newdat$dim, contrasts = FALSE)
pred2 <- predict(mod2, newdata = newdat)
lines(head(pred2, length(t_grid)), tail(pred2, length(t_grid)), col ="darkseagreen", t = "l")


ctr <- contrasts(d2$dim, FALSE)
mod3 <- mboost(value ~ bbs(t, by = dim_y, #knots = 3,
                           boundary.knots = c(0,1), df = 1,
                           constraint = "symmetric",
                           cyclic = TRUE) + 
                 bbs(t, by = dim_x, boundary.knots = c(0,1), df = 1, 
                     constraint = "antisymmetric",  #knots = 3,
                     cyclic = TRUE
                     ), offset = TRUE,
               data = d2)
pred3 <- predict(mod3)#, newdata = newdat)
plot(head(pred3, nrow(d)), tail(pred3, nrow(d)), lty = "dotted", col ="darkred", t = "b")
plot(pred3, t = "l")

newdat$dim_x <- newdat$dim_y <- factor(newdat$dim)
contrasts(newdat$dim_x) <- contrasts(newdat$dim_x, FALSE)[, "x", drop = F]
pred3 <- predict(mod3, newdata = newdat)
plot(head(pred3, length(t_grid)), tail(pred3, length(t_grid)), col ="darkred", t = "l")

d3 <- data.frame(
  t = rep(t_grid, 2), 
  value = c(draw(t_grid)) + rnorm(t_grid, sd = .01),
  dim = factor(rep(c("x", "y"), each = length(t_grid))))
d3$dim_x <- d3$dim_y <- d3$dim
d3$s <- d3$t
contrasts(d3$dim_x) <- contrasts(d3$dim, F)[, "x", drop = F]
mod4 <- mboost(value ~ bbs(t, by = dim_x, 
                           boundary.knots = c(0,1), df = 10, 
                           cyclic = TRUE,
                           constraint = "antisymmetric") %+% 
                 bbs(t, by = dim_y, 
                           boundary.knots = c(0,1), df = 10, 
                           cyclic = TRUE,
                           constraint = "symmetric"), offset = 0, data = d3)#[seq_along(t_grid), ])
d3$pred <- predict(mod4)
lines(d3$pred[d3$dim == "x"], d3$pred[d3$dim == "y"], t = "l")
plot(d3$pred[d3$dim == "x"], t = "l")
