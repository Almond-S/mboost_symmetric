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
