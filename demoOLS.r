# Quick way to compare multiple types of OLS model fits 

set.seed(1)
n <- 20
x <- runif(n)
y <- abs(x - 0.5) + rnorm(n, mean=0, sd=.1)
require(rms)
mods <- list(y ~ x, y ~ pol(x, 2), y ~ pol(x, 3), y ~ pol(x, 4),
             y ~ pol(x, 5), y ~ rcs(x, 3), y ~ rcs(x, 4), y ~ rcs(x, 5))
z <- NULL
xeval <- seq(0, 1, length=100)
for(mod in mods) {
  f <- ols(mod)
  p <- predict(f, data.frame(x=xeval))
  z <- rbind(z, data.frame(model=deparse(mod), x=xeval, y=p))
}
require(ggplot2)
d <- data.frame(x, y)
ggplot(z, aes(x=x, y=y)) + geom_line() + geom_point(data=d) +
  facet_wrap(~ model)
