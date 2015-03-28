# Quick way to compare multiple types of OLS and quantile regression fits 

set.seed(1)
n <- 20
x <- runif(n)
y <- abs(x - 0.5) + rnorm(n, mean=0, sd=.1)
require(rms)
mods <- list(y ~ x, y ~ pol(x, 2), y ~ pol(x, 3), y ~ pol(x, 4),
             y ~ pol(x, 5), y ~ rcs(x, 3), y ~ rcs(x, 4), y ~ rcs(x, 5))
z <- NULL
xeval <- seq(0, 1, length=100)
xd    <- data.frame(x=xeval)
for(method in c('OLS', 'Quantile Regression')) {
  for(mod in mods) {
    f <- switch(method,
                OLS=ols(mod),
                'Quantile Regression'=Rq(mod))
    p <- predict(f, newdata=xd)
    z <- rbind(z, data.frame(method=method, model=deparse(mod), x=xeval, y=p))
  }
}
d <- data.frame(x, y)
dtrue <- data.frame(x=c(0, .5, 1), y=c(.5, 0, .5))
ggplot(z, aes(x=x, y=y, color=method)) + geom_line() +
  geom_line(data=dtrue, color='blue', alpha=.5, linetype='dotted') +
  geom_point(data=d, alpha=.3) +
  facet_wrap(~ model)
