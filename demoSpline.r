# title: Interactive Demo of Spline Fitting
# major: regression modeling
# minor: curve fitting
require(rms)
pag <- function(x,titl,df) {
  if(!show.constructed.variables) return(invisible())
  titl <- paste('Constructed Var. for ', titl, ' (',df,' d.f.)',sep='')
  page(x, title=titl, multi=TRUE)
  invisible()
}
nam <- function(w, dof) paste(w, ' (', dof, ' d.f.)', sep='')
worked <- function(f) ! (inherits(f, 'try-error') || any(is.na(coef(f))))

show.constructed.variables <- FALSE

while (TRUE) {
	curves <- list()
	
	plot(0:1,0:1,xlab='x', ylab='y', type='n')
	cat('\nClick any points you desire.\nTerminate points by mouse right-click (Esc if under RStudio).\n')
	z <- locator(type='p')
	x <- z$x
	y <- z$y
	xs <- seq(min(x), max(x), length=200)
	dx <- data.frame(x=xs)

	k <- 0
	while(k < 3) {
	  cat('\nClick >= 3 knot locations (y-coordinates ignored).\nTerminate points by mouse right-click (Esc if under RStudio).\n')
	  knots <- locator()$x
	  abline(v=knots, lty=2)
	  k <- length(knots)
	  dfs <- c(1, 1 + k, 4, 3 + k, k - 1)
	}

	f <- ols(y ~ x)
	curves[[nam('Linear', 1)]] <- list(xs, f$coef[1] + xs * f$coef[2])

	f <- try(ols(y ~ lsp(x, knots), x=TRUE))
  if(worked(f)) {
    pag(f$x,'Linear Spline',dfs[2])
    curves[[nam('Linear Spline', 1 + k)]] <- list(xs, predict(f, dx))
  }

	f <- try(ols(y ~ pol(x,4), x=TRUE))
  if(worked(f)) {
    pag(f$x,'Quartic Polynomial',dfs[3])
    curves[[nam('Quartic Polynomial', 4)]] <- list(xs, predict(f,dx))
  }
	
	X <- cbind(x, x^2, x^3)
	for(i in 1 : k) X <- cbind(X, pmax(x - knots[i], 0) ^ 3)
	f <- try(ols(y ~ X, tol=1e-8), silent=TRUE)
  if(worked(f)) {
    dimnames(X)[[2]] <- c('x','x^2','x^3',rep('',k))
    pag(X,'Unrestricted Cubic Spline', dfs[4])
    X <- cbind(xs, xs^2, xs^3)
    for(i in 1 : k) X <- cbind(X, pmax(xs - knots[i], 0) ^ 3)
    curves[[nam('Unrestricted Cubic Spline', 3 + k)]] <-
      list(xs, cbind(1,X) %*% coef(f))
  }

	f <- try(ols(y ~ rcs(x, knots), x=TRUE))
	pag(f$x,'Restricted Cubic Spline',dfs[5])
  if(worked(f))
    curves[[nam('Restricted Cubic Spline', k - 1)]] <- list(xs, predict(f, dx))
	labcurve(curves, col=1 : length(curves), pl=TRUE, add=TRUE, keys='lines')
	title(sub='Left click for another example.  Click outside plot to stop',
			adj=0)
	z <- locator(1)
	if(any(z$x < 0 | z$x > 1 | z$y < 0 | z$y > 1)) break
}
