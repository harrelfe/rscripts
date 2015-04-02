# title: Interactive Demo of Spline Fitting
# major: curve fitting
require(rms)
pag <- function(x,titl,df) {
  if(!show.constructed.variables) return(invisible())
  titl <- paste('Constructed Var. for ', titl, ' (',df,' d.f.)',sep='')
  page(x, title=titl, multi=TRUE)
  invisible()
}

show.constructed.variables <- FALSE

while (TRUE) {
	curves <- vector('list',5)
	nams <- c('Linear','Linear Spline','Quartic Polynomial',
			  'Unrestricted Cubic Spline',
			  'Restricted Cubic Spline')
	
	plot(0:1,0:1,xlab='x', ylab='y', type='n')
	cat('\nClick any points you desire.\nTerminate points with right-click.\n')
	z <- locator(type='p')
	x <- z$x
	y <- z$y
	xs <- seq(min(x),max(x),length=200)
	dx <- data.frame(x=xs)

	k <- 0
	while(k < 3) {
	  cat('\nClick >= 3 knot locations (y-coordinates ignored).\nTerminate points with right-click.\n')
	  knots <- locator()$x
	  abline(v=knots, lty=2)
	  k <- length(knots)
	  dfs <- c(1, 1+k, 4, 3+k, k-1)
	}

	f <- ols(y ~ x)
	curves[[1]] <- list(xs, f$coef[1]+xs*f$coef[2])

	f <- ols(y ~ lsp(x, knots), x=TRUE)
	pag(f$x,'Linear Spline',dfs[2])
	curves[[2]] <- list(xs, predict(f, dx))

	f <- ols(y ~ pol(x,4), x=TRUE)
	pag(f$x,'Quartic Polynomial',dfs[3])
	curves[[3]] <- list(xs, predict(f,dx))
	
	X <- cbind(x, x^2, x^3)
	for(i in 1:k) X <- cbind(X, pmax(x-knots[i],0)^3)
	f <- ols(y ~ X)
	dimnames(X)[[2]] <- c('x','x^2','x^3',rep('',k))
	pag(X,'Unrestricted Cubic Spline',dfs[4])
	X <- cbind(xs, xs^2, xs^3)
	for(i in 1:k) X <- cbind(X, pmax(xs-knots[i],0)^3)
	curves[[4]] <- list(xs, cbind(1,X) %*% coef(f))

	f <- ols(y ~ rcs(x,knots), x=TRUE)
	pag(f$x,'Restricted Cubic Spline',dfs[5])
	curves[[5]] <- list(xs, predict(f,dx))

	names(curves) <- paste(nams, ' (',dfs,')',sep='')
	labcurve(curves, col=1:5, pl=T, add=TRUE, keys='lines')
	title(sub='Left click for another example.  Click outside plot to stop',
			adj=0)
	title(sub='(d.f.)',adj=1)
	z <- locator(1)
	if(any(z$x < 0 | z$x > 1 | z$y < 0 | z$y > 1)) break
}
