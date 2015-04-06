# title: Interactive Demo of Least Squares and Other Fitting
# major: regression modeling
# minor: curve fitting
require(Hmisc)
require(MASS)
while (TRUE) {
	curves <- vector('list',4)
	names(curves) <- c('Least Squares','Least Sum of\nAbsolute Deviations',
						  'Least Median\nSquared Deviation','Loess')
	
	plot(0:1,0:1,xlab='x', ylab='y', type='n')
	cat('\nClick any points you desire.  Terminate points with right-click.\n')
	z <- locator(type='p')
	f <- lsfit(z$x, z$y)	
	abline(f, lwd=3)
	segments(z$x, z$y, z$x, z$y - f$residuals, lwd=1, col=3)
	x <- c(0,1)
	curves[[1]] <- list(x, f$coef[1]+(0:1)*f$coef[2])
    f <- rlm(cbind(1,z$x), z$y)
	abline(coef(f), col=2)
	curves[[2]] <- list(x, f$coef[1] + (0:1)*f$coef[2])
	n <- length(z$x)
	if(n > 4) {
		f <- lmsreg(z$x, z$y)
		abline(f, col=3)
		curves[[3]] <- list(x, f$coef[1] + (0:1)*f$coef[2])
	}
	f <- lowess(z)
	lines(f, col=4)
	curves[[4]] <- f
	if(n <= 4) curves[[3]] <- NULL
	labcurve(curves, col=1:4, tilt=T)
	title(sub='Left click for another example.  Click outside plot to stop',
			adj=0)
	z <- locator(1)
	if(any(z$x < 0 | z$x > 1 | z$y < 0 | z$y > 1)) break
}
