# title: Interactive RStudio Simulation of Noise and Categorization
# major: statistical pitfalls
# minor: categorization
# Interactive demonstration of power loss by categorizing
# a single x in a linear model, and by introducing
# measurement error in x.  The categorical analysis amounts to a
# two-sample t-test, and the continuous test a test of whether the
# slope is zero after fitting a straight line.  If the user specifies
# a change in slope after x=0, the slope test will be suboptimal.
# But it will often still be better than the test using categorization.
# If the true relationship is nonlinear, a better approach is to fit a
# nonlinear relationship.  Here the power of a simple quadratic
# regression is shown.
#
# Frank Harrell  Vanderbilt Biostatistics  8May11

sim <- function(n=200, nsim=1000, slope=1, slope2=1, sigma=4, sigmaNoise=0,
                cutpt=0, eerror=FALSE)
  {
    x <- rnorm(n)
    zcont <- zcat <- r2cont <- r2cat <- pcont <- pquad <- pcat <-
      numeric(nsim)
    par(mfrow=c(2,2))
    cl <- gray(.75)
    xnoise <- if(sigmaNoise == 0) x else x + rnorm(n, 0, sd=sigmaNoise)
    stats <- function(x, y)
    {
    	k <- lm.fit.qr.bare(x, y)
    	r2 <- k$rsquared
    	a <- NCOL(x)
    	b <- NROW(x) - a - 1
    	fstat <- r2/(1 - r2) * b / a
      p <- 1 - pf(fstat, a, b)
      list(r2 = r2, p=p, z=-qnorm(pmax(p, 1e-15)/2))
    }
   
    for(i in 1:nsim)
      {
        if(i == 1)
          {
            rr <- range(x, xnoise)
            plot(x, xnoise, xlab='Original x', ylab='x with Noise',
                 sub='First iteration', cex.sub=.6,
                 xlim=rr, ylim=rr,
                 cex.lab=.8, cex.main=.9, main='Effect of Noise')
            abline(a=0, b=1, col=cl)
            abline(h=cutpt, col=cl)
            propwrong <- mean((x > cutpt) != (xnoise > cutpt))
            ee <- paste('Prop. misclassified >', cutpt,':', round(propwrong, 2), sep='')
            if(eerror)
            {
            	diff <- mean(x[x > cutpt]) - mean(x[x <= cutpt])
            	pdiff <- propwrong * diff
            	ee <- paste(ee, '\nEffective |error|:', round(pdiff, 2), sep='')
            }
            text(rr[2], rr[1]+.06*diff(rr), ee, adj=1, cex=.7, col='blue')
            text(rr[1], rr[2]-.02*diff(rr), paste('Mean |error|:',
             round(mean(abs(x - xnoise)), 2), sep=''), adj=0, cex=.7, col='blue')
            lines(c(min(x), 0), slope*c(min(x), 0), col='red')
            lines(c(0, max(x)), slope*c(0, max(x)) +
                  (slope2 - slope)*c(0, max(x)), col='red')
            title(sub='Line:\nTrue relationship with Y', col.sub='red', cex.sub=.7, adj=1, line=2.75)
        }
        y <- slope*x + (slope2 - slope)*pmax(x, 0) + rnorm(n, 0, sd=sigma)
        k <- stats(xnoise, y)
        zcont[i] <- k$z
        r2cont[i] <- k$r2
        pcont[i] <- k$p
        pquad[i] <- stats(cbind(xnoise, xnoise^2), y)$p
        k <- stats(1*(xnoise < cutpt), y)
        zcat[i] <- k$z
        r2cat[i] <- k$r2
        pcat[i] <- k$p
      }
    rr <- range(c(r2cont, r2cat))
    plot(r2cont, r2cat, xlim=rr, ylim=rr,
			xlab=expression(paste(R^2, ' continuous')),
      ylab=expression(paste(R^2, ' dichotomous')),
      cex.lab=.8, main='Explained Variation in Y', cex.main=.9)
    abline(a=0, b=1, col=cl)
    rr <- range(c(zcont, zcat))
    plot(zcont, zcat, xlim=rr, ylim=rr,
         xlab='Z-value r-test',
         ylab=expression(paste('Z-value', phantom(.), t,
         phantom(.), 'test')), main='Z Statistics',
         cex.lab=.8, cex.main=.9)
    abline(a=0, b=1, col=cl)
    powercont <- round(mean(pcont < 0.05), 3)
    powerquad <- round(mean(pquad < 0.05), 3)
    powercat  <- round(mean(pcat < 0.05), 3)
    pow <- format(c(powercont, powerquad, powercat))
    plot.new()
    text(c(.1,.7), c(.92,.92), c('x Modeled As','Power'),
      adj=0, cex=.9)
    lines(c(.1, 1), c(.8, .8), col=cl)
    lines(c(.67,.67), c(.98,.55), col=cl)
    text(c(.1,.1), c(.7, .6, .5),
      c('Linear', 'Quadratic', paste('Cut at', cutpt)), adj=0, cex=.8)
    text(c(.92, .92), c(.7, .6, .5), pow, adj=1, cex=.8)
    text(.1, .25, paste(nsim, 'simulations'), adj=0, cex=.8)
  }

require(manipulate)
require(Hmisc)
manipulate(sim(n=n, nsim=nsim, sigma=sigma, sigmaNoise=sigmaNoise,
               slope2=slope2, cutpt=Cutpoint),
           n=slider(200, 1000, step=100),
           sigma=slider(2, 10, label='Residual S.D.'),
           slope2=slider(-1, 3, step=.25, initial=1, label='Slope after x=0'),
           sigmaNoise=slider(0, 5, step=.5, label='Noise S.D.'),
           Cutpoint=slider(0, 3, step=.5,
             label='Cut point (optimal=mean x=0)'),
           nsim=slider(300,2000, step=100, initial=500,
             label='No. of simulations'))

