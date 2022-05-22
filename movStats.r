##' Moving Estimates Using Overlapping Fixed-width Windows
##' 
##' Function to compute moving averages and other statistics as a function
##' of a continuous variable, possibly stratified by another variable.
##' Estimates are made by creating overlapping moving windows and
##' computing the statistics defined in the stat function for each window.
##' The default method, `space='n'` creates varying-width intervals each having a sample size of `2*eps +1`, and the smooth estimates are made every `xinc` observations.  Outer intervals are not symmetric in sample size (but the mean x in those intervals will reflect that) unless `eps=10`, as outer intervals are centered at observations 10 and n - 10 + 1.  The mean x-variable within each windows is taken to represent that window.  If `trans` and `itrans` are given, x means are computed on the `trans(x)` scale and then `itrans`'d.  For `space='x'`, by default estimates are made on to the 10th smallest to the 10th largest
##' observed values of the x variable to avoid extrapolation and to
##' help getting the moving statistics off on an adequate start for
##' the left tail.  Also by default the moving estimates are smoothed using `supsmu`.
##' When `melt=TRUE` you can feed the result into `ggplot` like this:
##' `ggplot(w, aes(x=age, y=crea, col=Type)) + geom_line() +`
##'   `facet_wrap(~ Statistic)`
##'
##' See [here](https://www.fharrell.com/post/rflow#descriptively-relating-one-variable-to-another) for examples
##'
##' @title movStats
##' @author Frank Harrell
##' @md
##' @param formula a formula with the analysis variable on the left and the x-variable on the right, following by an optional stratification variable
##' @param stat function of one argument that returns a named list of computed values.  Defaults to computing mean and quartiles + N except when y is binary in which case it computes moving proportions.  If y has two columns the default statistics are Kaplan-Meier estimates of cumulative incidence at a vector of `times`.
##' @param eps tolerance for window (half width of window).  For `space='x'` is in data units, otherwise is the sample size for half the window.
##' @param varyeps applies to `space='n'` and causes a smaller `eps` to be used in strata with fewer than `` observations so as to arrive at three x points
##' @param xinc  increment in x to evaluate stats, default is xlim range/100 for `space='x'`.  For `space='n'` `xinc` defaults to m observations, where m = max(n/200, 1).
##' @param xlim 2-vector of limits to evaluate if `space='x'` (default is 10th to 10th)
##' @param times vector of times for evaluating one minus Kaplan-Meier estimates
##' @param tunits time units when `times` is given
##' @param msmooth set to `'smoothed'` or `'both'` to compute `lowess`-smooth moving estimates. `msmooth='both'` will display both.  `'raw'` will display only the moving statistics.  `msmooth='smoothed'` (the default) will display only he smoothed moving estimates.
##' @param tsmooth defaults to the super-smoother `'supsmu'` for after-moving smoothing.  Use `tsmooth='lowess'` to instead use `lowess`.
##' @param bass the `supsmu` `bass` parameter used to smooth the moving statistics if `tsmooth='supsmu'`
##' @param span the `lowess` `span` used to smooth the moving statistics
##' @param penalty passed to `hare`, default is to use BIC.  Specify 2 to use AIC.
##' @param maxdim passed to `hare`, default is 6
##' @param trans transformation to apply to x
##' @param itrans inverse transformation
##' @param loess set to TRUE to also compute loess estimates
##' @param ols   set to TRUE to include rcspline estimate of mean using ols
##' @param qreg  set to TRUE to include quantile regression estimates w rcspline
##' @param lrm   set to TRUE to include logistic regression estimates w rcspline
##' @param orm   set to TRUE to include ordinal logistic regression estimates w rcspline (mean + quantiles in `tau`)
##' @param hare  set to TRUE to include hazard regression estimtes of incidence at `times`, using the `polspline` package
##' @param family link function for ordinal regression (see `rms::orm`)
##' @param k     number of knots to use for ols and/or qreg rcspline
##' @param tau   quantile numbers to estimate with quantile regression
##' @param melt  set to TRUE to melt data table and derive Type and Statistic
##' @param data: data.table or data.frame, default is calling frame
##' @return a data table, with attribute `infon` which is a data frame with rows corresponding to strata and columns `Nmean`, `Nmin`, `Nmax` if `stat` computed `N`.  These summarize the number of observations used in the windows.  If `varyeps=TRUE` there is an additional column `eps` with the computed per-stratum `eps`.  An additional attribute `info` is a ready-to-use character string version of `infon`.  For `ggplot2` use for example `labs(caption=attr(result, 'info')) + theme(plot.caption=element_text(family='mono', size=7))`.
##' 
movStats <- function(formula, stat=NULL, space=c('n', 'x'),
                     eps =if(space=='n') 75, varyeps=FALSE,
                     xinc=NULL, xlim=NULL,
                     times=NULL, tunits='year',
                     msmooth=c('smoothed', 'raw', 'both'),
                     tsmooth=c('supsmu', 'lowess'),
                     bass=2, span=1/4, maxdim=6, penalty=NULL,
                     trans=function(x) x, itrans=function(x) x,
                     loess=FALSE,
                     ols=FALSE, qreg=FALSE, lrm=FALSE,
                     orm=FALSE, hare=FALSE, family='logistic',
                     k=5, tau=(1:3)/4, melt=FALSE,
                     data=environment(formula)) {
  require(data.table)
  if(ols || qreg || lrm) require(rms)
  space   <- match.arg(space)
  msmooth <- match.arg(msmooth)
  tsmooth <- match.arg(tsmooth)

  .knots. <<- k   # make a global copy

  mf   <- model.frame(formula, data=data)
  v    <- names(mf)
  Y    <- mf[[1]]
  sec  <- NCOL(Y) == 2
  if(sec && ! length(times))
    stop('when dependent variable has two columns you must specify times')
  if(sec && (loess || ols || qreg || lrm || orm))
    stop('loess, ols, qreg, lrm, orm do not apply when dependent variable has two columns')
  if(varyeps && space == 'x')
    stop('varyeps applies only to space="n"')
  
  if(sec) {
    require(survival)
    if(hare) require(polspline)
    Y2 <- Y[, 2]
    Y  <- Y[, 1]
    } else Y2 <- rep(1, nrow(mf))

  X  <- trans(mf[[2]])
  bythere <- length(v) > 2
  By <- if(bythere) mf[[3]] else rep(1, length(X))
  i  <- is.na(X) | is.na(Y) | is.na(Y2) | is.na(By)
  if(any(i)) {
    i  <- ! i
    X  <- X [i]
    Y  <- Y [i]
    Y2 <- Y2[i]
    By <- By[i]
  }

  ybin <- ! sec && all(Y %in% 0:1)

  qformat <- function(x)
    fcase(x == 0.05, 'P5', x == 0.1, 'P10',
          x == 0.25, 'Q1', x == 0.5, 'Median', x == 0.75, 'Q3',
          x == 0.9, 'P90', x == 0.95, 'P95',
          default=as.character(x))


  if(! length(stat))
    stat <- if(ybin) function(y) list('Moving Proportion' = mean(y),
                                      N = length(y))
            else if(sec)
              function(y, y2) {
                # km.quick is in Hmisc
                z <- c(1. - km.quick(Surv(y, y2), times), length(y))
                names(z) <- c(paste0('Moving ', times, '-', tunits), 'N')
                as.list(z)
              }
             else 
              function(y) {
                if(! length(y)) return(list(Mean=NA, Median=NA, Q1=NA, Q3=NA))
                qu <- quantile(y, (1:3)/4)
                list('Moving Mean'   = mean(y),
                     'Moving Median' = qu[2],
                     'Moving Q1'     = qu[1],
                     'Moving Q3'     = qu[3],
                     N=length(y))
              }

  statx <- function(y, y2, x) {
    s <- if(sec) stat(y, y2) else stat(y)
    if(space == 'n') s$.xmean. <- mean(x)
    s
  }

  R    <- NULL
  Xinc <- xinc

  uby  <- if(is.factor(By)) levels(By) else sort(unique(By))
  info <- matrix(NA, nrow=length(uby), ncol=4,
                 dimnames=list(uby, c('Nmean', 'Nmin', 'Nmax', 'eps')))
  
  for(by in uby) {
    j <- By == by
    if(sum(j) < 10) {
      warning(paste('Stratum', by, 'has < 10 observations and is ignored'))
      next
    }
    x  <- X [j]
    y  <- Y [j]
    y2 <- Y2[j]
    n  <- length(x)

    switch(space,
           x = {
             xl <- xlim
             if(! length(xl)) {
               xs <- sort(x)
               xl <- c(xs[10], xs[n - 10 + 1])
               if(diff(xl) >= 25) xl <- round(xl)
             }
             xinc <- Xinc
             if(! length(xinc)) xinc <- diff(xl) / 100.
             xseq <- seq(xl[1], xl[2], by=xinc)
             xv   <- x
           },
           n = {
             i    <- order(x)
             x    <- x [i]
             y    <- y [i]
             y2   <- y2[i]
             xinc <- Xinc
             if(! length(xinc)) xinc <- max(floor(n / 200.), 1)
             xseq <- seq(10, n - 10 + 1, by=xinc)
             xv   <- 1 : n
             } )

    ep <- eps
    if(space == 'n' && varyeps) {
      ## Requirement to have >= 3 x-points; new eps=h
      ## First estimate at observation min(xseq), last at max(xseq)
      ## First window goes to min(xseq) + h on right
      ## Last  window goes to max(xseq) - h on left
      ## Need difference in these two to be >= 2*xinc
      lowesteps <- floor((max(xseq) - min(xseq) - 2 * xinc) / 2.)
      if(lowesteps < eps) ep <- lowesteps
      info[by, 'eps'] <- ep
      }
    
    s <- data.table(x, y, y2, xv)
    a <- data.table(tx=xseq, key='tx')     # target xs for estimation
    a[, .q(lo, hi) := .(tx - ep, tx + ep)] # define all windows
    m <- a[s, on=.(lo <= xv, hi >= xv)]    # non-equi join
    setkey(m, tx)

    ## Non-equi join adds observations tx=NA
    m <- m[! is.na(tx), ]
    w <- m[, statx(y, y2, x), by=tx]
    if('N' %in% names(w)) {
      N <- w[, N]
      info[by, 1:3] <- c(round(mean(N), 1), min(N), max(N))
      }

    if(space == 'n') {
      w[, tx      := .xmean.]
      w[, .xmean. := NULL   ]
      }

    if(msmooth != 'raw') {
      computed <- setdiff(names(w), c('tx', 'N'))
      for(vv in computed) {
        smoothed <- switch(tsmooth,
                           lowess = lowess(w[, tx], w[[vv]], f   =span),
                           supsmu = supsmu(w[, tx], w[[vv]], bass=bass))
        if(length(smoothed$x) < 2)
          stop(paste0('Only 1 x point for stratum ', by,
                      ' with ', n,
                      ' observations. Consider specifying varyeps=TRUE.'))
        smfun <- function(x) approx(smoothed, xout=x)$y
        switch(msmooth,
               smoothed = {
                 w[, (vv) := smfun(tx)]
               },
               both = {
                 newname <- sub('Moving', 'Moving-smoothed', vv)
                 w[, (newname) := smfun(tx)]
               }
               )
        }
    }

    ## Also compute loess estimates
    dat <- data.frame(x=w[, tx])
    if(loess) {
      np <- loess(y ~ x, data=s)
      pc <- predict(np, dat)
      if(ybin)
        w[, 'Loess Proportion' := pc]
      else
        w[, `Loess Mean`       := pc]
    }

    if(ols) {
      f <- ols(y ~ rcs(x, .knots.), data=s)
      pc <- predict(f, dat)
      w[, `OLS Mean` := pc]
    }

    if(lrm) {
      f <- lrm(y ~ rcs(x, .knots.), data=s)
      pc <- predict(f, dat, type='fitted')
      w[, 'LR Proportion' := pc]
    }

    if(orm) {
      f <- orm(y ~ rcs(x, .knots.), data=s, family=family)
      pc <- predict(f, dat, type='mean')
      w[, 'ORM Mean' := pc]
      if(length(tau)) {
        pc <- predict(f, dat)
        qu <- Quantile(f)
        for(ta in tau) {
          w[, ormqest := qu(ta, pc)]
          cta <- qformat(ta)
          setnames(w, 'ormqest', paste('ORM', cta))
        }
      }
    }

    if(qreg)
      for(ta in tau) {
        f  <- Rq(y ~ rcs(x, .knots.), tau=ta, data=s)
        pc <- predict(f, dat)
        w[, qrest := pc]
        cta <- qformat(ta)
        setnames(w, 'qrest', paste('QR', cta))
      }

    if(hare) {
      f <- if(length(penalty))
             hare(y, y2, x, maxdim=maxdim, penalty=penalty)
           else
             hare(y, y2, x, maxdim=maxdim)
      for(ti in times) {
        inc <- phare(ti, dat$x, f)
        newname <- paste0('HARE ', ti, '-', tunits)
        w[, (newname) := inc]
      }
    }

    w[, by := by]
    R <- rbind(R, w)
  }

  R[, tx := itrans(tx)]
  if(bythere) setnames(R, c('tx', 'by'), v[2:3])
  else {
    R[, by := NULL]
    setnames(R, 'tx', v[2])
  }
  if(melt) {
    if(sec) v[1] <- 'incidence'
    # Exclude N if present or would mess up melt
    if('N' %in% names(R)) R[, N := NULL]
    R <- melt(R, id.vars=v[-1], variable.name='Statistic',
              value.name=v[1])
    if(sec) {
      addlab <- function(x) {
        label(x) <- 'Cumulative Incidence'
        x
      }
      R[, incidence := addlab(incidence)]
    }
    
    R[, Type      := sub (' .*', '', Statistic)]
    R[, Statistic := sub ('.* ', '', Statistic)]
    R[, Type      := gsub('~', ' ',  Type)]
    R[, Statistic := gsub('~', ' ',  Statistic)]
  }

  if(all(is.na(info[, 'eps']))) info <- info[, -4]
  if(! bythere) row.names(info) <- NULL
  infoc <- paste(capture.output(print(info)), collapse='\n')
  attr(R, 'infon') <- info
  attr(R, 'info')  <- infoc
  
  R
  }
