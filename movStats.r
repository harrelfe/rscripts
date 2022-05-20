##' Moving Estimates Using Overlapping Fixed-width Windows
##' 
##' Function to compute moving averages and other statistics as a function
##' of a continuous variable, possibly stratified by another variable
##' Estimates are made by creating overlapping moving windows and
##' computing the statistics defined in the stat function for each window.
##' By default estimates are made at the 10th smallest to the 10th largest
##' observed values of the x variable to avoid extrapolation and to
##' help getting the moving statistics off on an adequate start for
##' the left tail.  Also by default the moving estimates are smoothed using `lowess`.
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
##' @param stat function of one argument that returns a named list of computed values.  Defaults to computing mean and quartiles + N except when y is binary in which case it computes moving proportions
##' @param eps tolerance for window (half width of window)
##' @param xlim 2-vector of limits to evaluate (default is 10th to 10th)
##' @param xinc  increment in x to evaluate stats, default is xlim range/100
##' @param msmooth set to `'smoothed'` or `'both'` to compute `lowess`-smooth moving estimates. `msmooth='both'` will display both.  `'raw'` will display only the moving statistics.  `msmooth='smoothed'` (the default) will display only he smoothed moving estimates.
##' @param trans transformation to apply to x
##' @param itrans inverse transformation
##' @param loess set to TRUE to also compute loess estimates
##' @param ols   set to TRUE to include rcspline estimate of mean using ols
##' @param qreg  set to TRUE to include quantile regression estimates w rcspline
##' @param lrm   set to TRUE to include logistic regression estimates w rcspline
##' @param orm   set to TRUE to include ordinal logistic regression estimates w rcspline (mean + quantiles in `tau`)
##' @param family link function for ordinal regression (see `rms::orm`)
##' @param k     number of knots to use for ols and/or qreg rcspline
##' @param tau   quantile numbers to estimate with quantile regression
##' @param melt  set to TRUE to melt data table and derive Type and Statistic
##' @param data: data.table or data.frame, default is calling frame
##' 
movStats <- function(formula, stat=NULL, eps, xlim=NULL, xinc=NULL,
                     msmooth=c('smoothed', 'raw', 'both'),
                     trans=function(x) x, itrans=function(x) x,
                     loess=FALSE,
                     ols=FALSE, qreg=FALSE, lrm=FALSE,
                     orm=FALSE, family='logistic',
                     k=5, tau=(1:3)/4, melt=FALSE,
                     data=environment(formula)) {
  require(data.table)
  if(ols || qreg || lrm) require(rms)
  msmooth <- match.arg(msmooth)

  .knots. <<- k   # make a global copy

  v  <- all.vars(formula)
  if(any(v %nin% names(data)))
     stop('formula has a variable not in data')
  Y  <- data[[v[1]]]
  X  <- trans(data[[v[2]]])
  bythere <- length(v) > 2
  By <- if(bythere) data[[v[3]]] else rep(1, length(X))
  i  <- is.na(X) | is.na(Y) | is.na(By)
  if(any(i)) {
    i  <- ! i
    X  <- X[i]
    Y  <- Y[i]
    By <- By[i]
  }

  ybin <- all(Y %in% 0:1)

  qformat <- function(x)
    fcase(x == 0.05, 'P5', x == 0.1, 'P10',
          x == 0.25, 'Q1', x == 0.5, 'Median', x == 0.75, 'Q3',
          x == 0.9, 'P90', x == 0.95, 'P95',
          default=as.character(x))


  if(! length(stat))
    stat <- if(ybin) function(y) list('Moving Proportion' = mean(y))
            else
              function(y) {
                if(! length(y)) return(list(Mean=NA, Median=NA, Q1=NA, Q3=NA))
                qu <- quantile(y, (1:3)/4)
                list('Moving Mean'   = mean(y),
                     'Moving Median' = qu[2],
                     'Moving Q1'     = qu[1],
                     'Moving Q3'=qu[3],
                     N=length(y))
              }

  R    <- NULL
  Xinc <- xinc
  for(by in sort(unique(By))) {
    j <- By == by
    if(sum(j) < 10) {
      warning(paste('Stratum', by, 'has < 10 observations and is ignored'))
      next
    }
    x <- X[j]
    y <- Y[j]
    n <- length(x)

    xl <- xlim
    if(! length(xl)) {
      xs <- sort(x)
      xl <- c(xs[10], xs[n - 10 + 1])
      if(diff(xl) >= 25) xl <- round(xl)
    }
    xinc <- Xinc
    if(! length(xinc)) xinc <- diff(xl) / 100.
    xseq <- seq(xl[1], xl[2], by=xinc)

    s <- data.table(x, y)
    a <- data.table(tx=xseq, key='tx')       # target xs for estimation
    a[, .q(lo, hi) := .(tx - eps, tx + eps)] # define all windows
    m <- a[s, on=.(lo <= x, hi >= x)]        # non-equi join
    setkey(m, tx)

    ## Non-equi join adds observations tx=NA
    m <- m[! is.na(tx), ]

    w <- m[, stat(y), by=tx]
    if(msmooth != 'raw') {
      computed <- setdiff(names(w), c('tx', 'N'))
      for(vv in computed) {
        smoothed <- lowess(w[, tx], w[[vv]])
        smfun <- function(x) approx(smoothed, xout=x)$y
        switch(msmooth,
               smoothed = {
                 w[, (vv) := smfun(tx)]
               },
               both = {
                 newname <- paste0('Smoothed/', vv)
                 w[, (newname) := smfun(tx)]
               }
               )
        }
    }

    ## Also compute loess estimates
    dat <- data.frame(x=xseq)
    if(loess) {
      np <- loess(y ~ x, data=s)
      pc <- predict(np, dat)
      if(ybin)
        w[, 'Loess Proportion' := pc]
      else
        w[, `Loess Mean` := pc]
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
    # Exclude N if present or would mess up melt
    if('N' %in% names(R)) R[, N := NULL]
    R <- melt(R, id.vars=v[2], variable.name='Statistic',
              value.name=v[1])
    R[, Type      := sub(' .*', '', Statistic)]
    R[, Statistic := sub('.* ', '', Statistic)]
    }
  R
  }
