## Function to compute moving averages and other statistics as a function
## of a continuous variable, possibly stratified by another variable
## Estimates are made by creating overlapping moving windows and
## computing the statistics defined in the stat function for each window.
## By default estimates are made at the 10th smallest to the 10th largest
## observed values of the x variable to avoid extrapolation and to
## help getting the moving statistics off on an adequate start for
## the left tail.
## eps is the half-width of each interval
## When melt=TRUE you can feed the result into ggplot like this:
## ggplot(w, aes(x=age, y=crea, col=Type)) + geom_line() +
##   facet_wrap(~ Statistic)
##
## Usage:
##  movStat(formula, stat, eps, xlim, xinc, data=...)
##
## formula: a formula with the analysis variable on the left and the
##          x-variable on the right, following by an optional stratification
##          variable
## stat: function of one argument that returns a named list of
##       computed values.  Defaults to computing mean and quartiles + N
##       except when y is binary in which case it computes moving proportions
## eps:  tolerance for window (half width of window)
## xlim: 2-vector of limits to evaluate (default is 10th to 10th)
## xinc:  increment in x to evaluate stats, default is xlim range/100
## trans:transformation to apply to x
## itrans:inverse transformation
## loess:set to TRUE to also compute loess estimates
## ols  :set to TRUE to include rcspline estimate of mean using ols
## qreg :set to TRUE to include quantile regression estimates w rcspline
## lrm  :set to TRUE to include logistic regression estimates w rcspline
## k    :number of knots to use for ols and/or qreg rcspline
## tau  :quantile numbers to estimate with quantile regression
## melt :set to TRUE to melt data table and derive Type and Statistic
## data: data.table or data.frame, default is calling frame

movStats <- function(formula, stat=NULL, eps, xlim=NULL, xinc=NULL,
                     trans=function(x) x, itrans=function(x) x,
                     loess=FALSE, ols=FALSE, qreg=FALSE, lrm=FALSE,
                     k=5, tau=(1:3)/4, melt=FALSE,
                     data=environment(formula)) {
  require(data.table)
  if(ols || qreg || lrm) require(rms)

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

    if(qreg)
      for(ta in tau) {
        f  <- Rq(y ~ rcs(x, .knots.), tau=ta, data=s)
        pc <- predict(f, dat)
        w[, qrest := pc]
        cta <- fcase(ta == 0.25, 'Q1', ta == 0.5, 'Median',
                     ta == 0.75, 'Q3', default=as.character(ta))
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