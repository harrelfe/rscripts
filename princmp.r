princmp <- function(formula, data=environment(formula),
                    method=c('regular', 'sparse'),
                    k=min(5, p), cor=TRUE, offset=0.8, col=1,
                    adj=0, orig=TRUE, pl=TRUE, sw=FALSE) {
  method <- match.arg(method)
  if(method == 'sparse')
    if(! require(pcaPP))
      stop('You must install the pcaPP package')
  
  X  <- model.matrix.lm(formula, data)
  cat('Used', nrow(X), 'observations with no NAs out of', nrow(data), '\n')
  X  <- X[, -1]  # remove intercept
  g  <- switch(method,
               regular = princomp(X, cor=cor),
               sparse  = sPCAgrid(X, k=k, method='sd',
                                  center=mean,
                                  scale=if(cor) sd else function(x) 1.,
                                  scores=TRUE, maxiter=10) )
  
  co <- unclass(g$loadings)
  p  <- ncol(co)
  prz <- function(x, m) {
    x <- format(round(x, m), zero.print=FALSE)
    print(x, quote=FALSE)
  }
  type <- switch(method, regular='', sparse='Sparse ')
  cat('\n', type, 'PC Coefficients of Standardized Variables\n', sep='')
  prz(co[, 1 : k], 3)
  p <- ncol(co)
  if(orig) {
    sds <- g$scale
    cat('\n', type, 'PC Coefficients of Original Variables\n', sep='')
    co <- co / matrix(rep(sds, p), nrow=length(sds))
    prz(co[, 1 : k], 5)
    }

  if(pl) {
    plot(g, type='lines', main='', npcs=k)
    vars <- g$sdev^2
    cumv <- cumsum(vars) / sum(vars)
    p <- length(cumv)
    text(1:k, vars[1:k] + offset*par('cxy')[2],
         as.character(round(cumv[1:k], 2)),
         srt=45, adj=adj, cex=.65, xpd=NA, col=col)
  }

  if(sw) {
    if(! require(leaps)) stop('You must install the leaps package')
    for(j in 1 : k) {
      cat('\nStepwise Approximations to PC', j, '\n', sep='')
      fchar <- capture.output(
        f <- regsubsets(X, g$scores[, j], method='forward',
                        nbest=1, nvmax=max(1, p - 1)))
      s <- summary(f)
      w <- s$which[, -1, drop=FALSE]   # omit intercept
      print(t(ifelse(w, '*', ' ')), quote=FALSE)
      cat('R2:', round(s$rsq, 3), '\n')
    }
    }
    
  ## Use predict method with newdata to ensure that PCs NA when
  ## original variables NA
  ## See https://stackoverflow.com/questions/5616210
  X  <- model.matrix.lm(formula, data, na.action=na.pass)
  pcs <- predict(g, newdata=X)[, 1 : k]
  invisible(pcs)
}
