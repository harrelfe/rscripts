princmp <- function(x, k=min(5, p), data, cor=TRUE, offset=0.8, col=1,
                    adj=0, orig=TRUE, pl=TRUE) {
  g  <- princomp(x, cor=cor, data=data)
  co <- unclass(g$loadings)
  p  <- ncol(co)
  cat('\nPC Coefficients of Standardized Variables\n')
  print(round(co[, 1 : k], 4))
  p <- ncol(co)
  if(orig) {
    sds <- g$scale
    cat('\nPC Coefficients of Original Variables\n')
    co <- co / matrix(rep(sds, p), nrow=length(sds))
    print(round(co[, 1 : k], 5))
    }

  if(pl) {
    plot(g, type='lines', main='', npcs=k)
    vars <- g$sdev^2
    cumv <- cumsum(vars)/sum(vars)
    p <- length(cumv)
    text(1:k, vars[1:k] + offset*par('cxy')[2],
         as.character(round(cumv[1:k], 2)),
         srt=45, adj=adj, cex=.65, xpd=NA, col=col)
    }
 
  # Use predict method with newdata to ensure that PCs NA when
  # orginal variables NA
  invisible(predict(g, newdata=data)[, 1 : k])
}


