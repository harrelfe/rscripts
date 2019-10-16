# By Yuqi Tian, Dept. of Biostatistics, Vanderbilt University <yuqi.tian@vanderbilt.edu>
#
# The first half includes functions to calculate conditional mean/median/cdf
# and their confidence intervals. The second half is the simulation.
#
# The file includes the simulation code for the primary setting in "An Empirical Comparison of Two Novel Transformation Models". Here we show the code to get conditional mean/quantiles/cdfs, corresponding confidence intervals and out-of-sample log-likelihood of the CPM and the MLT.

### Simulation code for the primary setting simulation ###

library(rms)
library(mlt)
library(tram)
library(multcomp)

#### orm function ####
### Mean
mean.orm <- function(mod, new.data, se=TRUE){
  if(is.null(mod$yunique)) {
    stop("Need to set x=TRUE and y=TRUE for orm") 
  } else{
    order.y <- mod$yunique
    n.alpha <- length(order.y)-1
    xb <- as.matrix(new.data)%*%matrix(coef(mod)[colnames(new.data)])
    m.alpha <- mod$coef[1:n.alpha]
    lb <- t(outer(m.alpha, xb, "+")[,,1])
    m.s <- mod$trans$cumprob(lb)
    m.f <- t(apply(m.s, 1, FUN=function(x) c(1,x[1:n.alpha]) - c(x[1:n.alpha], 0)))
    m.mean <- apply(m.f, 1, FUN=function(x) sum(x*order.y))
    
    if(se){
      if(mod$family=="logistic") mod$trans$deriv <- function(x) exp(-x)/(1+exp(-x))^2
      if(mod$family=='probit') mod$trans$deriv <- function(x) dnorm(x)
      if(mod$family=='loglog') mod$trans$deriv <- function(x) exp(-x - exp(-x))
      if(mod$family=='cloglog') mod$trans$deriv <- function(x) exp(x - exp(x))
      
      dmean.dalpha <- t(apply(mod$trans$deriv(lb), 
                              1, FUN=function(x) x*(order.y[2:length(order.y)] - order.y[1:n.alpha])))
      dmean.dbeta <- apply(dmean.dalpha, 1, sum)*as.matrix(new.data)
      dmean.dtheta <- cbind(dmean.dalpha, dmean.dbeta)   
      mean.var <-diag(dmean.dtheta%*%solve(mod$info.matrix)%*%t(dmean.dtheta))
      mean.se <- sqrt(mean.var)   
      result <- cbind(m.mean, mean.se)
      ci <- t(apply(result, 1, FUN=function(x) c(x[1]- qnorm(0.975)*x[2],
                                                 x[1]+ qnorm(0.975)*x[2])))
      result <- cbind(result, ci)
      colnames(result) <- c("est", "se", "lb", "ub")
    } else{
      result <- matrix(m.mean)
      colnames(result) <- c("est")
    }
    
    
    return(result)
    
    
  } 
  
}

### CDF
cdf.orm <- function(mod, new.data, at.y=0,se=TRUE){
  if(is.null(mod$yunique)) {
    stop("Need to set x=TRUE and y=TRUE for orm") 
  } else{
    order.y <- mod$yunique
    xb <- as.matrix(new.data)%*%matrix(coef(mod)[colnames(new.data)])
    
    index <- sapply(at.y, FUN=function(x) {if(x<min(order.y)[1]) result <- Inf 
    else if (x==min(order.y)[1]) result <- 1
    else if(x >= max(order.y)[1]) result <- -Inf
    else which(order.y>=x)[1]-1})
    
    m.alpha <- mod$coef[index]
    m.alpha <- ifelse(is.infinite(index), index, m.alpha)
    if(length(at.y)==1){
      lb <- as.matrix(outer(m.alpha, xb, "+")[,,1])
    } else lb <- t(outer(m.alpha, xb, "+")[,,1])
    m.cdf <- 1- mod$trans$cumprob(lb)
    
    
    if(se){
      if(mod$family=="logistic") mod$trans$deriv <- function(x) exp(-x)/(1+exp(-x))^2
      if(mod$family=='probit') mod$trans$deriv <- function(x) dnorm(x)
      if(mod$family=='loglog') mod$trans$deriv <- function(x) exp(-x - exp(-x))
      if(mod$family=='cloglog') mod$trans$deriv <- function(x) exp(x - exp(x))
      
      cdf.se <- matrix(NA, ncol=length(at.y), nrow=dim(new.data)[1])
      lb.se <- matrix(NA, ncol=length(at.y), nrow=dim(new.data)[1])
      
      var <- as.matrix(solve(mod$info.matrix))
      
      for(i in 1:length(at.y)) {
        
        var.i <- var[c(index[i], which(names(coef(mod)) %in% colnames(new.data))), 
                     c(index[i], which(names(coef(mod)) %in% colnames(new.data)))]
        dcdf.dtheta <- cbind(-mod$trans$deriv(lb[,i]),  
                             -mod$trans$deriv(lb[,i])*as.matrix(new.data) )
        dlb.dtheta <- as.matrix(cbind(1, new.data))
        cdf.se[,i] <- sqrt(diag(dcdf.dtheta %*% var.i%*% t(dcdf.dtheta)))
        lb.se[, i] <- sqrt(diag(dlb.dtheta%*%var.i%*% t(dlb.dtheta)))
        
      }
      ci.lb <- sapply(1:length(at.y), FUN=function(i) 
      { 1- mod$trans$cumprob(lb[, i] +qnorm(0.975)*lb.se[, i])})
      ci.ub <- sapply(1:length(at.y), FUN=function(i) 
      { 1- mod$trans$cumprob(lb[, i] -qnorm(0.975)*lb.se[, i])})
      
      
      result <- list(est=m.cdf,
                     se=cdf.se,
                     lb=ci.lb,
                     ub=ci.ub)
    } else{
      result <- list(est=m.cdf)
    }
    
    
    return(result)
    
  } 
  
}


### Quantile
quantile.orm <- function(mod, new.data, probs=0.5, se=TRUE){
  
  quantile <- matrix(NA, nrow=dim(new.data)[1], ncol=length(probs))
  order.y <- mod$yunique
  #n.alpha <- length(order.y)-1
  xb <- as.matrix(new.data)%*%matrix(coef(mod)[colnames(new.data)])
  alpha <- mod$coef[1:(length(unique(order.y))-1)]
  lb <- t(outer(alpha, xb, "+")[,,1])
  m.cdf <- 1- mod$trans$cumprob(lb)
  m.cdf <- cbind(0, m.cdf, 1)
  for(i in 1: length(probs)){
    try({
      index.1 <- apply(m.cdf, 1, FUN=function(x){ max(which(x<=probs[i]))[1]} )
      index.2 <- apply(m.cdf, 1, FUN=function(x){ min(which(x>=probs[i]))[1]} )
      
      index.y1 <- ifelse(index.1>length(order.y), Inf, order.y[index.1])
      index.y2 <- ifelse(index.2>length(order.y),Inf,order.y[index.2])
      
      index.y1.cdf <- ifelse(index.1==0, 0, m.cdf[cbind(1:dim(new.data)[1], index.1)])
      
      index.y2.cdf <- ifelse(index.2>length(order.y), 1, m.cdf[cbind(1:dim(new.data)[1], index.2)])
      
      
      
      
      quantile[,i] <- ifelse(index.1==index.2, index.y1, 
                             (index.y2-index.y1)/(index.y2.cdf - index.y1.cdf)*
                               (probs[i]-index.y1.cdf) + index.y1) 
      quantile[, i] <- ifelse(is.na(quantile[,i]), max(order.y), quantile[, i])
    })
    
  }
  result <- quantile
  
  if(se){
    if(mod$family=="logistic") mod$trans$deriv <- function(x) exp(-x)/(1+exp(-x))^2
    if(mod$family=='probit') mod$trans$deriv <- function(x) dnorm(x)
    if(mod$family=='loglog') mod$trans$deriv <- function(x) exp(-x - exp(-x))
    if(mod$family=='cloglog') mod$trans$deriv <- function(x) exp(x - exp(x))
    
    quantile.lb <- quantile.ub <- matrix(NA, nrow=dim(new.data)[1], ncol=length(probs))
    lb.se <- matrix(NA, ncol=dim(lb)[2], nrow=dim(new.data)[1])
    var <- as.matrix(solve(mod$info.matrix))
    
    for(i in 1:dim(lb)[2]){
      var.i <- var[c(i, which(names(coef(mod)) %in% colnames(new.data))), 
                   c(i, which(names(coef(mod)) %in% colnames(new.data)))]
      
      dcdf.dtheta <- cbind(-mod$trans$deriv(lb[,i]),  
                           -mod$trans$deriv(lb[,i])*as.matrix(new.data) )
      dlb.dtheta <- as.matrix(cbind(1, new.data))
      lb.se[,i] <- sqrt(diag(dlb.dtheta%*%var.i%*% t(dlb.dtheta)))
    }
    
    ci.lb <- sapply(1:dim(lb)[2], FUN=function(i) 
    { 1- mod$trans$cumprob(lb[, i] +qnorm(0.975)*lb.se[, i])})
    ci.ub <- sapply(1:dim(lb)[2], FUN=function(i) 
    { 1- mod$trans$cumprob(lb[, i] -qnorm(0.975)*lb.se[, i])})
    ci.lb <- matrix(ci.lb, nrow=dim(new.data)[1])
    ci.ub <- matrix(ci.ub, nrow=dim(new.data)[1])
    
    ci.lb <- cbind(0, ci.lb, 1)
    ci.ub <- cbind(0, ci.ub, 1)
    
    for(i in 1: length(probs)){
      try({
        index.1 <- apply(ci.lb, 1, FUN=function(x){ max(which(x<=probs[i]))[1]} )
        index.2 <- apply(ci.lb, 1, FUN=function(x){ min(which(x>=probs[i]))[1]} )
        
        index.y1 <- ifelse(index.1>length(order.y), Inf, order.y[index.1])
        index.y2 <- ifelse(index.2>length(order.y),Inf,order.y[index.2])
        
        index.y1.cdf <- ifelse(index.1==0, 0, ci.lb[cbind(1:dim(new.data)[1], index.1)])
        
        index.y2.cdf <- ifelse(index.2>length(order.y), 1, ci.lb[cbind(1:dim(new.data)[1], index.2)])
        
        
        quantile.lb[,i] <- ifelse(index.1==index.2, index.y1, 
                                  (index.y2-index.y1)/(index.y2.cdf - index.y1.cdf)*(probs[i]-index.y1.cdf) + index.y1) 
        quantile.lb[, i] <- ifelse(is.infinite(quantile.lb[,i]), max(order.y), quantile.lb[, i])
        
        index.1 <- apply(ci.ub, 1, FUN=function(x){ max(which(x<=probs[i]))[1]} )
        index.2 <- apply(ci.ub, 1, FUN=function(x){ min(which(x>=probs[i]))[1]} )
        
        index.y1 <- ifelse(index.1>length(order.y), Inf, order.y[index.1])
        index.y2 <- ifelse(index.2>length(order.y),Inf,order.y[index.2])
        
        index.y1.cdf <- ifelse(index.1==0, 0, ci.ub[cbind(1:dim(new.data)[1], index.1)])
        
        index.y2.cdf <- ifelse(index.2>length(order.y), 1, ci.ub[cbind(1:dim(new.data)[1], index.2)])
        
        
        quantile.ub[,i] <- ifelse(index.1==index.2, index.y1, 
                                  (index.y2-index.y1)/(index.y2.cdf - index.y1.cdf)*(probs[i]-index.y1.cdf) + index.y1) 
        quantile.ub[, i] <- ifelse(is.na(quantile.ub[,i]), max(order.y), quantile.ub[, i])
        
        
      })
      
    }
    
    result <- list(quantile=quantile,
                   lb=quantile.ub,
                   ub=quantile.lb)
  }
  return(result)
}


#### mlt Functions ####
### Mean
mean.mlt <- function(mlt_y,mlt_y_mean,data,newdata){
  result <- as.data.frame(matrix(0,nrow=dim(newdata)[1],ncol=4))
  colnames(result) <- c('mean','se','lb','ub')
  
  ##The basis defined makes sure all cutpoints can be standardized into [0,1]
  q <- mkgrid(mlt_y, n = 1001)[['y']]
  trans <- mlt_y$todistr$d
  
  d <- predict(mlt_y, newdata = newdata, type = "density", q = q)
  result[,'mean'] <- sapply(1:ncol(d), function(i) 
    integrate(function(x) x * approxfun(q, d[,i])(x), 
              lower = min(q), upper = max(q))$value)
  
  ##se
  cutpoints <- mkgrid(mlt_y_mean, n = 1001)[['y']]
  basis <- mlt_y_mean$model$bases[[1]]
  xb <- as.matrix(newdata)%*%matrix(coef(mlt_y_mean)[colnames(newdata)])
  n.v <- mlt_y_mean$df-dim(newdata)[2] #M+1
  v <- coef(mlt_y_mean)[1:n.v]
  ay <- basis(cutpoints)
  hy <- ay%*%v
  lb <- outer(-xb,hy,'+')[,1,,]
  
  dm.s <- t(trans(lb)) #F'=f
  dm.f <- t(apply(dm.s, 2, FUN=function(x) c(x[1:length(cutpoints)])-c(0,x[1:length(cutpoints)-1])))
  dmean.dbeta <- -t(dm.f%*%cutpoints)*newdata
  
  diff_cutpoints <- cutpoints-c(cutpoints[2:length(cutpoints)],0)
  dmean.dv <- t(apply(dm.s,2,function(x){
    diff_cutpoints%*%(x*ay)
  }))
  
  dmean.dtheta <- as.matrix(cbind(dmean.dv, dmean.dbeta)) 
  mean.var <-diag(dmean.dtheta%*%vcov(mlt_y_mean)%*%t(dmean.dtheta))
  result[,'se'] <- sqrt(mean.var)
  result[,'lb'] <- result[,'mean']-qnorm(0.975)*result[,'se']
  result[,'ub'] <- result[,'mean']+qnorm(0.975)*result[,'se']
  
  return(result)
}


### CDF
cdf.mlt <- function(object, newdata, q = 5) {
  
  ### evaluate cumulative distribution function
  cdf <- predict(object, newdata = newdata, q = q, type = "distribution")
  ### compute confidence intervals on this scale for 1000 values
  ### from the response sample space
  cb <- confband(object, newdata = newdata, K = 1000, type = "distribution", 
                 calpha = univariate_calpha())
  
  ### interpolate to obtain confidence intervals at q values
  ci <- lapply(1:length(cb), function(i) {
    ret <- cbind(cdf[,i], 
                 approxfun(cb[[i]][,"q"], cb[[i]][,"lwr"])(q),
                 approxfun(cb[[i]][,"q"], cb[[i]][,"upr"])(q))
    colnames(ret) <- c("Estimate", "lwr", "upr")
    ret
  })
  
  return(ci)
}

### Quantile
quantile.mlt <- function(object, newdata, prob = c(.1, .5, .8)) {
  
  ### evaluate quantile function (internally via 
  ### number inversion of cdf)
  qf <- predict(object, newdata = newdata, 
                type = "quantile", K = 1000, prob = prob)
  
  ### compute confidence intervals
  cb <- confband(object, newdata = newdata, K = 1000, type = "distribution", 
                 calpha = univariate_calpha())
  
  ### interpolate to obtain confidence intervals at prob values
  ci <- lapply(1:length(cb), function(i) {
    f <- function(q, p, what = "lwr") 
      approxfun(cb[[i]][,"q"], cb[[i]][,what])(q) - p
    
    ret <- cbind(qf[,i], 
                 sapply(prob, function(p) {
                   tryCatch(uniroot(f, p = p, what = "upr", 
                                    interval = range(cb[[1]][,"q"]))$root,
                            error=function(e) min(cb[[1]][,"q"]))
                 }),
                 sapply(prob, function(p) {
                   tryCatch(uniroot(f, p = p, what = "lwr", 
                                    interval = range(cb[[1]][,"q"]))$root,
                            error=function(e) max(cb[[1]][,"q"]))
                 }))
    colnames(ret) <- c("Estimate", "lwr", "upr")
    ret
  })
  
  return(ci)
}




# generate data
n <- 1000
x <- rbinom(n,1,p=0.5)
e <- rnorm(n,0,1)
y <- qchisq(pnorm(x+e),df=5)
data <- data.frame('x'=x,'y'=y)
newdata <- data.frame('x'=c(0,1))

###### CPM ######
model_orm <- orm(y~x, data=data, family=probit) 
# conditional mean 
mean_orm <- mean.orm(model_orm, new.data = newdata, se=T)
# conditional median
median_orm <- quantile.orm(model_orm, new.data = newdata, se=T, probs=0.5)
# conditional CDF at 5
cdf_orm <- cdf.orm(model_orm, new.data = newdata, se=T, at.y=5)

###### MLT ######
model_mlt <- as.mlt(BoxCox(y ~ x, data, order = 10, 
                           support = quantile(data$y, prob = c(.1, .9)), 
                           add = c(-15, 15),
                           extrapolate = TRUE))
# conditional mean 
# (to get basis at every data point mkgrid generates)
model_mlt_mean <- as.mlt(BoxCox(y ~ x, data, order = 10, 
                                support = c(min(data$y),max(data$y)), 
                                bounds=c(-Inf,Inf))) 

mean_mlt <- mean.mlt(model_mlt, model_mlt_mean, data, newdata)
# conditional median
median_mlt <- quantile.mlt(model_mlt, newdata, 0.5)
# conditional CDF at 5
cdf_mlt <- cdf.mlt(model_mlt, newdata, 5)


##### Out-of-sample Log-likelihood #####
# generate test sample
x <- rbinom(n,1,p=0.5)
e <- rnorm(n,0,1)
y <- qchisq(pnorm(x + e), df=5)
test <- data.frame('x'=x,'y'=y)
# get breaks
brk <- model_orm$yunique
### -/+15 for values not in range(o$yunique)
brk <- c(brk[1] - 15, brk, rev(brk)[1] + 15)
test$yc <- unclass(cut(test$y, breaks = brk))

tmp1 <- tmp2 <- test
tmp1$y <- brk[tmp1$yc]
tmp2$y <- brk[tmp2$yc + 1]

# MLT: out-of-sample log-likelihood
p1 <- predict(object=model_mlt, newdata=tmp1, type = "distribution")
p2 <- predict(object=model_mlt, newdata=tmp2, type = "distribution")
loglikelihood_mlt <- sum(log(p2 - p1))

# CPM: out-of-sample log-likelihood
test$yc <- unclass(cut(test$y, breaks = brk, right=F)) - 1
test$yc[test$yc == 0] = 1
p <- predict(model_orm, newdata = test, type = "fitted")
### first cut-off is missing
### add one and zero probabilities to survivor fct
p <- cbind(1, p, 0)
p1 <- p[cbind(1:nrow(test), test$yc + 1)]
p2 <- p[cbind(1:nrow(test), test$yc)]
### log-likelihood of sum of log-probabilities
loglikelihood_orm <- sum(log(p2 - p1))

