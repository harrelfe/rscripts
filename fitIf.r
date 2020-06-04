##' fitIf
##'
##' By calling \code{fitIf(fitname <- fitting_function(...))} \code{fitIf} determines whether the fit object has already been created under the file named \code{"fitname.rds"} in the current working directory, and if so loads it.  It also looks for a file with a name such as \code{"fitname-rstan.rds"} and if it exists laods it and adds that object as the \code{rstan} part of the model fit, for \code{rstan} diagnostics.  If the \code{"fitname.rds"} does not exist, runs the model fit, strips off the large \code{rstan} object, stores the short fit object in \code{"fitname.rds"} and stores the full fit object in R object \code{fitname} in the global environment.  To force a re-fit, remove the \code{.rds} object.
##' @title fitIf
##' @param w an expression of the form \code{fitname <- fitting_function(...)}
##' @author Frank Harrell
fitIf <- function(w) {
  x <- as.character(substitute(w))
  fitname <- x[2]   # left hand side of assignment
  file    <- paste0(fitname, '.rds')
  filers  <- paste0(fitname, '-rstan', '.rds')
  if(file.exists(file)) {
    fit <- readRDS(file)
    if(file.exists(filers)) {
      rs <- readRDS(filers)
      fit$rstan <- rs
    }
    assign(fitname, fit, envir=.GlobalEnv)
    return(invisible())    # note that w was never evaluated
  }
  fit <- w   # evaluates w (runs the fit) and assigns in .GlobalEnv
  fitsmall <- fit
  fitsmall$rstan <- NULL
  saveRDS(fitsmall, file, compress='xz')
  invisible()
}

##' Load a Saved rms Object
##'
##' Adds a suffix of \code{.rds} to the name of the argument and calls \code{readRDS} to read the stored object, returning it to the user
##' @title fitLoad
##' @param fit R object name (unquoted) that was passed to \code{fitSave}
##' @return an object, using an rms package fit object
##' @author Frank Harrell
fitLoad <- function(fit) {
  file <- paste0(as.character(substitute(fit)), '.rds')
  readRDS(file)
}


##' Save Compact Version of rms Bayesian Fit
##'
##' Removes the \code{rstan} part a fit object and writes the fit object to the current working directory, with a \code{.rds} suffix, using \code{saveRDS}.
##' @title fitSave
##' @param fit fit object whose unquoted name becomes the base name of the \code{.rds} file
##' @param name suffix of file name to use if not the name of the first argument passed to \code{fitSave}
##' @author Frank Harrell
fitSave <- function(fit, name=as.character(substitute(fit))) {
  file <- paste0(name, '.rds')
  fit$rstan <- NULL
  saveRDS(fit, file, compress='xz')
  invisible()
}

