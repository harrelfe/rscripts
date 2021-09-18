## Given an RDS file name and a list of objects, does the following
## * makes a vector of hashes, one for each object
##   function objects are run through deparse so environment of function
##   will not be considered
## * see if the file exists; if not, return a list with result=NULL,
##   hash = new vector of hashes, changed='All'
## * if the file exists, read the file and its hash attribute as prevhash
## * if prevhash is not identical to hash:
##     if .print.=TRUE (default), print to console a summary of what's changed
##     return a list with result=NULL, hash = new hash vector, changed
## * if prevhash = hash, return a list with result=file object, hash=new hash,
##   changed=''
##
## Set options(debughash=TRUE) to trace results in /tmp/debughash.txt

hashCheck <- function(..., file, .print.=TRUE) {
  d <- list(...)
  nam <- as.character(sys.call())[-1]
  nam <- nam[1 : length(d)]
  names(d) <- nam

  
  debug <- length(.Options$debughash) && .Options$debughash
  ct <- if(debug)
          function(...) cat(..., '\n', file='/tmp/debughash.txt', append=TRUE)
        else
          function(...) {}

  ct(nam)
  g <- function(x) digest::digest(if(is.function(x)) deparse(x) else x)
  hash <- sapply(d, g)
  if(debug) prn(hash, fi='/tmp/debughash.txt')
  
  prevhash <- NULL
  if(! file.exists(file)) {
    ct('no file', file)
    return(list(result=NULL, hash=hash, changed='All'))
    }

  R        <- readRDS(file)
  prevhash <- attr(R, 'hash')
  if(! length(prevhash)) {
    if(.print.) cat('\nRe-run because of no previous hash\n\n')
    ct('no previous hash')
    return(list(result=NULL, hash=hash, changed='No previous hash'))
    }

  samelen <- length(hash) == length(prevhash)
  if(samelen && all(hash == prevhash)) {
    ct('no change')
    return(list(result=R, hash=hash, changed=''))
    }

  s <- character(0)

  if(! samelen) {
    a <- names(prevhash)
    b <- names(hash)
    w <- setdiff(a, b)
    if(length(w))
      s <- c(s, paste('objects removed:',
                      paste(w, collapse=' ')))
    w <- setdiff(b, a)
    if(length(w))
      s <- c(s, paste('objects added:',
                      paste(w, collapse=' ')))
  } else
    s <- c(s, paste('changes in the following objects:',
                    paste(nam[hash != prevhash], collapse=' ')))
  s <- paste(s, collapse=';')
  ct(s)
      
  if(.print.) cat('\nRe-run because of', s, '\n\n')

  list(result=NULL, hash=hash, changed=s)
}

## Uses hashCheck to run a function and save the results if specified
## inputs have changed, otherwise to retrieve results from a file.
## The file name is taken as the chunk name appended with .rds unless
## it is given as file=.  fun has no arguments.
## Set .inclfun.=FALSE to not include fun in the hash check

runifChanged <- function(fun, ..., file=NULL, .print.=TRUE, .inclfun.=TRUE) {
  if(! length(file)) {
    file <- knitr::opts_current$get('label')
    if(! length(file)) stop('attempt to run runifChanged outside a knitr chunk')
    file <- paste0(file, '.rds')
  }
  hashobj <- if(! .inclfun.) hashCheck(..., file=file, .print.=.print.)
             else {
               w         <- list(...)
               w$fun     <- fun
               w$file    <- file
               w$.print. <- .print.
               do.call(hashCheck, w)
               }
               
  hash    <- hashobj$hash
  result  <- hashobj$result
  if(! length(result)) {
    result <- fun()
    setattr(result, 'hash', hash)
    saveRDS(result, file, compress='xz')
  }
  result
}


  
