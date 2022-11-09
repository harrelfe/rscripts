# title: Function to clean up REDCap dataset exported to R
# major: data manipulation
# minor: import
#
importREDCap <- function(file=NULL, pr=TRUE) {

## Purpose: Process the two files exported from REDCap exactly as they
## were created by REDCap: the R script and the data .csv file.  Remove
## unnecessary variables and convert date variables to R Date variables.
##
## Given file (the name of the REDCap exported R script)
##
## 1. Runs the R script created by REDCap, which calls read.csv to
##    input the data.  Ignores lines containing rm(list=ls()) and
##    graphics.off()
## 2. Removes all original non-factor versions of variables that also exist
##    as factors
## 3. Renames factorized versions of variables by dropping .factor from the
##    end of the name
## 4. Converts any character or factor variable that other than blanks and
##    NAs contains only valid dates as judged by the as.Date function
##    (i.e., yyyy-mm-yy) to R Date variables, keeping the original label
## 5. Adds as a "codes" attribute to all factor variables the vector of
##    original codes of the variable in REDCap before value labels were
##    applied by factor()
## 6. Changes any factor variable having at least n/2 levels, where n is
##    number of observations, to a character vector
##
## If file is not given, the last created file containing _R_*.r at the end
## of its name is used.  Specify pr=FALSE to suppress printing of
## information about the conversion process.
##
## Example: mydata <- importREDCap()
##          Save(mydata)    # creates mydata.rda
##
## Requires Hmisc

  if(! length(file)) {
    possfiles <- file.info(list.files(pattern=".*_R_.*\\.r$"))
    if(nrow(possfiles) == 0) stop('no qualifying R scripts found')
    ## Find newest qualifying file
    file <- rownames(possfiles)[order(possfiles$mtime, decreasing=TRUE) == 1]
    if(pr) cat('Running', file, '\n')
  }
  ## Override factor function to make a version that retains the original
  ## levels as the "codes" attribute
  factor <- function(x, levels, ...) {
    z <- base::factor(x, levels, ...)
    attr(z, 'codes') <- all.is.numeric(levels, what='vector')
    z
  }
  f <- readLines(file)
  f <- f[f %nin% c('rm(list=ls())', 'graphics.off()')]
  eval(parse(text=f))   ## runs scripts, creates object data
  
  n <- names(data)
  delete <- integer(0)
  old <- options(warn = -1)
  on.exit(options(old))
  for(i in 1 : length(n)) {
    ni  <- n[i]
    w   <- data[[i]]
    lab <- label(w)
    if(is.character(w) || is.factor(w)) {
      k <- ! (is.na(w) | w == '')
      if(! any(k)) next
      if(is.factor(w) && length(levels(w)) > 0.5 * length(w)) {
        w <- as.character(w)
        label(w) <- lab
        data[[i]] <- w
      }
      w <- as.character(w)
      if(! all(grepl('^[1-2][0-9][0-9][0-9]-[0-1]*[0-9]-[0-3]*[0-9]$',
                     w[k]))) next
      dat <- tryCatch(as.Date(ifelse(w == '', NA, w), error=function(x) x))
      if(inherits(dat, 'error')) next
      label(dat) <- lab
      data[[i]]  <- dat
    }
    ## See if current variable has a factor-ized version
    if(! grepl('\\.factor', ni)) {
      j <- which(gsub('\\.factor', '', n) == ni)
      if(length(j) > 2) stop('more than 2 variables have the same base name')
      if(length(j) == 2) {
        delete <- c(delete, i)
        j <- setdiff(j, i)
        ## Label the .factor version of the variable; these aren't labeled
        label(data[[j]]) <- lab
      }
    }
  }
  if(length(delete)) {
    if(pr) {
      cat('\nVariables deleted because same variable exists as factor:\n',
          paste(names(data)[delete], sep=','), '\n\n')
      cat('(.factor removed from names of factor version)\n')
    }
    data <- data[, -delete]
  }
  n <- names(data)
  names(data) <- gsub('\\.factor', '', n)
  data
}



cleanupREDCap <- function(d, mchoice=TRUE, rmhtml=TRUE, pr=TRUE, ...) {
  # Purpose: Clean up a data frame imported from REDCap using either
  # manual export or API.  By default removes html tags from variable
  # labels and converts sequences of variables representing a single
  # multiple choice question to a single variable using Hmisc::mChoice
  #
  # Multiple choice variables are found by looking for variable names
  # that end in three underscores followed only by integers
  #
  # Set pr=FALSE to not print information about mChoice variables created
  # ... arguments are passed to mChoice
  #

  require(data.table)
  d <- copy(d)
  setDT(d)   # will leave existing keys intact if already a data.table

  if(rmhtml) {
    trans <- function(x) {
      rem <- c('<p>', '</p>', '</div>', '</span>', '<p .*?>', '<div .*?>', '<span .*?>',
               '<br>', '<br />', '\\n')
      for(a in rem) x <- gsub(a, '', x)
      x
      }
    for(v in names(d)) {
      lab <- attr(d[[v]], 'label')
      if(length(lab)) setattr(d[[v]], 'label', trans(lab))
    }
  }

  if(! mchoice) return(d)

  # Find all variable names that are part of multiple choice sequences
  # These names end in ___x with x being an integer
  n <- names(d)
  i <- grep('^.*___[0-9][0-9]*[0-9]*$', n)
  i <- grep('^.*___.*$', n)
  if(! length(i)) return(d)
  n <- n[i]

  basename <- sub('___[0-9][0-9]*[0-9]*$', '', n)
  basename <- sub('___.*', '', n)
  if(any(basename %in% names(d)))
    stop('base name for multiple choice variable has the same name as a non-multiple choice variable')

  for(v in unique(basename)) {
    V <- n[basename == v]
    numbers    <- sub(paste0('^', v, '___'), '', V)
    if(! all.is.numeric(numbers)) next
    numbers    <- as.integer(numbers)
    numchoices <- length(numbers)
    first <- paste0(v, '___', min(numbers))
    d[, (v) := do.call('mChoice', c(.SD, ...)), .SDcols=V]
    setattr(d[[v]], 'label', label(d[[first]]))
    d[, (V) := NULL]
    if(pr) cat(numchoices, 'variables named', paste0(v, '___*'), 'combined into mChoice variable', v, '\n')
  }
d
}
