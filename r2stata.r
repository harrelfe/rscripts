# title: Convert an R data frame to a Stata .dta file
# major: data manipulation
# minor: import
#
## Convert an R data file to a Stata data file
## To make write_stata in haven package work must remove labelled class
## from variables that have label attributes
## Output file will be named foo.dta where foo is the name of the first
## argument in the invocation of r2stata
## Specify for example dir='/tmp' to create /tmp/foo.dta

r2stata <- function(x, pkg=c('foreign', 'haven'), dir='') {
  pkg=match.arg(pkg)
  name <- deparse(substitute(x))
  file <- paste(name, 'dta', sep='.')
  if(dir != '') file <- paste(dir, file, sep='/')
  
  if(pkg == 'foreign') {
    require(foreign)
    vl <- sapply(x, label)
    un <- sapply(x, units)
    vl <- ifelse(un == '', vl, paste(vl, ' [', un, ']', sep=''))
    attr(x, 'var.labels') <- vl
    write.dta(x, file, version=10)
  } else {
    require(haven)
    for(i in 1 : length(x)) {
      w <- x[[i]]
      cl <- attr(w, 'class')
      if('labelled' %in% cl) {
        cl <- setdiff(cl, 'labelled')
        class(w) <- cl
        x[[i]] <- w
      }
    }
    write_dta(x, file)
  }
  invisible()
}
