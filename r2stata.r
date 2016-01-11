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

r2stata <- function(x, dir='') {
  require(haven)
  name <- deparse(substitute(x))
  for(i in 1 : length(x)) {
    w <- x[[i]]
    cl <- attr(w, 'class')
    if('labelled' %in% cl) {
      cl <- setdiff(cl, 'labelled')
      class(w) <- cl
      x[[i]] <- w
    }
  }

  file <- paste(name, 'dta', sep='.')
  if(dir != '') file <- paste(dir, file, sep='/')
  write_dta(x, file)
  invisible()
}
