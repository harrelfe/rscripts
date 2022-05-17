## Function to print a simple named list of vectors in html
## Creates a column `name` from the names of the list
## If a vector element of the list is numeric, it is rounded
## to `dec` digits to the right of the decimal place.
htmlList <- function(x, dec=4) {
  g <- function(z)
	  if(is.character(z)) paste(z,             collapse=', ') else
	                      paste(round(z, dec), collapse=', ')
  w <- sapply(x, g)
	d <- data.frame(name=names(w), value=w)
	rownames(d) <- NULL
	kabl(d, col.names=NULL)
	}


## Calls kable() if only one table is to be printed
## Calls kable() and each table and passes it to kables if more than one
## Accounts for results of tapply not being a vector (is an array)
## digits applies equally to all tables
## caption applies to the overall result

kabl <- function(..., caption=NULL, digits=4, col.names=NA, row.names=NA) {
  w <- list(...)
  tr <- function(x)
    if(is.vector(x) || (is.array(x) && length(dim(x)) == 1)) t(x) else x
  format <- if(knitr::is_latex_output()) 'latex' else 'pipe'
  if(length(w) == 1) {
    w <- w[[1]]
    return( knitr::kable(tr(w), digits=digits, caption=caption,
                         col.names=col.names, row.names=row.names,
                         format=format))
  }
  w <- lapply(w, function(x) knitr::kable(tr(x), digits=digits, format=format))
  knitr::kables(w, caption=caption, format=format)
}

