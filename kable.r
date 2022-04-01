## Calls kable() if only one table is to be printed
## Calls kable() and each table and passes it to kables if more than one
## Accounts for results of tapply not being a vector (is an array)
## digits applies equally to all tables
## caption applies to the overall result

kabl <- function(..., caption=NULL, digits=4) {
  w <- list(...)
  tr <- function(x)
    if(is.vector(x) || (is.array(x) && length(dim(x)) == 1)) t(x) else x
  if(length(w) == 1) {
    w <- w[[1]]
    return( knitr::kable(tr(w), digits=digits))
  }
  w <- lapply(w, function(x) knitr::kable(tr(x), digits=digits))
  knitr::kables(w, caption=caption)
}
