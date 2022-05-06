## Used with Quarto documents
## Loops through the elements of a named list and outputs each element into
## a separate `Quarto` tab.  A `wide` argument is used to expand the width
## of the output outside the usual margins.  An `initblank` argument
## creates a first tab that is empty.  This allows one to show nothing
## until one of the other tabs is clicked.

maketabs <- function(x, labels=names(x), wide=FALSE, initblank=FALSE) {
  yaml <- paste0('.panel-tabset', if(wide) ' .column-page')
  k <- c('', '::: {', yaml, '}', '')
  if(initblank) k <- c(k, '', '##   ', '')
  .objmaketabs. <<- x
  for(i in 1 : length(x)) {
    cname <- paste0('c', round(100000 * runif(1)))
    k <- c(k, '', paste('##', labels[i]), '',
           paste0('```{r ', cname, ',results="asis",echo=FALSE}'),
           paste0('.objmaketabs.[[', i, ']]'), '```', '')
  }
  k <- c(k, ':::', '')
  cat(knitr::knit(text=knitr::knit_expand(text=k), quiet=TRUE))
}
