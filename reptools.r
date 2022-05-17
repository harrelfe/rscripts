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


## Converts a series of objects created to html
## Displays these in the RStudio View pane
## If RStudio is not running displays in an external browser
## Assumes there is an html method for the objects (e.g., objects
## are result of Hmisc::describe or Hmisc::contents
## Examples:
##  htmlView(contents(d1), contents(d2))
##  htmlView(describe(d1), describe(d2, descript='Second Dataset'))
##  htmlView(contents(d), describe(d))
##
## User can page through the different outputs with the arrow keys
## in the RStudio View pane

htmlView <- function(...) {
  viewer <- getOption('viewer', default=utils::browseURL)
  w <- list(...)
  td <- tempdir()
  if(! dir.exists(td)) dir.create(td)
  for(u in w) {
    fi <- paste0(tempfile(), '.html')
    h  <- html(u)
    writeLines(as.character(h), fi)
    viewer(fi)
  }
}

## htmlViewx is similar to htmlView except that an external viewer is
## launched, and the first object is opened in a new window.
## Subsequent objects are opened in a new tab in the last created
## window.  Set options(vbrowser='command line to run browser')
## to use a browser other than Vivaldi
## Defaults to opening a new window for only the first object, and adding
## tabs after that.
## Set tab='all' to add even the first object to an existing window

htmlViewx <- function(..., tab=c('notfirst', 'all', 'none')) {
  tab <- match.arg(tab)
  view <- getOption('vbrowser')
  if(! length(view)) view <- 'vivaldi'
  w <- list(...)
  td <- tempdir()
  if(! dir.exists(td)) dir.create(td)

  i <- 0
  for(u in w) {
    i <- i + 1
    fi <- paste0(tempfile(), '.html')
    h  <- html(u)
    writeLines(as.character(h), fi)
    cmd <- paste0(view,
                  switch(tab,
                         all  = ' -new-tab',
                         none = ' -new-window',
                         notfirst = if(i == 1) ' -new-window' else ' -new-tab' ))
    browseURL(fi, browser=cmd)
  }
}


# Function to run various data checks on a data table d
# checks is a vector of expressions that if satisfied causes records to be listed
# The variables listed are all variables mentioned in the expression plus
# optional variables whose names are in the character vector id
# %between% c(a,b) in expressions is printed as [a,b]
# The output format is plain text unless html=TRUE which also puts
# each table in a separate Quarto tab (and you must have run
# getRs('maketabs.r', put='source') previously).
# The returned value is an invisible data frame containing variables
# check (the expression checked) and n (the number of records satisfying
# the expression)
# Set omit0 to TRUE to ignore checks finding no observations.

dataChk <- function(d, checks, id=character(0), html=FALSE, omit0=FALSE) {
  s  <- NULL
  X  <- list()
  dashes <- paste0(rep('-', getOption('width')), collapse='')
  fmt <- if(html)
           function(name, data)
             htmltools::HTML(c('<pre>', paste(data, '\n'),
                               '</pre>'))
         else function(name, data) c(dashes, name, dashes, data)
  
  for(i in 1 : length(checks)) {
    x <- checks[i]
    cx <- as.character(x)
    cx <- gsub('%between% c\\((.*?)\\)', '[\\1]', cx)
    form <- as.formula(paste('~', cx))
    ## Find all variables mentioned in expression
    vars.involved <- all.vars(form)
    z  <- d[eval(x), c(id, vars.involved), with=FALSE]
    no <- nrow(z)
    z <- if(no == 0) 'n=0' else capture.output(print(z))
    z <- fmt(cx, z)
    if(no > 0 || ! omit0) X[[cx]] <- z
    s <- rbind(s, data.frame(Check=cx, n=no))
  }
  X$Summary <- fmt('Summary', capture.output(print(s)))
  if(html) maketabs(X, initblank=TRUE)
  else for(z in X) cat(z, sep='\n')
  invisible(s)
}


## Separate chunk plot
##
## Function to run a plot in its own Rmarkdown knitr chunk with its own
## caption and size, and to put short captions in the markdown TOC
##
## Usage:
##
## scplot(id='chunkid')   # initialize output file scplot.Rmd
## or use scplot() to use the current chunk name as the id
## scplot(plotting expression, caption, optional short caption, w, h)
## scplot(plotting expression ...)
##
## Note that the expressions cannot be re-used, i.e., each expression
## must evaluate to the right quantity after the chunk in which the
## scplot calls are made is finished, and the new constructed chunk is input.
## To input and run the constructed chunk:
## ```{r child='scplot.Rmd'}
## ```
##
## Hmisc::putHcap is used to markup regular and short captions cap, scap
## Short caption appears in TOC.  If no scap, then cap is used for this.
## To change the putHcap subsub argument set options(scplot.subsub='## ') e.g.

scplot <- function(command, cap=NULL, scap=NULL, w=5, h=4, id=NULL) {

  command <- as.character(sys.call())
  if(length(command) == 1) id <- knitr::opts_current$get('label')

  if(length(id)) {
    cat('', sep='', file='scplot.Rmd')
    .iscplot.  <<- 0
    .idscplot. <<- id
    return(invisible())
    }

  .iscplot. <<- .iscplot. + 1

  cname <- paste0(.idscplot., .iscplot.)
  subsub <- if(length(.Options$scplot.subsub)) .Options$scplot.subsub else TRUE
  label  <- Hmisc::putHcap(cap, scap=scap, subsub=subsub, file=FALSE)

  k <- c(paste0('\n\n```{r ', cname, ',results="asis",echo=FALSE,fig.width=',
                w, ',fig.height=', h, '}\n'), paste0(command[2], '\n```\n\n'))
  cat(label, k, sep='', file='scplot.Rmd', append=TRUE)
  invisible()
}

