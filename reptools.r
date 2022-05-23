##' Print Named List of Vectors
##'
##' Function to print a simple named list of vectors in html
##' Creates a column `name` from the names of the list
##' If a vector element of the list is numeric, it is rounded
##' to `dec` digits to the right of the decimal place.
##' @title htmlList
##' @param x a named list
##' @param dec round to this decimal place
##' @return a `kable`
##' @author Frank Harrell
htmlList <- function(x, dec=4) {
  g <- function(z)
	  if(is.character(z)) paste(z,             collapse=', ') else
	                      paste(round(z, dec), collapse=', ')
  w <- sapply(x, g)
	d <- data.frame(name=names(w), value=w)
	rownames(d) <- NULL
	kabl(d, col.names=NULL)
	}


##' Front-end to `kable` and `kables`
##' Calls `kable()` if only one table is to be printed.
##' Calls `kable()` for each table and passes it to `kables` if more than one.
##' Accounts for results of `tapply` not being a vector (is an array)
##'
##' @title kabl
##' @param ... one or more objects to pass to `kable`
##' @param caption overall single caption
##' @param digits passed to `kable` and applies to all tables
##' @param col.names passed to `kable`
##' @param row.names passed to `kable`
##' @return result of `kable` or `kables`
##' @author Frank Harrell
##' @md
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

##' Make Tabs for Quarto Documents
##'
##' Loops through the elements of a named list and outputs each element into
##' a separate `Quarto` tab.  A `wide` argument is used to expand the width
##' of the output outside the usual margins.  An `initblank` argument
##' creates a first tab that is empty.  This allows one to show nothing
##' until one of the other tabs is clicked.
##' @title maketabs 
##' @param x a named list.  Names become tab names if `labels` is not given.
##' @param labels optional vector of names for tabs.
##' @param wide set to `TRUE` to have `.column-page` Quarto output that is wider than the margins
##' @param initblank set to `TRUE` to create an initial tab that is blank.  This keeps any content from showing initially.
##' @return 
##' @author Frank Harrell
##' @md
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

makecolmarg <- function(x) {
  .objcolmarg. <<- x
  cname <- paste0('c', round(100000 * runif(1)))
  k <- c('', '::: {.column-margin}', '',
         paste0('```{r ', cname, ',results="asis",echo=FALSE}'),
         '.objcolmarg.', '', '```', '', ':::', '')
  cat(knitr::knit(text=knitr::knit_expand(text=k), quiet=TRUE))
}



##' Convert Objects to HTML and View
##'
##' Converts a series of objects created to html.
##' Displays these in the RStudio View pane.
##' If RStudio is not running displays in an external browser.
##' Assumes there is an `html` method for the objects (e.g., objects
##' are result of `Hmisc::describe` or `Hmisc::contents`.
##' User can page through the different outputs with the arrow keys
##' in the RStudio View pane
##' @title htmlView
##' @param ... any number of objects for which an `html` method exists
##' @return 
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##'  htmlView(contents(d1), contents(d2))
##'  htmlView(describe(d1), describe(d2, descript='Second Dataset'))
##'  htmlView(contents(d), describe(d))
##' }
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

##' Convert to HTML and Eternally View Objects
##'
##' `htmlViewx` is similar to `htmlView` except that an external viewer is
##' launched, and the first object is opened in a new window.
##' Subsequent objects are opened in a new tab in the last created
##' window.  Set `options(vbrowser='command line to run browser')`
##' to use a browser other than `Vivaldi`.
##' Defaults to opening a new window for only the first object, and adding
##' tabs after that.
##' @title htmlViewx
##' @param ... a series of objects for which an `html` method exists
##' @param tab set to `'all'` to add even the first object to an existing window.
##' @return 
##' @author Frank Harrell
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

##' View Rscripts Help Files in Viewer
##'
##' Displays html help files for <https://github.com/harrelfe/rscripts> functions in the RStudio View pane.  These help files are found in <https://hbiostat.org/R/rscripts>.
##' If RStudio is not running, displays in an external browser.
##' @title rsHelp
##' @param fun unquoted name of function
##' @return 
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##'  rsView(dataChk)
##' }
rsHelp <- function(fun) {
  fun <- as.character(substitute(fun))
  viewer <- getOption('viewer', default=utils::browseURL)
  infile <- paste0('https://hbiostat.org/R/rscripts/', fun, '.html')
  
  td <- tempdir()
  if(! dir.exists(td)) dir.create(td)
  fi <- paste0(tempfile(), '.html')
  download.file(infile, fi, quiet=TRUE)
  viewer(fi)
}




##' Run a Series of Data Checks and Report
##'
##' Function to run various data checks on a data table.
##'
##' Checks are run separately for each part of the `expression` vector `checks`.  For each single expression, the variables listed in the output are all the variables mentioned in the expression plus optional variables whose names are in the character vector `id`. `%between% c(a,b)` in expressions is printed as `[a,b]`.  The output format is plain text unless `html=TRUE` which also puts each table in a separate Quarto tab.  See [here](https://www.fharrell.com/post/rflow) for examples.

##' @title dataChk
##' @param d a data table
##' @param checks a vector of expressions that if satisfied causes records to be listed
##' @param id option vector of variable names to serve as IDs
##' @param html set to `TRUE` to create HTML output and put each check in a separate tab, also creating summary tabs
##' @param omit0 set to `TRUE` to ignore checks finding no observations
##' @param byid if `id` is given set `byid=TRUE` to also list a data frame with all flagged conditions, sorted by `id` 
##' @param nrows maximum number of rows to allow to be printed 
##' @return an invisible data frame containing variables `check` (the expression checked) and `n` (the number of records satisfying the expression)
##' @author Frank Harrell
##' @md
dataChk <- function(d, checks, id=character(0),
                    html=FALSE, omit0=FALSE, byid=FALSE, nrows=500) {
  if(byid && length(id) < 1) stop('must specify id when byid=TRUE')
  s  <- NULL
  X  <- Dat <- list()
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
    if(byid && no > 0) {
      Da <- z[, id, with=FALSE]
      Da[, Check := cx]
      Da[, Values := do.call(paste, z[, vars.involved, with=FALSE])]
      Dat[[cx]] <- Da
      }
    z <- if(no == 0) 'n=0' else capture.output(print(z, nrows=nrows))
    z <- fmt(cx, z)
    if(no > 0 || ! omit0) X[[cx]] <- z
    s <- rbind(s, data.frame(Check=cx, n=no))
  }
  if(byid) {
    Dat <- rbindlist(Dat, fill=TRUE)
    # setcolorder(Dat, c(id, 'Check', setdiff(names(Dat), c(id, 'Check'))))
    setkeyv(Dat, id)
    u <- paste('By', paste(id, collapse=', '))
    X[[u]] <- fmt(u, capture.output(print(Dat, nrows=nrows)))
    }
  X$Summary <- fmt('Summary', capture.output(print(s, nrows=nrows)))
  if(html) maketabs(X, initblank=TRUE)
  else for(z in X) cat(z, sep='\n')
  if(byid) invisible(s) else invisible(Dat)
}


##' Separate Chunk Plot
##'
##' Runs a plot on its own `Rmarkdown/Quarto` `knitr` Chunk.  The plot will have its own caption and size, and short captions are placed in the markdown TOC
##'
##' Expressions cannot be re-used, i.e., each expression must evaluate to the right quantity after the chunk in which the `scplot` calls are made is finished, and the new constructed chunk is input.  To input and run the constructed chunk:
##' `{r child='scplot.Rmd'}` preceeded and following by 3 back ticks.
##' Hmisc::putHcap is used to markup regular and short captions `cap, scap`.  Short caption appears in TOC.  If no `scap`, then `cap` is used for this. To change the `putHcap` `subsub` argument set `options(scplot.subsub='## ')` for example.
##' @title scplot
##' @param command an command that causes a plot to be rendered
##' @param cap long caption
##' @param scap short caption
##' @param w width of plot
##' @param h height of plot
##' @param id a string ID for the plot.  Defaults to the current chunk label if `knitr` is running
##' @return 
##' @author Frank Harrell
##' @md
##' @examples
##' \dontrun{
##' scplot(id='chunkid')   # initialize output file scplot.Rmd
##' # or use scplot() to use the current chunk name as the id
##' # scplot(plotting expression, caption, optional short caption, w, h)
##' # scplot(plotting expression ...)
##' }

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

