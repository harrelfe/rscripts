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
##' Create Text for Running Code Chunk
##'
##' Creates text strings suitable for running through `knitr`.  The chunk is given a random name because certain operations are not allowed by `knitr` without it.
##' @title makecodechunk
##' @param cmd character string vector of commands to run inside chunk
##' @param results format of results, default is `'asis'`.  May specify `results='markup'`.
##' @param callout an optional Quarto callout to include after `#|` after the chunk header that affects how the result appears, e.g. `callout='column: margin'`
##' @param h, w optional height and width to place after the chunk header after `#|`
##' @return character vector 
##' @author Frank Harrell
makecodechunk <- function(cmd, results='asis',
                          callout=NULL, h=NULL, w=NULL) {
  if(! length(cmd) || (is.character(cmd) && length(cmd) == 1 &&
            cmd %in% c('', ' ', "` `"))) return('')

  r <- paste0('results="', results, '"')
  cname <- paste0('c', round(1000000 * runif(1)))
  if(length(callout)) callout <- paste('#|', callout)
  if(length(h)) h <- paste('#| fig.height:', h)
  if(length(w)) w <- paste('#| fig.width:', w)
  c('',
    paste0('```{r ', cname, ',', r, ',echo=FALSE}'),
    cmd, callout, h, w, '```', '')
    }

##' General Case Handling of Quarto Callouts
##'
##' This function generates and optionally runs markdown/R code that runs Quarto callouts such as collapsible notes or marginal notes.
##' @title makecallout
##' @param x object to print (if `type='print'`), or one or more formulas whose right hand sides are to be run.  Left side provides labels if needed by the particular callout, and if `raw` is included on the right side any R code chunk run will have `results='asis'` in the chunk header.
##' @param callout character string giving the Quarto callout
##' @param label character string label if needed and if not obtained from the left side of a formula
##' @param type defaults to `'print'` to print an object.  Set to `'run'` to run a chunk.
##' @param now set to `FALSE` to return code instead of running it
##' @param results if not using formulas, specifies the formatting option to code in the code header, either `'asis'` (the default) or `'markup'`
##' @param close specifies whether to close the callout or to leave it open for future calls
##' @param ... parameters passed to `print`
##' @return if code is not executed, returns a character vector with the code to run
##' @md
##' @author Frank Harrell
makecallout <- function(...) {

  ## Define internal function to keep new variables from clashing
  ## with knitted code

  build <- function(x, callout=NULL, label=NULL,
                    type=NULL, now=TRUE, results='asis',
                    close=length(callout), ...) {
  if(! length(type)) type <- 'print'
  k  <- character(0)

  if('formula' %in% class(x)) {
    v         <- as.character(attr(terms(x), 'variables'))[-1]
    if(! attr(terms(x), 'response')) {  # no left side variable
      label <- NULL
      x     <- v
      } else {
        ## Get left hand side and remove leading/training backticks
        left  <- sub('`$', '', sub('^`', '', v[1]))
        label <- paste('##', left)  # left-hand side
        x     <- v[-1]              # right-hand side
        }
    raw       <- 'raw' %in% x
    if(raw) x <- setdiff(x, 'raw')
    type      <- 'run'
    now       <- TRUE
    results   <- if(raw) 'markup' else 'asis'
  }

  if(length(callout))
    k <- c('', paste0('::: {', callout, '}'),
           if(! length(label)) '', label)

  res <- if(is.character(x) && length(x) == 1 &&
            all(x %in% c('', ' ', "` `"))) ' '
         else
           if(type == 'print') capture.output(print(x, ...))
         else
           makecodechunk(x, results=results)
  k <- c(k, res, if(close) c(':::', ''))
  list(k=k, now=now, type=type)
  }
  .z. <- build(...)
  .k. <- .z.$k
  if(.z.$now) {
    switch(.z.$type,
           print = cat(.k., sep='\n'),
           run   = cat(knitr::knit(text=knitr::knit_expand(text=.k.),
                               quiet=TRUE)))
    return(invisible())
  }
  .k.
}

##' Print an Object in a Collapsible Note
##'
##' Prints an object in a Quarto collapsible note.
##' @title makecnote
##' @param x an object having a suitable `print` method
##' @param label a character string providing a title for the tab.  Default is the name of the argument passed to `makecnote`.
##' @param ... an optional list of arguments to be passed to `print`
##' @return 
##' @author Frank Harrell
##' @md
makecnote <- function(x,
                      label=paste0('`', deparse(substitute(x)), '`'),
                      wide=FALSE,
                      type=c('print', 'run'),
                      ...) {
  type <- match.arg(type)
  co <- paste('.callout-note', if(wide) '.column-page', 'collapse="true"')
  makecallout(x, callout=co,
              label=paste('#', label), type=type, ...)
  invisible()
}

##' Put an Object in the Margin
##'
##' Prints an object in a Quarto column margin.
##' @title makecolmarg
##' @param x an object having a suitable `print` method
##' @param ... an optional list of arguments to be passed to `print`
##' @return 
##' @author Frank Harrell
##' @md
makecolmarg <- function(x, type=c('print', 'run'), ...) {
  type <- match.arg(type)
  makecallout(x, callout='.column-margin', type=type, ...)
  invisible()
}

##' Make Quarto Tabs
##'
##' Loops through a series of formulas or elements of a named list and outputs each element into
##' a separate `Quarto` tab.  A `wide` argument is used to expand the width
##' of the output outside the usual margins.  An `initblank` argument
##' creates a first tab that is empty, or you can specify a formula `` `` ~ `` ``.  This allows one to show nothing
##' until one of the other tabs is clicked.  Multiple commands can be run in one chunk by including multiple right hand terms in a formula.  A chunk can be marked for producing raw output by including a term `raw` somewhere in the formula's right side.
##' @title maketabs
##' @param ... a series of formulas or a single named list.  For formulas the left side is the tab label (if multiple words or other illegal R expressions enclose in backticks) and the right hand side has expressions to evaluate during chunk execution, plus an optional `raw` option.
##' @param wide 
##' @param initblank 
##' @return 
##' @author Frank Harrell
# See https://stackoverflow.com/questions/42631642
maketabs <- function(..., wide=FALSE, initblank=FALSE) {
  .fs. <- list(...)
  if(length(.fs.) == 1 && 'formula' %nin% class(.fs.[[1]]))
    .fs. <- .fs.[[1]]   # undo list(...) and get to 1st arg to maketabs
  
  makechunks <- function(fs, wide, initblank) {
    ## Create variables in an environment that will not be seen
    ## when knitr executes chunks so that no variable name conflicts
    
    yaml   <- paste0('.panel-tabset', if(wide) ' .column-page')

    k <- c('', paste0('::: {', yaml, '}'), '')
    if(initblank) k <- c(k, '', '##   ', '')

    for(i in 1 : length(fs)) {
      f <- fs[[i]]
      if('formula' %in% class(f)) {
        v   <- as.character(attr(terms(f), 'variables'))[-1]
        y   <- v[1]   # left-hand side
        x   <- v[-1]  # right-hand side
        raw <- 'raw' %in% x
        if(raw) x <- setdiff(x, 'raw')
      } else {
        raw  <- FALSE
        y    <- names(fs)[i]
        x    <- paste0('.fs.[[', i, ']]')
      }
      r <- paste0('results="',
                  if(raw) 'markup' else 'asis',
                  '"')
      cname <- paste0('c', round(1000000 * runif(1)))
      k <- c(k, '', paste('##', y), '',
             makecodechunk(x, results=if(raw) 'markup' else 'asis'))
    }
    c(k, ':::', '')
  }
  cat(knitr::knit(text=knitr::knit_expand(
      text=makechunks(.fs.,
                      wide=wide, initblank=initblank)), quiet=TRUE))
  return(invisible())
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

