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
makecodechunk <- function(cmd, results='asis', lang='r',
                          callout=NULL, h=NULL, w=NULL) {
  if(! length(cmd) || (is.character(cmd) && length(cmd) == 1 &&
            cmd %in% c('', ' ', "` `"))) return('')

  r <- paste0('results="', results, '"')
  if(lang == 'r') cname <- paste0('c', round(1000000 * runif(1)))
  if(length(callout)) callout <- paste('#|', callout)
  if(length(h)) h <- paste('#| fig.height:', h)
  if(length(w)) w <- paste('#| fig.width:', w)
  c('',
    if(lang == 'r') paste0('```{r ', cname, ',', r, ',echo=FALSE}')
    else            paste0('```{', lang, '}'),
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


missChk <- function(d, use=NULL, exclude=NULL,
                    maxpat=15, maxcomb=25, excl1pat=TRUE,
                    sortpatterns=TRUE,
                    prednmiss=FALSE, omitpred=NULL, ...) {
  setDT(d)
  if(length(use)) {
    if(inherits(use, 'formula')) use <- all.vars(use)
    d <- d[, (use)]
  }
  if(length(exclude)) {
    if(inherits(exclude, 'formula')) exclude <- all.vars(exclude)
    use <- setdiff(names(d), exclude)
    d <- d[, (use)]
  }
  
  p <- ncol(d)
  nna <- sapply(d, function(x) sum(is.na(x)))

  if(all(nna == 0)) 
    return(asisOut('No NAs on any of the',
                   p, ' variables examined.'))

  surrq <- function(x) paste0('`', x, '`')

  vmiss <- names(nna)[nna > 0]
  dm    <- d[, .SD, .SDcols=vmiss]
  pm    <- length(vmiss)

  cat('\n', p - pm, 'variables have no NAs and', pm,
      'variables have NAs\n\n')
  
  if(pm < max(20, maxpat)) {
    nap <- na.pattern(dm)
    nap <- matrix(nap, ncol=1, dimnames=list(names(nap), 'Count'))
    n1  <- sum(nap[,1] == 1)
    patex <- ''
    if(excl1pat && n1 > 0) {
      patex <- paste(n1,
                     'patterns with frequency of 1 not listed\n')
      nap <- nap[nap[,1] > 1, 1, drop=FALSE]
      }
      
    if(nrow(nap) <= maxpat) {
      cat('Frequency counts of all combinations of NAs\n\n',
          'Variables in column order are:',
          paste(surrq(vmiss), collapse=', '), '\n\n', patex,
          sep='')
      if(sortpatterns) {
        i   <- order(- nap[, 1])
        nap <- nap[i, , drop=FALSE]
        }
      print(kabl(nap))
      return(invisible())
    }
  }
 
  .naclus. <<- naclus(dm)
  abb <- pm > 40
  ptypes <- c('na per var' = 'NAs/var',
              'na per obs' = 'NAs/obs',
              'mean na'    = 'Mean # var NA when other var NA',
              'na per var vs mean na' =
                'NAs/var vs mean # other variables NA',
              'clus'       = 'Clustering of missingness')
  tabs <- vector('list', length(ptypes))
  for(i in seq(ptypes)) {
    lab <- surrq(ptypes[i])
    f <- if(i < 5) paste0(lab, ' ~ ',
                          'naplot(.naclus., which="', names(ptypes[i]),
                          '")')
         else
           paste0(lab, ' ~ ', 'plot(.naclus., abbrev=', abb, ')')
    tabs[[i]] <- as.formula(f)
  }

  dm <- dm[, lapply(.SD, is.na)]
  ## Produce combination plot for the right number of variables with NAs
  if(pm <= maxcomb) {
    .misscombdata. <<- list(data=dm)
    .misscombArgs. <<- c(list(maxcomb=maxcomb), list(...))
    tabs <- c(tabs,
              `NA combinations` ~ do.call('combplotp',
                                          c(.misscombdata., .misscombArgs.)))
  }

  if(prednmiss && (pm < p)) {
    ## Predict using ordinal regression the number of missing variables
    ## from all the non-missing variables
    ## Compute the number of missing variables per observation
    dm    <- as.matrix(dm)
    Nna   <- apply(dm, 1, sum)
    preds <- names(nna)[nna == 0]
    if(length(omitpred)) {
      omitv <- if(is.character(omitpred)) omitpred
               else
                 all.vars(omitpred)
      preds <- setdiff(preds, omitv)
      }
    form <- paste('Nna ~', paste(preds, collapse=' + '))
    f <- rms::lrm(as.formula(form), data=d)
    if(f$fail)
      cat('prednmiss=TRUE led to failure of ordinal model to fit\n\n')
    else {
      .misschkanova. <<- anova(f)
      .misschkfit.   <<- f
      prtype <- .Options$prType
      options(prType='html')
      tabs <- c(tabs,
                `Predicting # NAs per obs` ~ print(.misschkfit., coefs=FALSE) +
                                             plot(.misschkanova.))
      }
 }
  
  do.call(maketabs, c(list(initblank=TRUE), list(tabs)))
  options(prType=prtype)
}

varType <- function(data, include=NULL, exclude=NULL,
                    ndistinct=10, nnonnum=20) {
  # include: vector of variables in data to attend to
  # exclude: attend to all variables in data except those in exclude
  # include and exclude can be vectors or formulas with only right sides

  g <- function(x) {
    nnum <- is.character(x) || is.factor(x)
    lu   <- length(unique(x))
    fcase(! nnum && lu >= ndistinct, 'continuous',
          nnum && lu > nnonnum,      'nonnumeric',
          default = 'discrete')
  }
  
  if(! is.data.frame(data)) return(g(data))

  if(length(include) && ! is.character(include))
    include <- all.vars(include)
  if(length(exclude) && ! is.character(exclude))
    exclude <- all.vars(exclude)
  v <- if(length(include)) include else setdiff(names(data), exclude)
  
  s <- sapply(if(is.data.table(data)) data[, ..v] else data[v], g)
  split(names(s), s)
}

conVars <- function(...) varType(...)$continuous
disVars <- function(...) varType(...)$discrete
asForm  <- function(x) as.formula(paste('~', paste(x, collapse=' + ')))


makemermaid <- function(.object., ...) {
  x <- strsplit(.object., '\n')[[1]]
  code <- makecodechunk(x, lang='mermaid')
  ki <- knitr::knit_expand
  cat(knitr::knit(text=do.call('ki', c(list(text=code), list(...))),
                  quiet=TRUE))
  invisible()
}


vClus <- function(d, exclude=NULL, fracmiss=0.2, maxlevels=10, minprev=0.05,
                  horiz=FALSE, print=TRUE) {
  w <- as.data.frame(d)  # needed by dataframeReduce
  if(length(exclude)) w <- w[setdiff(names(w), exclude)]
  w <- dataframeReduce(w, fracmiss=fracmiss, maxlevels=maxlevels,
                       minprev=minprev, print=print)
  form <- as.formula(paste('~', paste(names(w), collapse=' + ')))
  v <- varclus(form, data=w)
  if(horiz) plot(as.dendrogram(v$hclust), horiz=TRUE)
  else plot(v)
}

dataOverview <- function(d, d2=NULL, id=NULL,
                         plot=c('scatter', 'dot', 'none'),
                         pr=nvar <= 50, which=1, dec=3) {
  nam1 <-                deparse(substitute(d ))
  nam2 <- if(length(d2)) deparse(substitute(d2))
  plot <- match.arg(plot)

  if(which == 2 && ! length(d2))
    stop('which=2 only applies when second dataset is provided')
  
  d <- copy(d)
  setDT(d)
  if(length(d2)) {
    d2 <- copy(d2)
    setDT(d2)
  }

  ## From rmsb package:
  distSym <- function(x, prob=0.9, na.rm=FALSE) {
    if(na.rm) x <- x[! is.na(x)]
    a <- (1. - prob) / 2.
    w <- quantile(x, probs=c(a / 2., 1. - a / 2.))
    xbar <- mean(x)
    as.vector((w[2] - xbar) / (xbar - w[1]))
  }

  lun <- function(x) length(unique(x))
  
  id1 <- id2 <- FALSE
  ids1 <- ids2 <- NULL
  idv <- if(length(id)) all.vars(id)
  
  nid <- if(length(idv) == 1) paste('ID variable', idv)
         else
           paste('ID variables', paste(idv, collapse=' '))

  if(length(id)) {
    id1 <- all(idv %in% names(d))
    if(id1)
      ids1 <- unique(d[, do.call(paste0, .SD), .SDcols=idv])
    if(length(d2)) {
      id2 <- all(idv %in% names(d2))
      if(id2) ids2 <- unique(d2[, do.call(paste0, .SD), .SDcols=idv])
      }
  }

  ismiss <- function(x) if(is.character(x)) is.na(x) | x=='' else is.na(x) 
  na         <- sapply(d, ismiss) * 1
  na.per.var <- apply(na, 2, sum)
  na.per.obs <- apply(na, 1, sum)

  cat(nam1, ' has ', nrow(d), ' observations (', sum(na.per.obs == 0),
              ' complete) and ', ncol(d), ' variables (', sum(na.per.var == 0),
              ' complete)', sep='')
  if(length(d2)) {
    vcommon <- sum(names(d) %in% names(d2))
    cat(' of which', vcommon, 'variables are in', nam2)
  } else cat('\n')
  
  if(sum(na) > 0) {
    z <- data.frame(Minimum=c(min(na.per.var), min(na.per.obs)),
                    Maximum=c(max(na.per.var), max(na.per.obs)),
                    Mean   =round(c(mean(na.per.var), mean(na.per.obs)), 1),
                    row.names=c('Per variable', 'Per observation'))
    print(kabl(z, caption='Number of NAs'))
    
    tab <- table(na.per.var)
    print(kabl(tab,
               caption='Frequency distribution of number of NAs per variable'))
    tab <- table(na.per.obs)
    print(kabl(tab,
               caption='Frequency ditribution of number of NA variables per observation'))
  }
  
  if(id1) {
    cat('There are', length(ids1), 'unique values of', nid, 'in', nam1)
    if(length(d2) && id2) {
      ncommon <- sum(ids1 %in% ids2)
      cat(' with', ncommon, 'unique IDs also found in', nam2)
    }
    cat('\n')
  }
  if(length(d2)) {
    cat(nam2, 'has', nrow(d2), 'observations and', ncol(d2), 'variables')
    vcommon <- sum(names(d2) %in% names(d))
    cat(' of which', vcommon, 'are in', nam1, '\n')
    if(id2) {
      cat('There are', length(ids2), 'unique values of', nid, 'in', nam2, '\n')
      if(id1) {
        ncommon <- sum(ids2 %in% ids1)
        cat(ncommon, 'unique IDs are found in', nam1, '\n\n')
      }
    } 
  }
  
  ## Get variables types
  w <- switch(which, d, d2)
  nvar <- ncol(w)

  g <- function(x) {
    type <- varType(x)
    info <- NA
    distinct <- lun(x)
    if(type == 'continuous') {
      sym <- distSym(x, na.rm=TRUE)
      x <- round(x, dec)
    }
    
    tab <- table(x)
    n   <- sum(tab)
    fp  <- tab / n
    if(type != 'continuous') sym <- 1. - mean(abs(fp - 1. / length(tab)))
    info <- if(distinct < 2) 0 else (1 - sum(fp ^ 3)) / (1 - 1 / n / n)
    low  <- which.min(tab)
    hi   <- which.max(tab)
                  
    list(type       = upFirst(type),
         distinct   = distinct,
         info       = info,
         symmetry   = sym,
         NAs        = sum(is.na(x)),
         mincat     = names(tab)[low],
         mincatfreq = unname(tab[low]),
         maxcat     = names(tab)[hi],
         maxcatfreq = unname(tab[hi]))
  }

  z <- lapply(w, g)
  r <- rbindlist(z, idcol='variable')
  if(pr) {
    s <- copy(r)
    setnames(s, .q(variable, type, distinct, info, symmetry, NAs, mincat,
                   mincatfreq, maxcat, maxcatfreq),
             .q(Variable, Type, Distinct, Info, Symmetry, NAs, "Rarest Value",
                "Frequency of Rarest Value", Mode, "Frequency of Mode"))
    print(kabl(s, digits=3))
  }
  
  if(plot == 'none') return(invisible())
  
  breaks <- function(mf) {
    br  <- pretty(c(0, mf), 10)
    mbr <- c(0, 10, 20, 30, 40, 50, 100, if(mf >= 200) seq(200, mf, by=100))
    mbr <- mbr[mbr < mf]
    mbr <- setdiff(mbr, br)
    list(br=br, mbr=mbr)
    }
    
  if(plot == 'dot') {
    r <- r[, .(variable, type, distinct, NAs, mincatfreq, maxcatfreq)]
    m <- melt(r, id.vars=c('variable', 'type'),
            variable.name='what', value.name='Freq')
    s <- split(m, m$type)
    b <- breaks(max(m$Freq))
    br <- b$br; mbr <- b$mbr
    gg <- function(data)
      ggplot(data, aes(y=variable, x=Freq, col=what)) + geom_point() +
        scale_x_continuous(trans='sqrt', breaks=br, 
                           minor_breaks=mbr) +
        xlab('') + ylab('Frequency') +
        guides(color=guide_legend(title='')) +
        theme(legend.position='bottom')
    g <- lapply(s, gg)
  } else if(plot == 'scatter') {
    r[, txt := paste(variable,
                     paste('distinct values:', distinct),
                     paste('NAs:', NAs),
                     paste('Info:', round(info, 3)),
                     paste('Symmetry:', round(symmetry, 3)),
                     paste0('lowest frequency (', mincatfreq, ') value:',
                            mincat),
                     paste0('highest frequency (', maxcatfreq, ') value:',
                            maxcat), sep='<br>')]

    b <- breaks(max(r$distinct))
    br <- b$br; mbr <- b$mbr

    gg <- function(data) {
      cap <- paste(nrow(data), 'variables and', nrow(w),
                   'observations\nNumber of NAs is color coded')
      ggplotlyr(
      ggplot(data, aes(x=distinct, y=symmetry,
                       color=as.integer(.nna.), label=txt)) +
        scale_x_continuous(trans='sqrt', breaks=br, 
                           minor_breaks=mbr) +
        scale_color_gradientn(colors=viridis::viridis(10),
                              breaks=1 : length(levels(data$.nna.)),
                              labels=levels(data$.nna.)) +
        geom_point() + xlab('Number of Distinct Values') +
        ylab('Symmetry') +
        labs(caption=cap) +
        guides(color=guide_legend(title='NAs'))  )
        #        theme(legend.position='bottom')
      }
    r[, .nna. := cut2(NAs, g=12)]
    s <- split(r, r$type)
    g <- lapply(s, gg)
  }
  maketabs(g, initblank=TRUE)
  invisible()
}

asisOut <- function(...) {
  x <- list(...)
  x <- if(length(x) > 1) paste0(unlist(x)) else x[[1]]
  knitr::asis_output(x)
  }
