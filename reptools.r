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
##' @param opts optional list of chunk options, e.g. `list(fig.width=6, fig.cap="This is a caption")`.  See <https://yihui.org/knitr/options> for a complete list of options.
##' @param results format of results, default is `'asis'`.  May specify `results='markup'`.
##' @param callout an optional Quarto callout to include after `#|` after the chunk header that affects how the result appears, e.g. `callout='column: margin'`
##' @param h, w optional height and width to place after the chunk header after `#|`
##' @return character vector 
##' @author Frank Harrell
makecodechunk <- function(cmd, opts=NULL, results='asis', lang='r',
                          callout=NULL, h=NULL, w=NULL) {
  if(! length(cmd) || (is.character(cmd) && length(cmd) == 1 &&
            cmd %in% c('', ' ', "` `"))) return('')

  r <- paste0('results="', results, '"')
  if(length(opts))
    for(oname in names(opts)) {
      op <- opts[[oname]]
      if(is.character(op)) op <- paste0('"', op, '"')
      r <- paste0(r, ',', oname, '=', op)
      }
  
  if(! exists('.chunknumber.')) .chunknumber. <<- 0
  .chunknumber. <<- .chunknumber. + 1
  cname <- paste0('chnk', .chunknumber.)
  if(length(callout)) callout <- paste('#|', callout)
  if(length(h)) h <- paste('#| fig.height:', h)
  if(length(w)) w <- paste('#| fig.width:', w)
  c('',
    if(lang == 'r') paste0('```{r ', cname, ',', r, ',echo=FALSE}')
    else            paste0('```{', lang, ' ', cname, '}'),
    callout, cmd, h, w, '```', '')
    }

##' General Case Handling of Quarto Callouts
##'
##' This function generates and optionally runs markdown/R code that runs Quarto callouts such as collapsible notes or marginal notes.
##' @title makecallout
##' @param x object to print (if `type='print'`), or one or more formulas whose right hand sides are to be run.  Left side provides labels if needed by the particular callout, and if `raw` is included on the right side any R code chunk run will have `results='asis'` in the chunk header.
##' @param callout character string giving the Quarto callout
##' @param label character string label if needed and if not obtained from the left side of a formula
##' @param type defaults to `'print'` to print an object.  Set to `'run'` to run a chunk or `'cat'` to use `cat()` to render.
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
           if(type == 'cat') x
         else
           makecodechunk(x, results=results)
  k <- c(k, res, if(close) c(':::', ''))
  list(k=k, now=now, type=type)
  }
  .z. <- build(...)
  .k. <- .z.$k
  if(.z.$now) {
    switch(.z.$type,
           print = cat(.k., sep='\n'),  # already print()'d
           cat   = cat(.k., sep='\n'),
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
                      type=c('print', 'run', 'cat'),
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
makecolmarg <- function(x, type=c('print', 'run', 'cat'), ...) {
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
##' until one of the other tabs is clicked.  Multiple commands can be run in one chunk by including multiple right hand terms in a formula.  A chunk can be marked for producing raw output by including a term `raw` somewhere in the formula's right side.  If can be marked for constructing a label and caption by including `+ caption(caption string, label string)`.  The tab number is appended to the label string, and if the label is not provided `baselabel` will be used.
##' @title maketabs
##' @param ... a series of formulas or a single named list.  For formulas the left side is the tab label (if multiple words or other illegal R expressions enclose in backticks) and the right hand side has expressions to evaluate during chunk execution, plus optional `raw`, `caption`, and `fig.size` options.
##' @param wide 
##' @param initblank
##' @param baselabel a one-word character string that provides the base name of `label`s for tabs with figure captions.  The sequential tab number is appended to `baselabel` to obtain the full figure label.  If using formulas the figure label may instead come from `caption(.., label)`. If not specified it is taken to be the name of the current chunk with `fig-` prepended.
##' @param cap applies to the non-formula use of `maketabs` and is an integer vector specifying which tabs are to be given figure labels and captions.
##' @param basecap a single character string providing the base text for captions if `cap` is specified.
##' @return 
##' @author Frank Harrell
# See https://stackoverflow.com/questions/42631642
maketabs <- function(..., wide=FALSE, initblank=FALSE,
                     baselabel=NULL, cap=NULL, basecap=NULL, debug=FALSE) {

  ## Put caption() and fig.size() in parent environment so they can
  ## be executed in that environment so that their argument values
  ## may be found there
  
  en <- parent.frame()
  assign(envir = en, 'caption',
         function(cap, label=NULL) list(label=label, cap=cap)  )
  assign(envir = en, 'fig.size',
         function(width=NULL, height=NULL, column=NULL)
           list(width=width, height=height, column=column)     )

  
  fs <- list(...)
  if(length(fs) == 1 && 'formula' %nin% class(fs[[1]])) {
    fs <- fs[[1]]   # undo list(...) and get to 1st arg to maketabs
    .fs. <<- fs
  }

  if(! length(baselabel))
    baselabel <- knitr::opts_current$get('label')
  else if(baselabel == 'none') baselabel <- NULL
  if(length(baselabel) && ! grepl('^fig-', baselabel))
      baselabel <- paste0('fig-', baselabel)
  
    yaml   <- paste0('.panel-tabset', if(wide) ' .column-page')

    k <- c('', paste0('::: {', yaml, '}'), '')
    if(initblank) k <- c(k, '', '##   ', '')

    for(i in 1 : length(fs)) {
      label <- baselabel
      capt  <- NULL
      size  <- NULL
      f <- fs[[i]]
      isform <- FALSE
      if('formula' %in% class(f)) {
        isform <- TRUE
        capt   <- NULL
        v      <- as.character(attr(terms(f), 'variables'))[-1]
        y      <- v[1]   # left-hand side
        y   <- gsub('`', '', y)
        x   <- v[-1]  # right-hand side
        raw       <- 'raw' %in% x
        if(raw) x <- setdiff(x, 'raw')
        ## process caption(..., ...)
        jc <- grep('caption\\(', x)
        if(length(jc)) {
          capt <- eval(parse(text=x[jc]), en)
          if(length(capt$label)) label <- capt$label
          capt   <- capt$cap
          x <- x[- jc]
        }
        ## process fig.size(...)
        sz <- grep('fig.size\\(', x)
        if(length(sz)) {
          siz <- eval(parse(text=x[sz]), en)
          if(length(siz$width))
            size <- paste('fig-width:', siz$width)
          if(length(siz$height))
            size <- c(size, paste('fig-height:', siz$height))
          if(length(siz$column))
            size <- c(size, paste('column:', siz$column))
          x <- x[- sz]
          }
      } else {
        raw  <- FALSE
        y    <- names(fs)[i]
        x    <- paste0('.fs.[[', i, ']]')
        if(i %in% cap) capt <- basecap
      }
      r <- paste0('results="',
                  if(raw) 'markup' else 'asis',
                  '"')
      callout <- NULL
      if(length(label) && length(capt)) {
        lab <- paste0(label, i)
        callout <- c(paste0('label: ', lab),
                     paste0('fig-cap: "',  capt, '"'))
        addCap(lab, capt)
      }
      if(length(size)) callout <- c(callout, size)
      
      k <- c(k, '', paste('##', y), '',
             makecodechunk(x, callout=callout,
                           results=if(raw) 'markup' else 'asis'))
    }
    k <- c(k, ':::', '')

  if(debug) cat(k, sep='\n', file='/tmp/z', append=TRUE)
  cat(knitr::knit(text=k, quiet=TRUE))
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


missChk <- function(data, use=NULL, exclude=NULL,
                    type=c('report', 'seq'),
                    maxpat=15, maxcomb=25, excl1pat=TRUE,
                    sortpatterns=TRUE,
                    prednmiss=FALSE, omitpred=NULL,
                    baselabel=NULL, ...) {

  type     <- match.arg(type)
  cargs    <- list(...)
  namedata <- deparse(substitute(data))
  prtype <- .Options$prType
  
  
  d    <- copy(data)
  setDT(d)
  if(length(use)) {
    if(inherits(use, 'formula')) use <- all.vars(use)
    d <- d[, ..use]
  }
  if(length(exclude)) {
    if(inherits(exclude, 'formula')) exclude <- all.vars(exclude)
    use <- setdiff(names(d), exclude)
    d <- d[, ..use]
  }
  
  p <- ncol(d)

  ismiss <- function(x)
    if(is.character(x)) is.na(x) | trimws(x) == '' else is.na(x)

  ## Replace each variable with missingness indicator
  di <- d[, lapply(.SD, ismiss)]
  
  ## Hierarchical exclusions

  exc <- do.call('seqFreq', c(di, list(noneNA=TRUE)))
  if(type == 'seq') return(exc)

  na.per.var <- apply(di, 2, sum)
  na.per.obs <- apply(di, 1, sum)

  if(all(na.per.var == 0)) 
    return(asisOut('No NAs on any of the',
                   p, ' variables examined.'))

  surrq <- function(x) paste0('`', x, '`')

  vmiss <- names(na.per.var)[na.per.var > 0]
  dm    <- d[, ..vmiss]
  pm    <- length(vmiss)

  cat('\n', p - pm, 'variables have no NAs and', pm,
      'variables have NAs\n\n')

  cat(namedata, ' has ', nrow(d), ' observations (', sum(na.per.obs == 0),
              ' complete) and ', ncol(d), ' variables (', sum(na.per.var == 0),
              ' complete)\n', sep='')
  
  if(sum(na.per.var) > 0) {
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
               caption='Frequency distribution of number of incomplete variables per observation'))
  }
  
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
    cap <- if(i == 1) paste0('+ caption("Missing data patterns in `',
                             namedata, '`"',
                             if(length(baselabel))
                               paste0(', "', baselabel, '"'),
                             ')')
    lab <- surrq(ptypes[i])
    f <- if(i < 5) paste0(lab, ' ~ ',
                          'naplot(.naclus., which="', names(ptypes[i]),
                          '")', cap)
         else
           paste0(lab, ' ~ ', 'plot(.naclus., abbrev=', abb, ')')
    tabs[[i]] <- as.formula(f)
  }

  ## Add tab for sequential NA exclusions
  .seqmisstab. <<- table(exc)
  
  tabs <- c(tabs, Sequential ~
                    kabl(.seqmisstab.,
                         caption='Sequential frequency-ordered exclusions due to NAs'))
  dm <- dm[, lapply(.SD, is.na)]
  ## Produce combination plot for the right number of variables with NAs
  if(pm <= maxcomb) {
    .combplotp. <<- do.call('combplotp', c(list(data=dm, maxcomb=maxcomb), cargs))
     tabs <- c(tabs,
               `NA combinations` ~ .combplotp.)
  }

  if(prednmiss && (pm < p) && any(na.per.var == 0)) {
    ## Predict using ordinal regression the number of missing variables
    ## from all the non-missing variables
    ## Compute the number of missing variables per observation
    preds <- names(na.per.var)[na.per.var == 0]
    if(length(omitpred)) {
      omitv <- if(is.character(omitpred)) omitpred
               else
                 all.vars(omitpred)
      preds <- setdiff(preds, omitv)
      }
    form <- paste('na.per.obs ~', paste(preds, collapse=' + '))
    f <- rms::lrm(as.formula(form), data=d)
    if(f$fail)
      cat('prednmiss=TRUE led to failure of ordinal model to fit\n\n')
    else {
      .misschkanova. <<- anova(f)
      .misschkfit.   <<- f
      options(prType='html')
      tabs <- c(tabs,
                `Predicting # NAs per obs` ~ print(.misschkfit., coefs=FALSE) +
                                             plot(.misschkanova.))
      }
  }

  do.call(maketabs, c(list(initblank=TRUE, baselabel=baselabel),
                      list(tabs)))
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


makemermaid <- function(.object., ..., callout=NULL, file=NULL) {
  x <- strsplit(.object., '\n')[[1]]
  code <- makecodechunk(x, lang='mermaid', callout=callout)
  ki <- knitr::knit_expand
  etext <- do.call('ki', c(list(text=code), list(...)))
  if(length(file)) cat(etext, sep='\n', file=file)
  cat(knitr::knit(text=etext, quiet=TRUE))
  invisible()
}


vClus <- function(d, exclude=NULL, corrmatrix=FALSE,
                  fracmiss=0.2, maxlevels=10, minprev=0.05,
                  horiz=FALSE, label='fig-varclus', print=TRUE) {
  w <- as.data.frame(d)  # needed by dataframeReduce
  if(length(exclude)) w <- w[setdiff(names(w), exclude)]
  w <- dataframeReduce(w, fracmiss=fracmiss, maxlevels=maxlevels,
                       minprev=minprev, print=FALSE)
  if(print) print(kabl(attr(w, 'info'),
                       caption='Variables removed or modified'))
  
  form <- as.formula(paste('~', paste(names(w), collapse=' + ')))
  v <- varclus(form, data=w)
  if(! corrmatrix) {
    if(horiz) plot(as.dendrogram(v$hclust), horiz=TRUE)
    else plot(v)
    return()
  }
  .varclus. <<- v
  rho <- varclus(form, data=w, trans='none')$sim
  .varclus.gg. <<- plotCorrM(rho, xangle=90)[[1]]
  cap <- 'Spearman rank correlation matrix.  Positive correlations are blue and negative are red.'
  form1 <- `Correlation Matrix` ~ .varclus.gg. + caption(cap, label=label) +
    fig.size(width=9.25, height=8.5)
  form2 <- `Variable Clustering` ~
    plot(as.dendrogram(.varclus.$hclus), horiz=TRUE) +
    fig.size(height=4.5, width=7.5)
  form3 <- `Variable Clustering` ~ plot(.varclus.) +
    fig.size(height=5.5, width=7.5)
  if(horiz) maketabs(form1, form2, initblank=TRUE)
else
            maketabs(form1, form3, initblank=TRUE)
}

dataOverview <- function(d, d2=NULL, id=NULL,
                         plot=c('scatter', 'dot', 'none'),
                         pr=nvar <= 50, which=1, dec=3, baselabel=NULL) {
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

  ## From rmsb package, augmented to handle dates/times:
  distSym <- function(x, prob=0.9, na.rm=FALSE) {
    if(na.rm) x <- x[! is.na(x)]
    x <- unclass(x) 
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

  ismiss <- function(x)
    if(is.character(x)) is.na(x) | trimws(x) == '' else is.na(x) 
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
      lnna <- levels(data$.nna.)
      ggplotlyr(
        if(any(trimws(as.character(data$.nna.)) != '0'))
          ggplot(data, aes(x=distinct, y=symmetry,
                       color=as.integer(.nna.), label=txt)) +
          scale_x_continuous(trans='sqrt', breaks=br, 
                           minor_breaks=mbr) +
          scale_color_gradientn(colors=viridis::viridis(min(10, length(lnna))),
                              breaks=1 : length(lnna),
                              labels=lnna) +
          geom_point() + xlab('Number of Distinct Values') +
          ylab('Symmetry') +
          labs(caption=cap) +
          guides(color=guide_legend(title='NAs'))
        else
          ggplot(data, aes(x=distinct, y=symmetry,
                       label=txt)) +
          scale_x_continuous(trans='sqrt', breaks=br, 
                           minor_breaks=mbr) +
          geom_point() + xlab('Number of Distinct Values') +
          ylab('Symmetry') +
          labs(caption=cap)
      )
        #        theme(legend.position='bottom')
      }
    r[, .nna. := cut2(NAs, g=12)]
    s <- split(r, r$type)
    g <- lapply(s, gg)
  }
  print(kabl(r[, paste(levels(.nna.), collapse=' ')],
             'Intervals of frequencies of NAs used for color-coding plots'))
  if(plot == 'scatter') {
    cap <- paste('Plot of the degree of symmetry of the distribution of a variable (value of 1.0 is most symmetric) vs. the number of distinct values of the variable.  Hover over a point to see the variable name and detailed characteristics.')
    maketabs(g, initblank=TRUE, basecap=cap, cap=1)
    }
  else maketabs(g, initblank=TRUE)
  invisible()
}

asisOut <- function(...) {
  x <- list(...)
  x <- if(length(x) > 1) paste0(unlist(x)) else x[[1]]
  knitr::asis_output(x)
  }


seqFreq <- function(..., labels=NULL, noneNA=FALSE) {
  d <- list(...)
  k <- length(d)
  if(length(labels)) nam <- labels
  else {
    s <- as.character(sys.call()[-1])[1 : k]
    nam <- names(d)
    if(! length(nam)) nam <- rep('', k)
    nam <- ifelse(nam == '', s, nam)
    }

  ispos <- function(x) {
    w <- if(is.logical(x)) x
    else if(is.numeric(x)) x > 0
    else tolower(as.character(x)) %in%
           c('present', 'yes', 'y', 'true', 'positive', 'pos', '+')
    w[is.na(x)] <- FALSE
    1L * w
  }

  names(d) <- nam
  x <- sapply(d, ispos)  # creates a matrix
  # Count number of positives per observation
  po   <- apply(x, 1, sum)
  cond <- c(sum(po == 0), tabulate(po, nbins=k))

  j <- integer(k)
  w <- rep(0, nrow(x))
  for(i in 1 : k) {
    freqs <- apply(x, 2, sum)
    if(all(freqs == 0)) break
    imax  <- which.max(freqs)
    j[i]  <- imax
    w <- ifelse(w == 0 & x[, imax], imax, w)
    x[x[, imax] == 1, ] <- 0
  }
j <- j[j != 0]
x <- if(noneNA) factor(w, j, nam[j]) else
                factor(w, c(0, j), c('none', nam[j]))
attr(x, 'obs.per.numcond') <- cond
x
}

## Test
if(FALSE) {
set.seed(1)
x1 <- runif(100)
x2 <- sample(letters[1:5], 100, replace=TRUE)
x3 <- sample(LETTERS[1:5], 100, replace=TRUE)
sum(x1 > .5); sum(x2 == 'a'); sum(x3 == 'A') # 48 21 24
sum(x1 <= .5 & x2 == 'a')             # 10
sum(x1 <= .5 & x3 == 'A')             # 11
sum(x1 <= .5 & x3 != 'A' & x2 == 'a') # 10
sum(x1 <= .5 & x2 != 'a' & x3 != 'A') # 31
w <- seqFreq(bigx1=x1 > .5, x2 == 'a', x3 == 'A')
table(w)
attributes(w)
}


meltData <- function(formula, data, vnames=c('labels', 'names')) {
  vnames <- match.arg(vnames)
  s <- copy(data)
  if(! is.data.table(s)) setDT(s)

  v <- all.vars(formula)
  y <- v[1]
  x <- v[-1]
  s <- s[, ..v]
  labs <- sapply(s, label)
  labs <- ifelse(labs == '', names(labs), labs)
  ## data.table wants all variables to be melted to have the same type
  s <- s[, lapply(.SD, as.double)]
  m <- melt(s, id.var=y)
  if(vnames == 'labels')
    m[, variable := labs[as.character(variable)]]
  m
}

ebpcomp <- function(x, qref=c(.5, .25, .75),
                    probs= c(.05, .125, .25, .375)) {
  w <- 1.

  x <- x[! is.na(x)]
  probs2 <- sort(c(probs, 1. - probs))

  m  <- length(probs)
  m2 <- length(probs2)
  j  <- c(1, sort(rep(2 : m2, 2)), - sort(- rep(1 : (m2 - 1),2)))
  z  <- c(sort(rep(probs, 2)),     - sort(- rep(probs[1 : (m - 1)], 2)))
  z  <- c(z, -z, probs[1])
  k  <- max(z)
  k  <- if(k > .48) .5 else k
  
  if(length(qref)) {
    size.qref <- pmin(qref, 1 - qref)
    size.qref[qref == .5] <- k
  }
  
  q  <- quantile(x, c(probs2, qref))
  qo <- quantile(x, c(0.01, 0.99))
  Segs <- if(length(qref)) list(x=q[-(1 : m2)],
                                y1= -w * size.qref / k,
                                y2=  w * size.qref / k)
  Lines <- list(x=q[j], y=w * z / k)
  Mean  <- list(x=mean(x), y=0, N=length(x))
  Extreme <- list(x=qo, y=0)
  return(list(segments=Segs, lines=Lines, points=Mean, points2=Extreme))
}

spikecomp <- function(x) {
  x <- x[! is.na(x)]
  n.unique <- length(unique(x))
  if(n.unique >= 100 ||
     (n.unique > 20 && 
      min(diff(sort(unique(x)))) < diff(range(x)) / 500)) {
    pret <- pretty(x, if(n.unique >= 100) 100 else 500)
    dist <- pret[2] - pret[1]
    r    <- range(pret)
    x     <- r[1] + dist * round((x - r[1]) / dist)
  }
  f <- table(x)
  x <- as.numeric(names(f))
  y <- unname(f / max(f))
  list(segments=list(x=x, y1=0, y2=y))
  }
  

addggLayers <- function(g, data,
                        type=c('ebp', 'spike'),
                        ylim=layer_scales(g)$y$get_limits(),
                        by='variable', value='value',
                        frac=0.065, mult=1., facet=NULL,
                        pos=c('bottom', 'top'), showN=TRUE) {
  type <- match.arg(type)
  pos  <- match.arg(pos)
  d    <- copy(data)
  setDT(d)
  if(by %nin% names(data)) {
    by <- '.by.'
    d[, .by. := rep('', .N)]
    }
  setnames(d, c(by, value), c('.by.', '.value.'))

  comp <- switch(type,
                 ebp   = ebpcomp,
                 spike = spikecomp)
  
  vars <- d[, unique(.by.)]
  r    <- list()
  for(v in vars) {
    x <- d[.by. == v, .value.]
    r[[v]] <- comp(x)
  }
  R <- list()
  for(n in names(r[[1]]))
    R[[n]] <- rbindlist(lapply(r, function(z) z[[n]]), idcol=by)

  ## Transform y from ebpcomp which has y in [-1, 1]
  ## -->  ylim[1] + (y + 1.) * diff(ylim) * frac / 2.
  b <- mult * diff(ylim) * frac / 2.

  switch(type,
         ebp   = switch(pos,
                        bottom = {a <- ylim[1] + b},
                        top    = {a <- ylim[2] - b} ),
         spike = switch(pos,
                        bottom = {a <- ylim[1]},
                        top    = {a <- ylim[2]; b <- -b} )          )

  for(geo in names(R)) {
    dat <- R[[geo]]
    if(length(facet)) dat[, names(facet) := facet]
    g <- g +
      switch(geo,
             lines    = geom_path(aes(x=x, y=a + b * y), alpha=I(0.6),
                                  data=dat),
             segments = geom_segment(aes(x=x, xend=x,
                                         y=a + b * y1, yend=a + b * y2),
                                     data=dat),
             points   = geom_point(aes(x=x, y=a + b * y),
                                       col=I('blue'), size=I(0.8), data=dat),
             points2  = geom_point(aes(x=x, y=a + b * y),
                                       size=I(0.2), alpha=I(0.4),
                                   data=dat) )
  }

  if(showN && 'points' %in% names(R) && 'N' %in% names(R$points) &&
     diff(range(R$points[, N])) > 0)
    g <- g + geom_text(aes(x=Inf, y=Inf, label=paste0('N=', N),
                           hjust=1, vjust=1, size=I(2)), data=R$points)

  g
  }


addCap <- function(label=NULL, cap=NULL, scap=NULL) {
  g <- function(tag1, tag2) {
    r <- knitr::opts_current$get(tag1)
    if(! length(r)) r <- knitr::opts_current$get(tag2)
    r
    }
  h <- function() {
    lab <- g('label')
    if(length(lab) && ! grepl('^fig-', lab)) lab <- paste0('fig-', lab)
    lab
    }
  if(! length(label)) label <- h()
  deb <- .Options$debugaddCap; deb <- length(deb) && deb
  if(deb) cat('label:', label, '\n', file='/tmp/z', append=TRUE)
  if(! length(label))              return(invisible(list(NULL, NULL, NULL)))
  if(is.logical(label) && ! label) return(invisible(list(NULL, NULL, NULL)))
  if(! length(cap))  cap  <- g('fig.cap',  'cap')
  if(! length(scap)) scap <- g('fig.scap', 'scap')
  if(! length(cap) && length(scap))  cap  <- scap
  if(! length(scap) && length(cap))  scap <- cap
  if(! exists('.captions.')) .captions. <<- NULL
  info <- data.frame(label=label, cap=cap, scap=scap)
  if(deb) prn(info, fi='/tmp/z')
  if(! length(.captions.) || label %nin% .captions.$label)
    .captions. <<- rbind(.captions., info)
  invisible(list(label=label, cap=cap, scap=scap))
}

saveCap <- function(basename)
  if(exists('.captions.'))
    saveRDS(.captions., file=paste0(basename, '-captions.rds'), compress='xz')
                                
printCap <- function(book=FALSE) {
  if(book) {
    files <- list.files(pattern='.*-captions.rds')
    .captions. <- NULL
    for(f in files) .captions. <- rbind(.captions., readRDS(f))
    }
  cap <- .captions.[c('label', 'scap')]
  cap$label <- paste0('@', cap$label)
  names(cap) <- c('Figure', 'Short Caption')
  if(book) knitr::kable(cap, row.names=FALSE)
  else     knitr::kable(cap, row.names=FALSE, format='html')
}

hookaddcap <- function(loc=NULL) {
  cf <- function(before, options, envir) {
    if(! before) return()
    label   <- knitr::opts_current$get('label')
    cap     <- options$fig.cap
    if(! length(cap)) cap <- options$cap
    if(length(cap) && is.call(cap))   cap <- eval(cap)
    ## Chunk produced a figure if label: fig- and fig-cap were
    if(! length(cap) || cap == '' || ! grepl('^fig-', label)) return()
    scap    <- options$fig.scap
    if(! length(scap)) scap <- options$scap
    if(length(scap) && is.call(scap)) scap <- eval(scap)
    if( ! length(scap) || scap == '') scap <- cap
    ## addCap will ignore an entry if .captions. already has an entry
    ## with the same label.  So if use manually put addCap() inside a chunk
    ## it is likely that a second entry will be avoided
    addCap(label, cap, scap)
    }
  knitr::knit_hooks$set(addcapfile=cf)
  knitr::opts_chunk$set(addcapfile=TRUE)
  if(length(loc)) knitr::opts_chunk$set(fig.cap.location=loc)
}


latestFile <- function(pattern, verbose=TRUE) {
  f <- list.files(pattern=pattern)
  if(length(f) == 1) return(f)
  if(length(f) == 0) {
    warning(paste('no files matching', pattern, 'were found'))
    return(character(0))
  }

  i <- file.info(f, extra_cols=FALSE)
  mtime <- i$mtime
  j <- order(mtime, decreasing=TRUE)[1]
  if(verbose) cat('\nLast modified file: ', f[j],
                  '  (of ', length(f), ' files)\n\n', sep='')
  f[j]
}


spar <-
  function(mar=if(!axes)
                 c(2.25+bot-.45*multi,2*(las==1)+2.2+left,
                   .5+top+.25*multi, .5+rt) else
                 c(3.25+bot-.45*multi,2*(las==1)+3.7+left,
                   .5+top+.25*multi, .5+rt),
                 lwd = if(multi)1 else 1.75,
                 mgp = if(!axes) mgp=c(.75, .1, 0) else
                 if(multi) c(1.5+0.83,    0.365-0.03, 0) else
                           c(2.4-.4+0.83, 0.475-0.03, 0),
                 tcl = if(multi)-0.25 else -0.4, xpd=FALSE, las=1,
                 bot=0, left=0, top=0, rt=0, ps=if(multi) 12 else 15,
                 mfrow=NULL, axes=TRUE, cex.lab=1.15, cex.axis=.8,
                 ...) {
  multi <- length(mfrow) > 0
  par(mar=mar, lwd=lwd, mgp=mgp, tcl=tcl, ps=ps, xpd=xpd,
      cex.lab=cex.lab, cex.axis=cex.axis, las=las, ...)
  if(multi) par(mfrow=mfrow)
}

## For illustrating in-line R codes so back ticks will appear
rwrap <- function(x) paste0('\\`r ', x, '\\`')

## The following doesn't work
## Usage for 80% font size: `r fontstyle(80)`    . . .  `r endfont`
# fontstyle <- function(size=NULL, color=NULL)
#   paste0('<p style="',
#   if(length(size))  paste0('font-size: ', size,  '%;'),
#   if(length(color)) paste0('color:',      color, ';'), '">')
# endfont  <- '</p>'



# Function to time an expression, printing the result of system.time in
# the right margin, and storing the result of system.time in .systime.
# in the global environment.
# Invisibly returns the result of the expression

timeMar <- function(x) {
  .systime. <<- system.time(.res. <- x)
  k <- capture.output(.systime.)
  # change trailing blank to s
  k[2] <- sub(' $', 's', k[2])
  k  <- c('```', k, '```')
  makecolmarg(k, type='cat')
  invisible(.res.)
}
