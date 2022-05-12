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
