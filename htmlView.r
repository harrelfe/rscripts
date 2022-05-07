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
