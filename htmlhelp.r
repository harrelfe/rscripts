##' Create html Files for Standalone Functions
##'
##' For each file ending with .r or .R in the current directory and having roxygen content in the first line, runs document:document on the file to create html documentation, and stores it in directory `dir`.  Symlinks are created for functions other than the function for which the help file is named.  Conditionally runs system command `mkindexd` to create an `index.html` file in `dir`.
##' @title htmlhelp
##' @param files list of files to process if not all
##' @param dir directory in which to place html files
##' @param index set to `FALSE` to suppress creation of `index.html`
##' @param dryrun set to `TRUE` to not write any files or create symbolic links
##' @return 
##' @author Frank Harrell
##' @md
htmlhelp <- function(files=list.files(pattern='.*\\.[Rr]$'),
                     dir='~/web/R/rscripts', index=TRUE, dryrun=FALSE) {
  require(document)
  cur <- getwd()
  for(f in files) {
    r <- readLines(f)[1]
    if(! (grepl("^##'", r) || grepl("^#'", r))) next
    cat('Processing', f, '\n')
    d <- document(f, check_package=FALSE)
    htmlpath <- d$html_path
    html     <- readLines(htmlpath)
    bh       <- basename(htmlpath)
    b        <- sub('\\.html', '', bh)
    g        <- paste(dir, bh, sep='/')
    if(dryrun) cat('Would write to file', g, '\n')
    else cat(html, file=g, sep='\n')
    funs <- html[grepl('DOCTYPE html.*<title>.*</title>', html)]
    funs <- sub('.*<title>R: (.*)</title>', '\\1', funs)
    nonbase <- setdiff(funs, b)
    if(length(nonbase)) {
      if(! dryrun) setwd(dir)
      for(fu in nonbase) {
        cmd <- paste0('ln -sf ', bh, ' ', fu, '.html')
        if(dryrun) cat(cmd, '\n') else system(cmd)
      }
      if(! dryrun) setwd(cur)
    }
  }
  if(! dryrun && index) {
    setwd(dir)
    system2('mkindexd')
    setwd(cur)
    }
}
