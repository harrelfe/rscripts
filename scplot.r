## Separate chunk plot
## Usage:
##
## scplot(id='chunkid')   # initialize output file scplot.Rmd
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

scplot <- function(command, cap=NULL, scap=NULL, w=5, h=4, id=NULL) {

  command <- as.character(sys.call()[2])

  if(length(id)) {
    cat('', sep='', file='scplot.Rmd')
    .iscplot.  <<- 0
    .idscplot. <<- id
    return(invisible())
    }

  .iscplot. <<- .iscplot. + 1

  cname <- paste0(.idscplot., .iscplot.)
  label   <- Hmisc::putHcap(cap, scap=scap, file=FALSE)

  k <- c(paste0('\n\n```{r ', cname, ',results="asis",echo=FALSE,fig.width=',
                w, ',fig.height=', h, '}\n'), paste0(command, '\n```\n\n'))
  cat(label, k, sep='', file='scplot.Rmd', append=TRUE)
  invisible()
}





# https://stackoverflow.com/questions/61620768
if(FALSE) chu <- function(g, title=NULL, h=5, w=5, id = NULL) {
  g_deparsed <- paste0(deparse(
    function() {g}
  ), collapse = '')
  
  if(length(title)) title <- paste0('### ', title, '\n\n')

  sub_chunk <- paste0(title, "```{r sub_chunk_", id, "_",
                      floor(runif(1) * 10000), 
    ", fig.height=", h, ", fig.width=", w, ", echo=FALSE}",
    "\n(", 
    g_deparsed
    , ")()",
  "\n`","``
  ")
  sub_chunk <- paste0(title, "```{r fig.height=", h, ", fig.width=", w, ", echo=FALSE}",
    "\n(", 
    g_deparsed
    , ")()",
  "\n`","``
  ")


  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE), sep='')
}
