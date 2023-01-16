qproject <- .Options$qproject
if(! length(qproject)) qproject <- 'bbr'

ipacue <- function(x=NULL, marg=TRUE) {
  if(length(x)) apacue <<- x - 1
  else
    if(! exists('apacue')) apacue <<- 0
  apacue <<- apacue + 1
  i <- 1 + ((apacue -1) %% 26)   # recycle letters within chapter
  w <- paste0('<span style="color:red">', LETTERS[i], '</span>')
  if(marg) mrg(w) else w
  }

mrg <- function(..., color=NULL, method=NULL) 	{
  x <- c(...)

  if(! length(method)) method <- min(length(x), 2)
#  if(method == 1) x <- paste(x, collapse=' ')
  x <- paste(x, collapse=' ')
  if(length(color))
    x <- paste0('<span style="color:', color, '">', x, '</span>')
  
  w <- if(method == 1) paste0('[', x, ']{.aside}')
       else
         paste0('\n\n::: {.column-margin}\n', x, '\n\n:::\n\n')
#         paste0('\n::: {.column-margin}\n', x, '\n:::\n\n')

  if(length(.Options$debugmrg) && .Options$debugmrg)
    cat(w, file='/tmp/debugmrg.txt', append=TRUE)
  w
  }

linkit <- function(text, link)
  if(grepl('png$', text)) paste0('<a href="', link, '"><img src="',
                                 text, '" width="25"></a>') else
     paste0('[', text, '](', link, ')')
#paste0('[![](', text, ')](', link, '){ width=5px; }') else

sound <- function(x) {
  file <- paste0('http://hbiostat.org/audio/', qproject, '/', x, '.mp3')
  paste0('<audio controls><source src="', file,
         '" data-external="1" type="audio/mpeg"></source></audio>') 
  }

bmovie <- function(x) {
  file <- paste0('http://bit.ly/yt-', qproject, x)
  linkit('movie.png', file)
  }

movie <- function(x) linkit('movie.png', x)

ddisc <- function(x) {
  file <- paste0('http://bit.ly/datamethods-', qproject, x)
  linkit('discourse.png', file)
  }

disc <- function(x) {
  file <- paste0('https://discourse.datamethods.org/search?q="', x, '"')
  linkit('🅓', file)
  }

rmsdisc <- function(x='') {
  file <- paste0('https://discourse.datamethods.org/rms', x)
  linkit('discourse.png', file)
}

blog <- function(x) {
  file <- paste0('https://fharrell.com/post/', x)
  linkit('blog', file)
}

blogl <- function(x, title) {
  file <- paste0('https://fharrell.com/post/', x)
  linkit(title, file)
  }
  
bookref <- function(book, section) paste(book, section)
  
abd <- function(x, ...) bookref('ABD', x)   # remove ...
ems <- function(x, ...) {}
rms <- function(x, ...) bookref('RMS', x)  

quoteit <- function(x, y=NULL, marg=FALSE, bymarg=FALSE) {
  if(! marg && ! bymarg) {
  w <- paste0('\n\n::: {.quoteit}\n', x)
  if(length(y)) w <- paste(w, '<br><span style="color:black;font-style:normal;float:right;text-align:right;"> — ', y, '</span><br>')
  w <- paste0(w, '\n:::\n\n')
  return(w)
  }
  w <- if(length(y)) paste0(' — ', y) else ''
  if(marg) paste0('\n::: {.column-margin}\n', x, w, '\n:::\n\n')
  else if(bymarg)
    paste0('\n>> ', x, '\n\n::: {.column-margin}\n', y, '\n:::\n\n')
  else paste0('\n>> ', x, w, '\n\n')
  }

## Creates a dataset matching file names with anchors defined in all
## .qmd or .Rmd files in a directory
## Also looks in .html files for figure anchors
catalogAnchors <- function(dir='~/doc/bbr/md', type=c('qmd', 'Rmd')) {
  type <- match.arg(type)
  typ  <- paste0('\\.', type)
  files <- list.files(dir, pattern=typ)
  files <- setdiff(files, paste0('template.', type))
  w <- NULL
  for(f in files) {  # <a name="foo"></a>
    fb <- gsub(typ, '', f)
    z <- readLines(paste0(dir, '/', f))
    i <- grep('<a name=["\']+.*></a>', z)
    if(length(i)) {
      anchors <- gsub('.*<a name=["\']+(.*?)["\']></a>.*', '\\1', z[i])
      w <- rbind(w, data.frame(file=fb, anchor=anchors))
      }
  }
  files <- list.files(dir, pattern='\\.html')
  files <- setdiff(files, 'index.html')
  switch(type,
         Rmd = {r1 <- ' id=["\']fig:+.*?["\']>'
                r2 <- '.* id=["\']+fig:(.*?)["\']>.*'
                r3 <- 'fig:\\1' },
         qmd = {r1 <- ' id=["\']fig-+.*?["\']>'
                r2 <- '.* id=["\']+fig-(.*?)["\']>.*'
                r3 <- 'fig-\\1' }
         )
  for(f in files) {  # (space)id="fig:foo"> or fig-foo
    fb <- gsub('\\.html', '', f)
    z  <- readLines(paste0(dir, '/', f))
    i  <- grep(r1, z)
    if(length(i)) {
      anchors <- gsub(r2, r3, z[i])
      w <- rbind(w, data.frame(file=fb, anchor=anchors))
    }
  }
  w
}


## Creates <a href="...">here</a> for anchor label used for BBR
## Set self=TRUE if referring to an anchor within the same html document
anchorLoc <- function(label, place='here', self=FALSE) {
  locs        <- anchorLocs$file
  names(locs) <- anchorLocs$anchor
##  locs <- c(
##    'sec:r-import'      = 'r',
##    'reg-val'           = 'reg',
##    'reg-choose-val'    = 'reg',
##    'sec:repro-software'= 'repro'
##  )
  if(self) return(paste0('<a href="#', label, '">', place, '</a>'))

  if(label %nin% names(locs)) {
    warning(paste('Label', label, 'not defined'))
    return('(TBD)')
  }

  file <- paste0(locs[label], '.html#', label)
  paste0('<a href="', file, '">', place, '</a>')
}

## multmarg <- function(...) htmltools::HTML(
##      paste0('<p>', paste(..., collapse='<br>'), '</p>'))


ishtml  <- knitr::is_html_output()
islatex <- knitr::is_latex_output()

# Create pop-up for how to use hypothes.is
hypcomment <- '<a href="https://hbiostat.org/comment.html"
  target="popup"
	onclick="window.open(\'https://hbiostat.org/comment.html\', \'popup\',
     \'width=450,height=600\'); return false;">
	<small>Comments</small>
</a>'
  
disccom <- function(topic)
  if(! ishtml) invisible() else paste0('[[![](https://hbiostat.org/img/discourse.png)](https://discourse.datamethods.org/t/', topic, ')<br>',
                                       hypcomment, ']{.aside}<br><br>')


mNote <- if(ishtml) '.column-margin' else '.callout-note appearance="minimal"'
