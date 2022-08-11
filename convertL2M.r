# Function to partially convert a LaTeX knitr file to Markdown
#
# Usage:
#   require(Hmisc)
#   getRs('convertL2M.r')   # gets from github
#   convertL2M('my.Rnw', 'my.Rmd')
#   Add internalize='directory.../shared.R' to move external R code
#   chunks to inside the new code chunks
#
# Must be done manually:
#  before running: make sure chunk headers are all on one input line
#  look for \ commands split over multiple lines, especially \emph{}
#  Fix all \item entries to occupy a single text line
#  Likewise for \footnote \chapter \section \subsection \subsubsection
#  If you define \label{} for a section heading make sure it is on the
#  same line as the \section{} or similar command
#  After running:
#  Edit lines marked with <!-- ?---> and look for closing } to edit
#  Edit translations from \begin{tabular} to markdown tables to fine tune
#  Edit \hspace to ~ within equations, edit \textbf
#  Put $$ on same line as equation
#  Add fig.align='left', lang='markdown' in knitrSet()
#  Change author names @ref ?
# See ~/r/rmarkdown/style.txt (insert in new .Rmd file) and bookfun.r
# This function uses translation regular expressions in
# https://github.com/harrelfe/rscripts/blob/master/convertL2M.rex

## Notes
##  If cache=TRUE figure captions may not be created
##  Set ipacue=FALSE to not generate `r ipacue()` at each
##  top-level \being{itemize,enumerate,description}

convertL2M <- function(file, out='', transtab=NULL, ipacue=TRUE, rsetup=TRUE,
  internalize=NULL) {

  if(! length(transtab)) 
    transtab <- 'https://raw.githubusercontent.com/harrelfe/rscripts/master/convertL2M.rex'
  
  trans <- readLines(transtab)
  n <- length(trans)
  if((n %% 3) != 0)
    stop('convertL2M.rex has # lines that is not a multiple of 3')
  typet <- trans[seq(1, n, by=3)]  # blank or w (warning)
  from  <- trans[seq(2, n, by=3)]
  to    <- trans[seq(3, n, by=3)]
  nt    <- length(to)
  
  w <- readLines(file)
  s <- trimws(w, which='right') # lines in file with no white space on right
  
  ## Intercept \cite{foo1,foo2,foo3} -> [@foo1; @foo2; @foo3]
  s <- gsub('~\\\\cite', '\\\\cite', s)
  j <- grep('\\cite[a-z]*\\{', s)
  if(any(j)) for(k in j) {
    x <- s[k]
    inside <- sub('.*\\\\cite[a-z]*\\{(.*?)\\}.*', '\\1', x)
    tto    <- sub(',', '; @', paste0('[@', inside, ']'))
    ex     <- paste0('\\\\cite[a-z]*\\{', inside, '\\}')
    x      <- sub(ex, tto, x)
    s[k]   <- x
    }
  
  ## Do main translations

  for(i in 1 : nt)
    if(typet[i] != 'w') s <- gsub(from[i], to[i], s)

  ## Delayed conversion of back ticks to handle regular LaTeX string quoting
  s <- gsub('BACKTICK', '`', s)
  
  ## For some reason neither \n nor \\n worked in convertL2M.dat
  s <- gsub('NEWLINE', '\n', s)
  st <- trimws(s)    # version of s with no white space on left either

  nestingi <- cumsum(st == '\\bi') - cumsum(st == '\\ei')
  nestinge <- cumsum(st == '\\be') - cumsum(st == '\\ee')
  n <- length(nestingi)

  if(nestingi[n] != 0) warning('itemize environment not closed')
  if(nestinge[n] != 0) warning('enumerate environment not closed')

  ## Compute degree of nesting ignoring whether iti was \bi or \be
  nestingie <- cumsum(st %in% c('\\bi', '\\be')) -
               cumsum(st %in% c('\\ei', '\\ee'))

  ## If ipacue=TRUE, for every top level \bi or \be insert a new
  ## line with `r ipacue()` at end of next line
  if(ipacue) {
    i <- which(st %in% c('\\bi', '\\be') & nestingie == 1)
    if(any(i)) for(j in i) s[j + 1] <- paste(s[j + 1], '`r ipacue()`')
  }

  ## For every \bi find line number of matching \ei
  ## For every line determine current environment

  openedi <- grep('\\\\bi$', st)
  closedi <- integer(0)
  for(i in openedi) {
    ns <- nestingi[i]
    ## match is first \ei with nesting level ns - 1
    poss <- (i + 1) : n
    closedi <- c(closedi, min(poss[nestingi[poss] == ns -1]))
  }
  ## Likewise for \be \ee
  openede <- grep('\\\\be$', st)
  closede <- integer(0)
  for(i in openede) {
    ns <- nestinge[i]
    ## match is first \ee with nesting level ns - 1
    poss <- (i + 1) : n
    closede <- c(closede, min(poss[nestinge[poss] == ns -1]))
  }

  ## Put all line numbers of openings into a single sorted vector
  ## along with indicator of i vs e
  opened <- c(openedi, openede)
  closed <- c(closedi, closede)
  ttype  <- c(rep('i', length(openedi)), rep('e', length(openede)))
  i      <- order(opened)
  opened <- opened[i]
  closed <- closed[i]
  ttype  <- ttype[i]

  item    <- grep('\\\\item', st)
  n       <- length(st)
  type    <- rep(' ', n)

  mx <- function(x, cond) if(any(cond)) max(x[cond]) else 0
  ## For each line containing \item find the last opened environment
  ## that was not closed between the opening and the \item
  for(j in item) {
    ## Find latest environment that was not closed before current line
    lasto <- mx(opened, opened < j & closed > j)
    if(lasto == 0) stop(paste('\\item encountered that is not after beginning of an itemize or enumerate environment', '\nj=', j, '\nitem[j]=', item[j]))
    type[j] <- ttype[which(opened == lasto)]
  }

  ## cat(paste(type, nestingi, nestinge, st), sep='\n', file='/tmp/a')
  if(any(type[item] == ' ')) {
    warning(paste('could not determine environment in lines',
                  paste(item[type[item] == ' '], collapse=' ')))
    cat('Offending lines including 1 line before and 1 after each:\n')
    lines <- item[type[item] == ' ']
    lines <- sort(c(lines - 1, lines, lines + 1))
    cat(st[lines], sep='\n')
  }
    
  nesti <- c('* ',  '    + ',      '       - ') 
  neste <- c('1. ', '       i) ', '         A. ')
  if(max(nestingi, nestinge) > 3)
    stop('does not handle itemized or enumerated lists deeper than 3 levels')
  z <- s
  for(i in item) {
    ## cat(i, type[i], nestinge[i], neste[nestinge[i]], '\n', z[i], '\n')
    if(type[i] != ' ')
      z[i] <-
        switch(type[i],
               i = gsub('\\\\item\\s+', nesti[nestingi[i]], trimws(z[i])),
               e = gsub('\\\\item\\s+', neste[nestinge[i]], trimws(z[i]))
               )
  }
  
  if(length(internalize)) {
    ext <- readLines(internalize)
    ext <- sub('# Figure \\(\\*.*\\*\\)', '', ext)
    ext <- ext[ext != '']
    le <- length(ext)
    ecn <- character(le)
    for(j in 1 : le) {
      cat(j, grepl('^##\\s@knitr', ext[j]), '\n')
      # Carry forward chunk names, extracted from ## @knitr chunkname
      if(grepl('^##\\s@knitr', ext[j]))
        ecn[j : le] <- sub('##\\s@knitr\\s(.*)', '\\1', ext[j])
    }
    ext <- split(ext, ecn)
    ext <- lapply(ext, function(x) x[-1])  # remove first entry
    }

  notchunkheader    <- ! grepl('^```\\{r', z)
  z[notchunkheader] <-   gsub('\\\\\\\\', '<br>', z[notchunkheader])
  
  # For every chunk header whose name is in internalize, expand the 
  # chunk to include lines in internalize
  
  if(length(internalize)) {
    for(j in 1 : length(z)) {
      if(grepl('^```\\{r', z[j])) {
        cname <- sub('^```\\{r\\s(*\\w*).*\\}', '\\1', z[j])
        if(cname %in% names(ext))
          z[j] <- paste(z[j], paste(ext[[cname]], collapse='\n'), sep='\n')
      }
    }
    # Convert z back into multiple lines
    z <- unlist(strsplit(z, '\n'))
  }

  ## For all array environments convert <br> back to \\
  ## Also replace {ccc} with correct number if needed

  ar  <- grep('\\\\begin\\{array', z)
  are <- grep('\\\\end\\{array', z)

  if(length(ar)) {
    for(i in ar) {
      ## Find end of environment
      j <- min(are[are >= i])
      z[i : j] <- gsub('<br>', '\\\\\\\\', z[i : j])
      ## Find first line of table and count # fields
      w <- z[i + 1]
      w <- substring(w, 1 : nchar(w), 1 : nchar(w))
      fields <- sum(w == '&') + 1
      ccs <- paste(rep('c', fields), collapse='')
      if(fields != 3)
        z[i] <- gsub('\\{ccc\\}', paste0('{', ccs, '}'), z[i])
    }
  }

  ## Do simple fix on tabular environments

  tab  <- grep('\\\\begin\\{tabular', z)
  tabe <- grep('\\\\end\\{tabular',   z)
  if(length(tab))
    for(i in tab) {
      z[i] <- ''
      ## Find end of environment
      j <- min(tabe[tabe >= i])
      z[i : j] <- gsub('\\\\\\\\', ' ', z[i : j])
      w <- z[(i + 1) : (j - 1)]
      fields <- length(strsplit(w[1], split='&')[[1]])
      w <- paste('|', gsub('&', '|', w), '|')
      w <- gsub('\\\\hline', '', w)
      w <- gsub('<br>', '', w)
      w <- gsub('~', '', w)
      w[1] <- paste0(w[1], '\n|', paste(rep('-----', fields), collapse='|'),
                     '|')
      z[(i + 1) : (j - 1)] <- w
      z[j] <- ''
    }

  ## Flag warnings but only outside array environment
  wrn <- integer(0)
  for(f in from[typet == 'w'])
    wrn <- c(wrn, grep(f, z))
  wrn <- setdiff(wrn,
                 c(grep('\\\\begin\\{array\\}', z),
                   grep('\\\\end\\{array\\}',   z)))

  if(length(wrn))
    for(i in 1 : length(wrn))
      if(any(ar >= wrn[i] & are <= wrn[i])) wrn[i] <- 0
  wrn <- setdiff(wrn, 0)
  
  if(length(wrn)) {
    wrn <- unique(wrn)
    z[wrn] <- paste(z[wrn], '<!-- ?--->')
  }

  ## Remove now unnecessary environment begin and ends
  z <- gsub('\\\\bi$', '', z)
  z <- gsub('\\\\be$', '', z)
  i <- c(grep('\\\\ei$', z), grep('\\\\ee$', z))
  ## if(length(i)) z <- z[- i]
  if(length(i)) z[i] <- ''

  ## Remove any blank line preceeding a > 1st level list
  p <- c('^    \\+ ',      '^        -',
         '^        i\\) ', '^        A\\. ')
  for(k in p) {
    i <- grep(k, z)
    if(length(i))
      for(j in i)
        if(trimws(z[j-1]) == '') z[j-1] <- '**DELETE**' 
  }
  z <- z[z != '**DELETE**']


  bt <- paste(rep("`", 3), collapse='')
  if(rsetup)
    z <-
      c("```{r include=FALSE}",
        "require(Hmisc)",
        "options(qproject='rms', prType='html')",
        "getRs('reptools.r')",
        "getRs('qbookfun.r')",
        "hookaddcap()",
        "knitr::set_alias(w = 'fig.width', h = 'fig.height', cap = 'fig.cap', scap ='fig.scap')",
        "```",
        "",
        z,
        "",
        "```{r echo=FALSE}",
        "# saveCap('')",
        "```")

  cat(z, sep='\n', file=out)
  invisible()
}
