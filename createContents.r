method <- c('septables', 'onetable')[2]

trim <- function(x) {
  x <- sub("[[:space:]]+$", "", x)
  sub('^[[:space:]]+', '', x)
}

files <- list.files(pattern='\\.Rmd$')
rfiles <- setdiff(list.files(pattern='\\.r$'), 'createContents.r')
Files <- c(files, rfiles)
baseFiles <- sub('\\.Rmd$', '', sub('\\.r$', '', Files))
htmls     <- list.files(pattern='\\.html$')

n <- length(files)
m <- length(rfiles)
type <- c(rep('R Markdown', n), rep('R', m))
N <- n + m
title <- character(N)
major <- vector('list', N)
minor <- vector('list', N)
i <- 0
for(f in files) {
  i <- i + 1
  a <- readLines(f)
  ti <- a[grep('^title:', a)[1]]
  ti <- gsub('^title: "', '', ti)
  ti <- gsub('"$', '', trim(ti))
  title[i] <- ti
  maj <- a[grep('^major:', a)[1]]
  maj <- gsub('^major:', '', maj)
  major[[i]] <- trim(strsplit(maj, ';')[[1]])
  mi <- a[grep('^minor:', a)]
  if(length(mi)) mi <- gsub('^minor:', '', mi)
  minor[[i]] <- if(length(mi)) trim(strsplit(mi[1], ';')[[1]]) else ''
}

for(f in rfiles) {
  i <- i + 1
  a <- readLines(f)
  ti <- a[grep('^\\# title:', a)[1]]
  title[i] <- trim(sub('^# title:', '', ti))
  maj <- a[grep('^\\# major:', a)[1]]
  maj <- sub('^\\# major:', '', maj)
  major[[i]] <- trim(strsplit(maj, ';')[[1]])
  mi <- a[grep('^\\# minor:', a)]
  if(length(mi)) mi <- sub('^\\# minor:', '', mi)
  minor[[i]] <- if(length(mi)) trim(strsplit(mi[1], ';')[[1]]) else ''
}

Major <- unique(sort(unlist(major)))
Minor <- unique(sort(unlist(minor)))
upFirst <- greport:::upFirst
fwrap <- function(x, html=FALSE) {
  link <- if(html)'http://htmlpreview.github.io/?https://github.com/harrelfe/rscripts/blob/master/' else 'https://github.com/harrelfe/rscripts/blob/master/'
  nam <- if(html) 'report' else x
  paste('[', nam, '](', link, x, ')', sep='')
}
f <- 'contents.md'
cat('', file=f, sep='')
cat('\nCategories Found:\n\n')

if(method == 'onetable') cat('*Major Category* | *Minor Category* | *File Name* | *Type* | *Description*\n---- | ---- | ---- | ---- | ----\n',
     file=f)

lastmaj <- lastmin <- 'xxxxxx'
for(maj in Major) {
  cat(maj, '\n')
  if(method == 'septables')
    cat('\n## ', upFirst(maj), '\n', sep='', file=f, append=TRUE)
  containingMaj <- sapply(major, function(x) any(x == maj))
  posminor <- unique(sort(unlist(minor[containingMaj])))

  for(mi in posminor) {
    cat('  ', mi, '\n')
    if(method == 'septables') {
      if(mi != '') cat('#### ', upFirst(mi), '\n', sep='', file=f, append=TRUE)
      cat('*File Name* | *Type* | *Description*\n---- | ---- | ----\n',
          file=f, append=TRUE)
    }
    containingMin <- sapply(minor, function(x) any(x == mi))
      for(g in which(containingMaj & containingMin)) {
        htmlf <- paste(baseFiles[g], 'html', sep='.')
        tig <- title[g]
        if(htmlf %in% htmls)
          tig <- paste(tig, '; ', fwrap(htmlf, html=TRUE), sep='')
                      
        switch(method,
               septables=cat(fwrap(Files[g]), '|', type[g], '|', tig,
                 '\n', file=f, append=TRUE),
               onetable={
                 ma <- ifelse(maj == lastmaj, '"', maj)
                 m  <- ifelse(maj == lastmaj && mi  == lastmin, '"', mi)
                 cat('**', upFirst(ma), '**| ', upFirst(m), ' | ',
                     fwrap(Files[g]), ' | ',
                     type[g], ' | ', tig, '\n', file=f, append=TRUE, sep='')
               } )
        lastmaj <- maj
        lastmin <- mi
      }
  }
}

