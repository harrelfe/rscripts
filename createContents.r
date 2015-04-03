trim <- function(x) {
  x <- sub("[[:space:]]+$", "", x)
  sub('^[[:space:]]+', '', x)
}

files <- list.files(pattern='\\.Rmd$')
rfiles <- setdiff(list.files(pattern='\\.r$'), 'createContents.r')
Files <- c(files, rfiles)
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

f <- 'contents.md'
cat('', file=f, sep='')
cat('\nCategories Found:\n\n')
for(maj in Major) {
  cat(maj, '\n')
  cat('\n## ', upFirst(maj), '\n', sep='', file=f, append=TRUE)
  containingMaj <- sapply(major, function(x) any(x == maj))
  posminor <- unique(sort(unlist(minor[containingMaj])))
  for(mi in posminor) {
    cat('  ', mi, '\n')
    if(mi != '') cat('#### ', upFirst(mi), '\n', sep='', file=f, append=TRUE)
    cat('*File Name* | *Type* | *Description*\n---- | ---- | ----\n',
        file=f, append=TRUE)
    containingMin <- sapply(minor, function(x) any(x == mi))
    cat(containingMaj, '\n', containingMin, '\n',
        containingMaj & containingMin, '\n')
    for(g in which(containingMaj & containingMin))
      cat(Files[g], '|', type[g], '|', title[g], '\n', file=f, append=TRUE)
  }
}

