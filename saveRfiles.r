save_fst <- function(d, filebase=as.character(substitute(d)),
                verbose=TRUE) {
    at <- keepHattrib(d)
    fd <- paste0(filebase, '.fst')
    fa <- paste0(filebase, '-attr.rds')
    saveRDS(at, fa, compress='xz')
    write_fst(d, fd, compress=100)
    if(verbose) cat('Data saved in', fd, 'and attributes saved in', fa, '\n')
    invisible()
}

retrieve_fst <- function(filebase, ...) {
    fd <- paste0(filebase, '.fst')
    fa <- paste0(filebase, '-attr.rds')
    d  <- read_fst(fd, ...)
    at <- readRDS(fa)
    invisible(restoreHattrib(d, at))
}
