# Functions to facilitate the use of the fst package
# for efficiently storing and retrieving data frames/tables
# by keeping Hmisc package attributes labels and units in
# a separate RDS file when running save_fst() and by re-attaching
# these attributes to variables when running retrieve_fst.
#
# d: a data frame/table
# filebase: base file name, default is name of argument d
# verbose: set to FALSE to make save_fst not print the names
#   of the files created
# ...: other arguments to read_fst, especially as.data.frame
# For retrieve_fst the d argument's value is not used; d is
# used to specify the base file name.  If you want to use a base
# name that is not the object d name, put the first argument
# to retrieve_fst in quotes.


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

retrieve_fst <- function(d, ...) {
    filebase <- as.character(substitute(d))
    fd <- paste0(filebase, '.fst')
    fa <- paste0(filebase, '-attr.rds')
    d  <- read_fst(fd, ...)
    at <- readRDS(fa)
    invisible(restoreHattrib(d, at))
}
