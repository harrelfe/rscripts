# rscripts
R scripts with templates and examples of analyses using knitr with Rmarkdown or LaTeX

This is for use with the R function `getRs` (included in the `.Rprofile` file mentioned below) for auto-populating an RStudio script editor window with a script stored here.  Windows users must install `wget` in order to access github because it uses https instead of http.  Installation instructions as well as an attachment containing `.Rprofile` are [here](http://biostat.mc.vanderbilt.edu/RConfiguration).

The special file `contents.md` lists all the available files and short descriptions for them.  It should be edited whenever new scripts are added.  `contents.md` is used when `getRs` is called with no arguments, to give the user a list of available scripts and their descriptions.

The scripts included here use a template that makes the result part of a reproducible research process by documenting the versions of R and attached packages at the end of the report.  They make use of the `knitrSet` function also defined in the `.Rprofile`.  When running Rmarkdown, call `knitrSet(lang='markdown')`.  `knitrSet` gets rid of ## at the start of R output lines, and makes it easy to specify things like figure sizes in `knitr` chunk headers.  It also causes annoying messages such as those generated from attaching R packages to be put in a separate file `messages.txt` rather than in the report.

`html` output for some of the scripts is also included in this repository.  The file names are the same as the script names with `.Rmd` replaced with `.html`.
