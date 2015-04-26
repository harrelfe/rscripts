# title: various graphics utility functions including color specifications
# major: graphics

## Function to set nice defaults for par() for base graphics
spar <- function(mar=if(!axes)
                 c(2.25+bot-.45*multi,2*(las==1)+2+left,.5+top+.25*multi,
                   .5+rt) else
                 c(3.25+bot-.45*multi,2*(las==1)+3.5+left,.5+top+.25*multi,
                   .5+rt),
                 lwd = if(multi)1 else 1.75,
                 mgp = if(!axes) mgp=c(.75, .1, 0) else
                 if(multi) c(1.5, .365, 0) else c(2.4-.4, 0.475, 0),
                 tcl = if(multi)-0.25 else -0.4, xpd=FALSE, las=1,
                 bot=0, left=0, top=0, rt=0, ps=if(multi) 14 else 10,
                 mfrow=NULL, axes=TRUE, cex.lab=1.15, cex.axis=.8,
                 ...) {
  multi <- length(mfrow) > 0
  par(mar=mar, lwd=lwd, mgp=mgp, tcl=tcl, ps=ps, xpd=xpd,
      cex.lab=cex.lab, cex.axis=cex.axis, las=las, ...)
  if(multi) par(mfrow=mfrow)
}

## Function to layout multiple lattice graphs into a matrix with nc columns
pmlattice <- function(..., nc=2) {
  w <- list(...)
  if(!inherits(w[[1]], 'trellis')) w <- w[[1]]
  n <- length(w)
  nr <- ceiling(n / nc)
  row <- 1
  col <- 0
  for(i in 1 : n) {
    col <- col + 1
    if(col > nc) {
      col <- 1
      row <- row + 1
    }
    print(w[[i]], more=i < n, split=c(col, row,  nc, nr))
  }
}

## Function to payout multiple ggplot2 graphs onto a matrix with nc columns
## Returned object is a grid object suitable for print()'ing
## For this versions, component plots should have margins removed, e.g. by
## adding theme(plot.margin=grid::unit(rep(0,4), 'cm'))
## Replaced by Hmisc arrGrob function
## pmggplt <- function(..., nc=2)
##   gridExtra::arrangeGrob(..., ncol=nc)

## Alternate function - prints directly
pmggplot <- function(..., nc=2) {
  w <- list(...)
  if(! inherits(w[[1]], 'ggplot')) w <- w[[1]]
  n <- length(w)
  nr <- ceiling(n / nc)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nr, nc)))
  row <- 1
  col <- 0
  for(i in 1 : n) {
    col <- col + 1
    if(col > nc) {
      col <- 1
      row <- row + 1
    }
    print(w[[i]], vp=viewport(layout.pos.row=row, layout.pos.col=col))
  }
}

## Add a footnote to an already printed grid/lattice/ggplot2 plot
## See http://bigdata-analyst.com/best-way-to-add-a-footnote-to-a-plot-created-with-ggplot2.html
## Changed to expect size in mm
## For adding footnote to a grid object see internal footnote() in ggplot.Predict
prfootnote <- function(text, size=2.5, color=grey(.5)) {
  grid::pushViewport(viewport())
  grid::grid.text(label= text,
                  x   = unit(1,"npc") - unit(2, "mm"),
                  y   = unit(2, "mm"),
                  just= c("right", "bottom"),
                  gp  = gpar(fontsize=size/0.3527778, col=color))
  grid::popViewport()
}

# Good default colors according to colorbrewer2.org "qualitative" for
# Color-blind persons.  When more than 4 groups are requested, colors
# are not colorblind-safe but are print-friendly still
# Up to 8 groups are allowed.  groups should include black and gray.
# If grayscale is TRUE, colors are not used and different levels of gray
# scale and different line widths are used instead (and black and gray are
# ignored).  Specify rellwd=1 to not vary line widths, otherwise rellwd is
# the relative line width for thickest lines

colBrew <- function(groups, black=FALSE, gray=FALSE, grayscale=FALSE,
                    alpha=1, rellwd=NULL) {
  
  if(grayscale) {
    if(! length(rellwd)) rellwd <- 4
    x   <- seq(0, 0.85, length=groups)
    col <- gray(x, alpha=alpha)
    if(rellwd == 1) lwd <- rep(1, groups)
    else {
      s   <- x > 0.4   # lighter ones
      lwd <- c(rep(c(1, rellwd / 2.5), length=sum(! s)),
               rep(c(rellwd, rellwd / 2), length=sum(s)))
    }
    return(list(col=col, lwd=lwd))
  }
  groups <- groups - black - gray
  co <- switch(groups,
         "#1b9e77",
         c("#1b9e77", "#d95f02"),
         c("#1b9e77", "#d95f02", "#7570b3"),
         c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"),
         c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"),
         c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02"),
         c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02",
           "#a6761d"),
         c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02",
           "#a6761d", "#666666")
         )
  w <- rellwd
  if(! length(w)) w <- 2.5
  lwd <- if(w == 1) rep(1, groups) else
  switch(groups,
         1,
         c(1, 1),
         c(1, 1, 1),
         c(w, 1, w, 1),
         c(1, 1, 1, 1, 1),
         c(1, 1, 1, 1, w, w),
         c(1, 1, 1, 1, w, w, 1),
         c(1, 1, 1, 1, w, w, 1, 1))
  insert <- c(if(black) 'black', if(gray) gray(.75))
  ilwd   <- c(if(black) 1, if(gray) w)
  cols   <- c(insert, co)
  lwd    <- c(ilwd,   lwd)
  list(col=cols, lwd=lwd)
}

showcolBrew <- function(groups, ...) {
  cb <- colBrew(groups, ...)
  plot(NA, NA, xlim=c(0,1), ylim=c(0,1), type='n', axes=FALSE, xlab='', ylab='')
  for(i in 1 : groups) lines(0 : 1, runif(2), col=cb$col[i], lwd=cb$lwd[i])
}
   

## Add same transparency to a vector of colors

colTransp <- function(x, alpha=1) {
  y <- col2rgb(x) / 255
  z <- character(length(x))
  for(i in 1 : length(x))
    z[i] <- rgb(y[1,i], y[2,i], y[3,i], alpha=alpha)
  z
}

## Set lattice graphics line colors and types, and shading colors,
## using by default colBrew
## Shading colors are set to be more transparent (alpha argument)
## Set black=TRUE to insert black as the first color
## Set gray=TRUE to insert gray scale as second color (first if black is FALSE)
## Run show.settings() to show effects of the settings on superpose.*
latticeSet <- function(groups=8, col=NULL, alpha=.2,
                       lty=rep(1,groups),  lwd=rep(1,groups),
                       cex=rep(.8,groups), pch=rep(1,groups),
                       border=rep('black', groups), ...) {
  require(lattice)
  if(length(col)) cols <- col
  else {
    cb     <- colBrew(groups, ...)
    cols   <- cb$col
    lwd    <- cb$lwd
  }
  z <- trellis.par.get()
  z$plot.symbol$cex <- cex
  z$plot.symbol$col <- cols
  z$plot.symbol$pch <- pch
  z$superpose.symbol$cex <- cex
  z$superpose.symbol$col <- cols
  z$superpose.symbol$pch <- pch
  z$plot.line$col <- cols
  z$plot.line$lty <- lty
  z$plot.line$lwd <- lwd
  z$superpose.line$col <- cols
  z$superpose.line$lty <- lty
  z$superpose.line$lwd <- lwd
  z$superpose.polygon$alpha  <- rep(alpha, groups)
  z$superpose.polygon$col    <- colTransp(cols, alpha=alpha)
  z$superpose.polygon$border <- border
  trellis.par.set(z)
}

## Default color scheme for ggplot2 graphics
## See http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
gglinecol <- function(...)
  scale_color_manual(..., values=c("#000000", "#E69F00", "#56B4E9",
                            "#009E73", "#F0E442", "#0072B2", "#D55E00",
                            "#CC79A7"))
## Same without black
gglinecolnb <- function(...)
  scale_color_manual(..., values=c("#E69F00", "#56B4E9",
                            "#009E73", "#F0E442", "#0072B2", "#D55E00",
                            "#CC79A7"))

## Example: ggplot() + gglinecol()
ggfillcol <- function(...) scale_fill_manual(..., values=c("#999999",
   "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

## Call theme() with size specifications for legends in mm, and position
ltheme <- function(width=NULL, height=NULL, text=NULL, title=NULL, pos=NULL) {
  w <- list()
  mm2pts <- function(x) x / 0.3527778  ## convert mm to points
  if(length(width))  w$legend.key.width  <- grid::unit(width, 'mm')
  if(length(height)) w$legend.key.height <- grid::unit(height, 'mm')
  if(length(text))   w$legend.text       <- element_text(size=mm2pts(text))
  if(length(title))  w$legend.title      <- element_text(size=mm2pts(title))
  if(length(pos))    w$legend.position   <- pos
  do.call(theme, w)
}

# To change size of tick mark labels do ggplot( ) +
# theme(axis.text.x=element_text(size=8))   (size is in points)

# To get a log scale: ... + scale_y_log10()
