# Function for state tile thermometer plot, modified from
# https://gitlab.com/hrbrmstr/statebins

thermstate <- function(data, title=NULL, add=NULL,
                       coladd='midnightblue',
                       wvary='N' %in% names(data), labels=NULL,
                       trans=function(x) x, xi=0.3, yi=0.4) {

  ## rc was created from:
  ## rc <- statebins:::state_coords
  ## rc <- data.frame(state=rc$abbrev, x=rc$col, y=rc$row)
  ## dput(rc)

  reg <- 'region' %in% names(data)
  rc <- if(reg)
    data.frame(
      region = Cs(NE,  MW, S,   W),
      x      = c(  2,   1, 2,   0),
      y      = c(  1, 1.5, 2, 1.5)) else
    data.frame(
      state = Cs(AL, AK, AZ, AR, CA, CO, 
               CT, DE, DC, FL, GA, HI, ID, IL, IN, IA, KS, 
               KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, 
               NV, NH, NJ, NM, NY, NYC, NC, ND, OH, OK, 
               OR, PA, PR, RI, SC, SD, TN, TX, UT, VT, VI, 
               VA, WA, WV, WI, WY),
      x = c(8, 1, 3, 6, 2, 4, 11, 11, 
            10, 10, 9, 1, 3, 7, 7, 6, 5, 7, 6, 12, 10, 11, 8, 6, 7, 6, 4, 
            5, 3, 12, 10, 4, 10, 12, 8, 5, 8, 5, 2, 9, 12, 12, 9, 5, 7, 5, 
            3, 11, 12, 9, 2, 8, 7, 4),
      y = c(7, 7, 6, 6, 5, 5, 4, 
            5, 6, 8, 7, 8, 3, 3, 4, 4, 6, 5, 7, 1, 5, 3, 3, 
            3, 7, 5, 3, 5, 4, 2, 4, 6, 3, 3, 6, 3, 4, 7, 4, 
            4, 8, 4, 6, 4, 6, 8, 5, 2, 7, 5, 3, 5, 2, 4) )
  
  if(length(add)) rc <- rbind(rc, add)
  mvalue <- max(data$values, na.rm=TRUE)
  rc$y   <- max(rc$y) - rc$y + 1
  st     <- merge(rc, data, by=if(reg) 'region' else 'state',
                  all=TRUE, sort=TRUE)
  st$loc  <- if(reg) st$region else st$state
  locsadded <- if(length(add)) (if(reg) add$region else add$state)
  st$bcol <- ifelse(st$loc %in% locsadded, 'midnightblue', 'black')
  if(length(labels)) st$loc <- labels[st$loc]
  sub    <- paste0('Height scaled to maximum value of ', round(mvalue, 3))
  if(wvary) {
    mN   <- max(data$N, na.rm=TRUE)
    sub  <- paste0(sub, '\nWidth scaled to a maximum sample size of ',
                  mN)
    st$w <- xi * trans(st$N) / trans(mN)
  } else st$w <- xi

  sz <- if(reg) 2.4 else 1.65
  g <- ggplot() +
    geom_text(data=st, aes(x=x, y=y + yi + 0.1,
                           label=loc,
                           size=I(sz), color=I('midnightblue'))) +
    coord_equal() + labs(x=NULL, y=NULL, caption=paste0(title, '\n', sub)) +
    theme_void() +
    geom_rect(aes(xmin=x - w, xmax=x + w, ymin=y - yi,
                  ymax=y - yi + 2 * yi * values / mvalue,
                  color=I(bcol), fill=I(bcol), alpha=I(.7)),
              data=st) +
    geom_rect(aes(xmin=x - w, xmax=x + w, ymin=y - yi, ymax=y + yi,
                  color=I(bcol)),
              data=nomiss(st), fill=NA)
  if(any(is.na(st$values)))
     g <- g + geom_rect(aes(xmin=x - xi, xmax=x + xi,
                            ymin=y - yi, ymax=y + yi),
              data=st[is.na(st$values),], fill=NA, color='gray')
  g
}




## Obtained US Census regions from https://stackoverflow.com/questions/46066974

Cs <- Hmisc::Cs
USregions <- list(
  NE = Cs(CT,ME,MA,NH,RI,VT,NJ,NY,NYC,PA),
  MW = Cs(IN,IL,MI,OH,WI,IA,KS,MN,MO,NE,ND,SD),
  S  = Cs(DE,DC,FL,GA,MD,NC,SC,VA,WV,AL,KY,MS,TN,AR,LA,OK,TX),
  W  = Cs(AZ,CO,ID,NM,MT,UT,NV,WY,AK,CA,HI,OR,WA) )

## Given 2-letter state codes lookup their US Census regions NE MW S W
## Set abbrev=FALSE to use Northeast Midwest South West
## states not found keep their original values
lookupUSregion <- function(states, abbrev=TRUE) {
  regnames <- names(USregions)
  if(! abbrev) {
    regfull <- c(NE='Northeast', MW='Midwest', S='South', W='West')
    ## In case someone added new regions to USregions
    regnames <- ifelse(regnames %in% names(regfull),
                       regfull[regnames], regnames)
    }
  reg <- rep(regnames, sapply(USregions, length))
  names(reg) <- unlist(USregions)
  ifelse(states %in% names(reg), reg[states], states)
}
