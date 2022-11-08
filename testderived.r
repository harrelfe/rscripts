# Define any complex derivations or repetitively used code as functions here
# These functions may be called as needed from code inside lists inside
# the derv object below
# Example: (not very complex but you'll get the idea)
# myfun <- function(x1, x2) x1 > 3 | (x1 < 1 & x2 < 0)
# ...
# list(x = expression(myfun(x1, x2)))
# myfun is defined first, when this entire file is source()'d


derv <- list(  # mega list for all datasets
  # Derived variables for dataset A
  A = list(
    list(bmi = expression(703 * weight / height ^ 2),
     label='Body Mass Index',
     units='Kg/m^2')
  ),
  
  # Derived variables for dataset B, and transformation of existing var z
  B = list(
    list(x = expression(x1 * x2 / x3),
         label = 'X', drop=.q(x1, x2, x3)),
    list(y = expression(y1 / y2),
         label = 'Y', drop=.q(y1, y2)),
    list(z = expression(z * 1000))
  )
)

# Another approach using upData() instead of runDeriveExpr()

updata <- list(
  # upData input for dataset A
  A = list(
    bmi = expression(703 * weight / height ^ 2),
    labels = c(bmi='Body Mass Index'),
    units  = c('Kg/m^2')
  ),
  B = list(
    # upData input for dataset B
    x = expression(x1 * x2 / x3),
    y = expression(y1 / y2),
    z = expression(z * 1000),
    labels = .q(x=X, y=Y),
    drop   = .q(x1, x2, x3, y1, y2)
  )
)

# To syntax-check code in this file, just source() it into any R session
# after running require(Hmisc) (to define .q if you use it)
