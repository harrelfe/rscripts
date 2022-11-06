derv <- list(  # mega list for all datasets
  # Derived variables for dataset A
  A = list(
    list(bmi = expression(703 * weight / height ^ 2),
     label='Body Mass Index',
     units='Kg/m^2')
  ),
  # Derived variables for dataset B
  B = list(
    list(x = expression(x1 * x2 / x3),
         label = 'X', drop=.q(x1, x2, x3)),
    list(y = expression(y1 / y2),
         label = 'Y', drop=.q(y1, y2))
  )
)
