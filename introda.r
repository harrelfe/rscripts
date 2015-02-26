# This script shows what a basic analysis looks like in R
# We focus on the 2-sample t-test for comparing serum creatinine (SCr)
# in a small sample of males and females
# In this script, data are created in-line whereas in the vast
# majority of projects data will be read from a separate file

# First create and analyze the data in the simplest way: have a vector
# of SCr for males and a separate vector for females.  This is atypical
# because we usually want the data in a single object (a data frame)
# that would mark the sexes of the subjects with a 2-valued variable "sex"

females <- c(.8, .7, .95, 1.05, 1.03)
males   <- c(.9, .92, 1.25, 1.1, 1.08, 1.01)

mean(females)
mean(males)
sd(females)
sd(males)

# t.test is one of those few functions that allow different types
# of invocations.  For documentation type ?t.test at the command line or
# in RStudio click on Help in the bottom right window and search for t.test

t.test(females, males)
t.test(females, males, var.equal=TRUE) # traditional equal variance test

# More typically, create a data frame with two variables for the comparison

# Longhand: sex=c('female', 'female', 'female', ... 'male', 'male', ...)
dat <- data.frame(
  sex=c(rep('female', 5), rep('male', 6)),
  SCr=c(.8, .7, .95, 1.05, 1.03, .9, .92, 1.25, 1.1, 1.08, 1.01)
  )
dat
summary(dat)
# Use R formula syntax: SCr is modeled as a function of sex
# aggregate and t.test will subset on sex values
aggregate(SCr ~ sex, FUN=summary, data=dat)
t.test(SCr ~ sex, data=dat)
t.test(SCr ~ sex, data=dat, var.equal=TRUE)
with(dat, plot(sex, SCr))
# Note: functions that do not accept a data= argument need to know
# in which data frame the variables are found.  with(mydata, ...)
# says to look up variable values in mydata.





                  
