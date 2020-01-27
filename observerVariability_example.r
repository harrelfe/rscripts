

#######################################################################
#DATA TESTING
#######################################################################
library("Hmisc")
Sys.time()

source("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/AnalysisOfObserverVariability/observerVariability_3_3.R")


#dataset one.  Handout problem one
#Testing data one:
#		     Technician   t1          t2           t3
#            Patients
#               s1        (18,17,14)    (16,15,16)  (12,15,12)	     
#               s2        (20,21,20)     (14,12)      (13)
#               s3        (26,20,23)     (18,20)     (22,24)
#               s4          (19,17)       (16)       (21,23)  
#               s5          (28,24)      (32,29)     (29,25)
#		  
subject1 <- Cs(s1,s1,s1,s1,s1,s1,s1,s1,s1,
              s2,s2,s2,s2,s2,s2,
              s3,s3,s3,s3,s3,s3,s3,
              s4,s4,s4,s4,s4,
              s5,s5,s5,s5,s5,s5)
rater1   <- Cs(t1,t1,t1,t2,t2,t2,t3,t3,t3,
              t1,t1,t1,t2,t2,t3,
              t1,t1,t1,t2,t2,t3,t3,
              t1,t1,t2,t3,t3,
              t1,t1,t2,t2,t3,t3)
reading1 <- c(18,17,14,16,15,16,12,15,12,
             20,21,20,14,12,13,
             26,20,23,18,20,22,24,
             19,17,16,21,23, 
             28,24,32,29,29,25)
print("TESTING DATASET 1 -- intraVar() function")
tempIntra1 = intraVar(subject1,rater1,reading1)
print(tempIntra1)
summary(tempIntra1)
dump = bootStrap(tempIntra1, by = "subject", times = 1000, makeHistogram = TRUE)
dump = bootStrap(tempIntra1, by = "rater", times = 1000, makeHistogram = TRUE)
print("TESTING DATASET 1 -- interVar() function")
tempInter1 = interVar(subject1,rater1,reading1)
print(tempInter1)
summary(tempInter1)
dump = bootStrap(tempInter1, by = "subject", times = 1000, makeHistogram = TRUE)
dump = bootStrap(tempInter1, by = "rater", times = 1000, makeHistogram = TRUE)


#dataset two.  Example from Frank's code
#Testing data two:
#                subject      a1         a2          a3
#            raters
#               h1            (1)        (7)         (1)	     
#               h2            (3)        (3)         (4)
#               h3            (2)                    (8)
subject2 <- Cs(a1,a1,a1,a2,a2,a3,a3,a3)
rater2   <- Cs(h1,h2,h3,h1,h2,h1,h2,h3)
reading2 <- c(1,3,2,7,3,1,4,8)
print("TESTING DATASET 2 -- intraVar() function")
tempIntra2 = intraVar(subject2,rater2,reading2)
print(tempIntra2)
print("TESTING DATASET 2 -- interVar() function")
tempInter2 = interVar(subject2,rater2,reading2)
print(tempInter2)

#dataset three.  Handout example
#Testing data three:
#                   subject     1				
#		raters
#               A             (5, 7)
#               B             (8, 5)
#               C             (6, 7)
subject3 <- Cs(1,1,1,1,1,1)
rater3   <- Cs(A,B,C,A,B,C)
reading3 <- c(5,8,6,7,5,7)
print("TESTING DATASET 3 -- intraVar() function")
tempIntra3 = intraVar(subject3,rater3,reading3)
print(tempIntra3)
print("TESTING DATASET 3 -- interVar() function") 
tempInter3 = interVar(subject3,rater3,reading3)
print(tempInter3)

#dataset four.  Handout example
#Testing data three:
#                   subject     1               2	
#		raters
#               A             (5, 7)           (4,8)
#               B             (8, 5)            (5)
#               C             (6, 7)            (5,5)
subject4 <- Cs(1,1,1,1,1,1,2,2,2,2,2)
rater4   <- Cs(A,B,C,A,B,C,A,A,B,C,C)
reading4 <- c(5,8,6,7,5,7,4,8,5,5,5)
print("TESTING DATASET 4 -- intraVar() function")
tempIntra4 = intraVar(subject4,rater4,reading4)
print(tempIntra4)
print("TESTING DATASET 4 -- interVar() function") 
tempInter4 = interVar(subject4,rater4,reading4)
print(tempInter4)

#dataset testing 5
#testing interVar under "mulitSub" mode, with confidence as auxA variable

subject5 <- c(1,1,1,2,2,3,3,3)
rater5   <- c(1,2,3,1,2,1,2,3)
y5 <- c(1,3,2,7,3,1,4,8)
confidence5 <- c(100, 100, 20, 30, 50, 80, 70, 20)

interRstSpc = interVar(subject5, rater5,y5, dfun = "multiSub",auxA = confidence5)
print(interRstSpc)
summary(interRstSpc)
dump = bootStrap(interRstSpc, times = 1000, makeHistogram = TRUE)



