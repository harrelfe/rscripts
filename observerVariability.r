
#DEMO-VERSION 3-2
cat("YOU ARE RUNNING A DEMO VERSION 3_2 \n")
##############################################################
#Project Name:	intra-observer and inter-observer functions
#Vertion:		3.2 (testing)
#Author:		Frank Harrell, 
#                 Zhouwen Liu, zhouwen.liu@vanderbilt.edu
#			Department of Biostatistics, Vanderbilt University
#
# 
#
#Description:	calculate inter- and intra observer disagreement
#	
############################################################### 

###############################################################
#function Name:	is.wholeNumber
#check if it's a whole number
#input:
#       A numeric vector
#output:
#       A boolean vector
###############################################################
is.wholeNumber = function(dataVct)
{ 
    return(ceiling(dataVct) == floor(dataVct))
}

###############################################################
#function Name:	bootStrap
#	bootStrap wrap function
#input:
#     x:      dataset to run bootstrap
#     times:  number of sampling
#       
#output:
#       
###############################################################
bootStrap = function(...)
{
    UseMethod("bootStrap")
}

###############################################################
#function Name:	bootStrap
#	bootStrap default function
#input:
#     x:      dataset to run bootstrap
#     times:  number of sampling
#       
#output:
#       
###############################################################
bootStrap.default = function(x, times,
                     replace = TRUE, dfun = NULL, 
                     wgt = NULL, wgtfun = NULL,
                     makeHistogram = FALSE,
                     ...)
{ 
    #step 1.  define and check arguments
    #define a dfun. default mean()
    if(is.null(dfun))
    {dfun = mean}    
    #check if wgt exist. if yes, compare length with x and also check wgtfun
    #if not, keep them NULL
    if(!is.null(wgt))
    {
        if(length(wgt) == length(x))
        {
            if(is.null(wgtfun))
            {wgtfun = function(a,b){return(a*b)}}
        }
        else
        {
            cat("Error: x and wgt have different length.", "\n")
            return(NULL)
        }
    }
    else
    {wgtfun = function(a,b){return(a)}}

    #step 2.  calculation
    result = rep(NA, times);  #save the rsults
    for(i in 1: length(result))
    {
        bt = sample(wgtfun(x, wgt), length(x), replace = replace)
        result[i] = dfun(bt, ...)
    }

    if(!is.null(result) && makeHistogram == TRUE)
    {
        hist(result,...);
        box();
    }
    return(result)
}

###############################################################
#function Name:	bootStrap.variability
#	bootStrap function for variability class
#input:
#     obj:      a variability type object
#     times:  number of sampling
#       
#output:
#       a vector of numbers 
###############################################################
bootStrap.variability = function(obj, by = NULL, addWgt = FALSE, ...)
{
    rst = NULL;  #to store result
    if(obj$variabilityType == "Intra-Variability")
    {
        if(by == "subject")
        {
            x = obj$variabilityBySub$variability;   # get the x
            wgt = NULL; 
            if(addWgt == TRUE)
            {wgt = obj$variabilityBySub$N; }
            rst = bootStrap.default(x, wgt = wgt,...)
        }
        else if(by == "rater")
        {
            x = obj$variabilityByRater$variability;   # get the x
            wgt = NULL; 
            if(addWgt == TRUE)
            {wgt = obj$variabilityByRater$N; }
            rst = bootStrap.default(x, wgt = wgt, ...)
        }
        else
        {
            cat("Please specify 'by' argument", "\n")
        }
    }
    
    if(obj$variabilityType == "Inter-Variability")
    {
        if(by == "subject" && !is.null(obj$variabilityBySubject))
        {
            x = obj$variabilityBySubject$variability;   # get the x
            wgt = NULL; 
            if(addWgt == TRUE)
            {wgt = obj$variabilityBySubject$N; }
            rst = bootStrap.default(x, wgt = wgt,...)
        }
        else if(by == "rater" && !is.null(obj$variabilityByRaterPair))
        {
            x = obj$variabilityByRaterPair$variability;   # get the x
            wgt = NULL; 
            if(addWgt == TRUE)
            {wgt = obj$variabilityByRaterPair$N; }
            rst = bootStrap.default(x, wgt = wgt,...)
        }
        else if(by == "multi" || (is.null(obj$variabilityByRaterPair) && is.null(obj$variabilityBySubject)))
        {
            x = obj$pairwiseVariability$variability;   # get the x
            wgt = NULL; 
            if(addWgt == TRUE)
            {wgt = obj$pairwiseVariability$N; }
            rst = bootStrap.default(x, wgt = wgt,...)
        }       
        else
        {
            cat("No inter-variability by bootStrapping available\n")
        }
    }
    return(rst)
}

###############################################################
#function Name:	intraVar
#Calculate intra-observer difference 
#input:	
#	subject, subject list.
#	rater, rater list.
#	rating, the observed values by corresponding rater to corresponding subject.
#	The above three input arguments should have same length.  
#				
#	dfun, A function used to calculate disagreement.  Default = abs(A-B).
#	..., arguments used by the dfun if dfun defined by the users
#		
#output:
#     The function returns an "disagreement" object.  This object type is defined by following
#     members:
#     disagreementType, it can be either 'Intra-disagreement' or 'Inter-disagreement'.  
#                        In corrent function, it is defined as 'Intra-disagreement'
#     disagreementBySubRater, a dataframe with four fields: subject, rater, disagreement and N.
#     disagreementBySub, A dataframe with two fields: subject, disagreement.  
#     overallAvg, a vector contains the average of the disagreement 
#################################################################
intraVar = function(subject, rater, rating ,dfun = function(a,b){abs(a-b)}, ...)
{
    retVal3 = NULL;  #set defalt return val to NULL
    #checking input variables
    if(!(length(subject) == length(rater) && 
       length(subject) == length(rating) &&
       length(subject) != 0))
    {
        flag = FALSE;  #find error and set up error flag
        print("Error in input values: the length of input variables must be same and not 0.  Exit");
    }
    else
    {
        #make subject and rater as characters, if not.
        sub = as.character(subject);
        ra = as.character(rater);

        #get the interaction between sub and ra
        w <- interaction(sub, ra);
    
        #define pre-return temporary result variables
        rtnsub = rtnrater = rtndisagree = rtnN = c()
        #if found duplicates, intra-observer difference exist
        #calculate this value and store to use this to calculate retVal2 and return it;
   
        if(any(duplicated(w)))
        {
            uniqSub = unique(sub)
            uniqRa  = unique(ra)

            for(i in 1:length(uniqSub))
            {
                for(j in 1:length(uniqRa))
                {
                    tempRating = rating[(subject == uniqSub[i] & rater == uniqRa[j])]
                    size = length(tempRating) 
                    if(size > 1)
                    {
                        #get the difference
                        #step 1.  build two matrix, one by row, another by col. 
                        #step 2.  take difference between these two matrix.
                        #step 3.  take absolute value of the difference, 
                        #step 4.  get sum then divide by 2, put in variable difference
                        partA = matrix(tempRating, size, size)
                        partB = matrix(tempRating, size, size, byrow = TRUE)
                        partAB = dfun(partA, partB, ...)
                        difference = sum(partAB)/2
                        itemNum = sum (1: size-1)  #number of items in the valid matrix
                        rtnsub = c(rtnsub, uniqSub[i])
                        rtnrater = c(rtnrater, uniqRa[j])
                        rtndisagree = c(rtndisagree, difference)
                        rtnN = c(rtnN, itemNum)
                    }
                } #end of for /j
            }  #end of for/i

            rtnsub2 = rtndisagree2 = rtnN2 = c() 
            for(i in 1: length(uniqSub))
            {
                sumDiff = sum(rtndisagree[rtnsub == uniqSub[i] & is.na(rtndisagree) == FALSE], na.rm = TRUE);           
                sumNumCount = sum(rtnN[rtnsub == uniqSub[i] & is.na(rtndisagree) == FALSE], na.rm = TRUE);
                rtnsub2 = c(rtnsub2, uniqSub[i])
                rtndisagree2 = c(rtndisagree2, sumDiff/sumNumCount);
                rtnN2 = c(rtnN2, sumNumCount)
            }

            rtnrt3 = rtndisagree3 = rtnN3 = c() 
            for(i in 1: length(uniqRa))
            {
                sumDiff = sum(rtndisagree[rtnrater == uniqRa[i] & is.na(rtndisagree) == FALSE], na.rm = TRUE);           
                sumNumCount = sum(rtnN[rtnrater == uniqRa[i] & is.na(rtndisagree) == FALSE], na.rm = TRUE);
                rtnrt3 = c(rtnrt3, uniqRa[i])
                rtndisagree3 = c(rtndisagree3, sumDiff/sumNumCount);
                rtnN3 = c(rtnN3, sumNumCount)
            }

            retVal3 = structure(list(variabilityType = "Intra-Variability",
                                     variabilityBySubRater = data.frame(subject = rtnsub, 
                                                                         rater = rtnrater, 
                                                                         variability = rtndisagree, 
                                                                         N = rtnN), 
                                     variabilityBySub = data.frame(subject = rtnsub2, 
                                                                    variability = rtndisagree2,
                                                                    N = rtnN2), 
                                     variabilityByRater = data.frame(subject = rtnrt3, 
                                                                    variability = rtndisagree3,
                                                                    N = rtnN3)),
                                class = "variability") 
        }
        else
        {
            print("No intra-difference found")   
        }
    } #end of if/else
    return(retVal3)
}#end of function intraVar def.


###################################################
#function Name:	interVar
#Calculate inter-observer difference 
#input:	
#	subject, subject list.
#	rater, rater list.
#	rating, the observed values by corresponding rater to corresponding subject.
#     auxA, auxB, the control varibles, these are optional.
#	The above five input arguments should have same length.  
#				
#	dfun, A function used to calculate disagreement.  Default = "singleSub".  Another value is 
#           "multiSub".  The function also can be defined by the user.
#     singledfun, A function used to calculate disagreement between individual pair of ratings,
#                 default = abs(A-B)
#     multidfun, A function used to calculate corelation in "multiSub" mode.  Default = cor()
#	..., arguments used by the dfun, singledfun or multidfun if they are defined by the users
#		
#output:
#     The function returns an "disagreement" object.  This object type is defined by following
#     members:
#     disagreementType, it can be either 'Intra-disagreement' or 'Inter-disagreement'.  
#                        In corrent function, it is defined as 'Inter-disagreement'
#     pairwiseDisagreement, a dataframe with four fields: subject, rater, disagreement and N.
#     disagreementBySubject, A dataframe with two fields: subject, disagreement.  
#     overallAvg, a vector contains the average of the disagreement 
###################################################################
interVar = function(subject, rater, rating,
                     auxA = NULL,
                     dfun = "singleSub", 
                     singledfun = NULL,
                     multidfun = NULL,
                     ...)
{
    #stage 0.  define dfun functions
    singleSub = function(grprater1, grprater2, grpsub1, grpsub2,
                         grprating1, grprating2, grpauxA1, grpauxA2,...)
    {
        raterA = unique(grprater1);   #1
        raterB = unique(grprater2);   #5
        uniqSubA = unique(grpsub1);   #1-90
        uniqSubB = unique(grpsub2);   # 1-90

        rstsub = rstrater = rstrater2 = rstdiff = rstauxA1 = rstauxA2 = c()
        for(kA in 1:length(uniqSubA))
        {
            tempA = grprating1[grpsub1 == uniqSubA[kA]]
            tempB = grprating2[grpsub2 == uniqSubA[kA]]
            tempA1 = rep(tempA, each = length(tempB))
            tempB1 = rep(tempB, length(tempA))
            diff = singledfun(tempA1, tempB1, ...);
            diff2 = diff[!is.na(diff)];  #remove NA's

            if(length(grpauxA1) != 0)
            {
                auxAA = grpauxA1[grpsub1 == uniqSubA[kA]]
                auxAB = grpauxA2[grpsub2 == uniqSubA[kA]]
                auxAA1 = rep(auxAA, each = length(auxAB))
                auxAB1 = rep(auxAB, length(auxAA))
                auxAA1 = auxAA1[!is.na(diff)]
                auxAB1 = auxAB1[!is.na(diff)]
            }
            else
            {
                auxAA1 = rep(NA, length(diff2))
                auxAB1 = rep(NA, length(diff2))                
            }

            #now assemble:
            rstsub = c(rstsub, rep(uniqSubA[kA], length(diff2))) 
            rstrater = c(rstrater, rep(raterA, length(diff2)))
            rstrater2 = c(rstrater2, rep(raterB, length(diff2)))
            rstdiff = c(rstdiff, diff2)
            rstauxA1 = c(rstauxA1, auxAA1)
            rstauxA2 = c(rstauxA2, auxAB1)
        }
        data.frame(subject = rstsub, rater = rstrater, rater2 = rstrater2, 
                   diff = rstdiff,auxA1 = rstauxA1, auxA2 = rstauxA2)
    };#end of singuleSub def.

    multiSub = function(grprater1, grprater2, grpsub1, grpsub2,
                         grprating1, grprating2, grpauxA1, grpauxA2,...)
    {
        rstTemp = c()
        single = singleSub(grprater1, grprater2, grpsub1, grpsub2,
                           grprating1, grprating2, grpauxA1, grpauxA2,...)
        if(!(sum(is.na(single$auxA1)) == dim(single)[1]))
        {
            auxA = single$auxA1 + single$auxA2
            if(length(auxA[!is.na(auxA)])<2)
            {val = NA}
            else
            {val = multidfun(auxA[!is.na(auxA)], single$diff[!is.na(auxA)],...)}
            rstTemp = rbind(rstTemp, 
                            data.frame(subject = "auxA",
                            rater = unique(single$rater),
                            rater2 = unique(single$rater2),
                            diff = val))                      
        }
        rstTemp
    } #end of function def

    if(dfun == "singleSub")
    {privatefun = singleSub;}
    else if(dfun == "multiSub")
    {privatefun = multiSub;}

    #stage1.  checking input data to make sure that they are correct!
    flag = TRUE
    retVal3 = NULL;  #set return val
    if(length(auxA) != 0 && length(auxA) != length(subject))
    {
        flag = FALSE; 
        print("Error in input values: the length of input variables must be same and not 0.  Exit");
    }

    #make a local copy of the input dataframe
    if(!(length(subject) == length(rater) && 
       length(subject) == length(rating) &&
       length(subject) != 0))
    {
        flag = FALSE; 
        print("Error in input values: the length of input variables must be same and not 0.  Exit");
    };   #set to false 
    #finishing checking here.

    #stage2, if flag == TRUE, continue otherwise give error message and exit
    if(flag == TRUE)
    {
        #stage2.1.  define default singledfun, multidfun.
        if(length(singledfun) == 0)
        {
            singledfun = function(a, b,...)
            {abs(a - b);}
        }

        if(length(multidfun) == 0)
        {
            multidfun = function(a, b,...)
            {cor(a,b,...);}
        }

        #stage 2.2       
        result1 =c()    
        uniqRater = unique(rater);  #get the unique rater
        for(i in 1: (length(uniqRater)-1))
        {
            uniqRater2 = uniqRater[(i+1):length(uniqRater)]
            for(j in 1: length(uniqRater2))
            {                 
                grprater1 = rater[rater == uniqRater[i]]
                grprater2 = rater[rater == uniqRater2[j]]
                grpsub1 = subject[rater == uniqRater[i]]
                grpsub2 = subject[rater == uniqRater2[j]]
                grprating1 = rating[rater == uniqRater[i]]
                grprating2 = rating[rater == uniqRater2[j]]                
                grpauxA1 = auxA[rater == uniqRater[i]]
                grpauxA2 = auxA[rater == uniqRater2[j]]    

                temp3 = privatefun(grprater1, grprater2, grpsub1, grpsub2,
                                   grprating1, grprating2, grpauxA1, grpauxA2,...)

                if(dim(temp3)[1] > 0)
                {result1 = rbind(result1, temp3)}
            }
        }

        result2 = cbind(result1, std = paste(result1$subject,  result1$rater,  result1$rater2, sep = ""))
        std = unique(result2$std)

        myitem = c()
        for(i in as.vector(std))
        {
            oneset = result2[result2$std == i,]
            newmean = mean(oneset$diff, na.rm = TRUE)
            myitem = rbind(myitem, cbind(result2[result2$std == i,][1,], 
                           disagreement = newmean, N = dim(oneset)[1]))
        }

        if(dfun == "singleSub")
        {myitem= upData(myitem, drop = Cs(diff,std, auxA1,auxA2), 
                        rename = c(rater = "rater1", disagreement = "variability"))}
        else
        myitem= upData(myitem, drop = Cs(diff,std), 
                        rename = c(rater = "rater1", disagreement = "variability"))

        uniqSub = unique(myitem$subject)
        retVal2 = data.frame(subject=c(), variability=c(), N = c())
        for(k in 1:length(uniqSub))
        {
            temp = myitem[myitem$subject ==uniqSub[k],]
            tempAvg = sum(temp$variability * temp$N, na.rm = TRUE)
            tempCount = sum(temp$N, na.rm = TRUE)
            tempItem= data.frame(subject = uniqSub[k], variability = tempAvg/tempCount, N = tempCount)
            retVal2 = rbind(retVal2, tempItem)
        }
        rtnMean = mean(retVal2$variability)

        if(dfun == "multiSub")
        {
            myitem = upData(myitem, drop = Cs(subject));
            retVal2 = NULL;
            retVal3 = NULL;
            rtnMean3 = NULL;
        }
        else
        {
            uniqRaterPair = unique(paste(myitem$rater1, myitem$rater2))
            retVal3 = data.frame(rater1=c(), rater2 = c(), variability=c(), N = c())
            for(k in 1:length(uniqRaterPair))
            {
                temp = myitem[paste(myitem$rater1, myitem$rater2) == uniqRaterPair[k],]
                tempAvg = sum(temp$variability * temp$N, na.rm = TRUE)
                tempCount = sum(temp$N, na.rm = TRUE)
                tempItem= data.frame(rater1 = strsplit(uniqRaterPair[k], " ")[[1]][1],
                                     rater2 = strsplit(uniqRaterPair[k], " ")[[1]][2],
                                     variability = tempAvg/tempCount, N = tempCount)
                retVal3 = rbind(retVal3, tempItem)
            }
        }

        retVal3 = structure(list(variabilityType = "Inter-Variability",
                                 pairwiseVariability = myitem, 
                                 variabilityBySubject = retVal2,
                                 variabilityByRaterPair = retVal3), 
                            class = "variability")
    } 
    return(retVal3)
}#end of function definition

######################################################################
#variability class member functions:  print
######################################################################
print.variability = function(x,...)
{
    type = x$variabilityType;  #get the type
    cat(type, "\n")
    if(type == "Inter-Variability")
    {
        cat('\nMeasures for All Pairs of Raters\n')
        print(x$pairwiseVariability,...)
        if(!is.null(x$variabilityBySubject))
        {
            cat('\nMeasures by Rater Pairs\n')
            print(x$variabilityByRaterPair,...)

            cat('\nMeasures by Subjects\n')
            print(x$variabilityBySubject,...)
            #cat('Mean of variability: ')
            #cat(x$overallAvg, "\n")
        }
    }
    else if(type == "Intra-Variability")
    {
        cat('\nMeasures by Subjects and Raters\n')
        print(x$variabilityBySubRater,...)
        cat('\nMeasures by Subjects\n')
        print(x$variabilityBySub,...)
        cat('\nMeasures by Raters\n')
        print(x$variabilityByRater,...)
    }
    else
    {print(x,...)}
    invisible()
}

######################################################################
#variability class member functions:  summary
######################################################################
summary.variability = function(x, ...)
{
    type = x$variabilityType;  #get the type
    if(type == "Inter-Variability")
    {
        cat("Inter-variability summary:\n")

        val = x$pairwiseVariability$variability
        if(!is.null(x$variabilityBySubject))
        {
            cat("Measures for All Pairs of Raters\n")
        }
        else
        {
            cat("Measures for All Pairs of Raters, multi-subject mode\n")
        }

        cat("Variability mean: ", mean(val, na.rm = TRUE), "\n")
        cat("Variability median: ", median(val, na.rm = TRUE), "\n")
        cat("Variability range: ", range(val, na.rm = TRUE), "\n")
        cat("Variability quantile: \n")
        qtl = quantile(val, na.rm = TRUE, ...)
        cat("0%: ", qtl[1], "  25%: ", qtl[2], "  50%: ", qtl[3], 
            "  75%: ", qtl[4], "  100%: ", qtl[5], "\n")

        if(!is.null(x$variabilityBySubject))
        {
            val = x$variabilityByRaterPair$variability
            cat("\nMeasures by Rater Pairs only\n")
            cat("Variability mean: ", mean(val, na.rm = TRUE), "\n")
            cat("Variability median: ", median(val, na.rm = TRUE), "\n")
            cat("Variability range: ", range(val, na.rm = TRUE), "\n")
            cat("Variability quantile: \n")
            qtl = quantile(val, na.rm = TRUE, ...)
            cat("0%: ", qtl[1], "  25%: ", qtl[2], "  50%: ", qtl[3], 
                "  75%: ", qtl[4], "  100%: ", qtl[5], "\n")       

            val = x$variabilityBySubject$variability
            cat("\nMeasures by Subjects\n")
            cat("Variability mean: ", mean(val, na.rm = TRUE), "\n")
            cat("Variability median: ", median(val, na.rm = TRUE), "\n")
            cat("Variability range: ", range(val, na.rm = TRUE), "\n")
            cat("Variability quantile: \n")
            qtl = quantile(val, na.rm = TRUE, ...)
            cat("0%: ", qtl[1], "  25%: ", qtl[2], "  50%: ", qtl[3], 
                "  75%: ", qtl[4], "  100%: ", qtl[5], "\n")       
        }
    }
    else if(type == "Intra-Variability")
    {
        cat("Intra-variability summary:\n\n")

        val = x$variabilityBySubRater$variability
        cat("Measures by Subjects and Raters\n")
        cat("Variability mean: ", mean(val, na.rm = TRUE), "\n")
        cat("Variability median: ", median(val, na.rm = TRUE), "\n")
        cat("Variability range: ", range(val, na.rm = TRUE), "\n")
        cat("Variability quantile: \n")
        qtl = quantile(val, na.rm = TRUE, ...)
        cat("0%: ", qtl[1], "  25%: ", qtl[2], "  50%: ", qtl[3], 
            "  75%: ", qtl[4], "  100%: ", qtl[5], "\n")

        val = x$variabilityBySub$variability
        cat("\nMeasures by Subjects\n")
        cat("Variability mean: ", mean(val, na.rm = TRUE), "\n")
        cat("Variability median: ", median(val, na.rm = TRUE), "\n")
        cat("Variability range: ", range(val, na.rm = TRUE), "\n")
        cat("Variability quantile: \n")
        qtl = quantile(val, na.rm = TRUE, ...)
        cat("0%: ", qtl[1], "  25%: ", qtl[2], "  50%: ", qtl[3], 
            "  75%: ", qtl[4], "  100%: ", qtl[5], "\n")
      
        val = x$variabilityByRater$variability
        cat("\nMeasures by Raters\n")
        cat("Variability mean: ", mean(val, na.rm = TRUE), "\n")
        cat("Variability median: ", median(val, na.rm = TRUE), "\n")
        cat("Variability range: ", range(val, na.rm = TRUE), "\n")
        cat("Variability quantile: \n")
        qtl = quantile(val, na.rm = TRUE, ...)
        cat("0%: ", qtl[1], "  25%: ", qtl[2], "  50%: ", qtl[3], 
            "  75%: ", qtl[4], "  100%: ", qtl[5], "\n")
    }
    else
    {summary(x, ...)}
}

