##' This function generate individual trapping histories suitable to be analised by RMark (and therefore MARK) 
##'
##' This function takes the results of a removal experiment in its simples expression (pooled total of individuals per nigth or removal event/pass) and format it to be used by \code{RMark} package. RMark (and therefore MARK) accepts a few different types of data formats. I have chosen the long format because it will be consistent with the rest of modelling approaches offered in the package
##' @title Fotrmatting removal data for RMark
##' @param pop Numerical vector containing a sequence of numbers representiong the total number of animals extracted per night
##' @param events Numeric. Number of removal events considering for futher analysis (equal or smaller than the number of trappiong events)
##' @param msg \code{logical}. IF TRUE a short message containing the total number of individuals removed for the number of events considered is printed in the console.
##' @return an object of class \code{HistRMarkLong} consisting on a list of the following objects: \cr
##' \itemize{
##' \item formatted: a data frame containing two columns
##' \itemize{
##' \item ch: individual removal histories
##' \item ind: number of individuals associated to each removal history (default to 1)
##' }
##' \item captures: number of removed individuals per removal event considered for formatting
##' }
##' @examples
##' myHist <- genHistRMark(c(200, 150, 125), 3)
##' @export
##' @author Fer Arce
genHistRMark <- function(pop,events, msg = TRUE){
    indi <- sum(pop)
    ali <- matrix(, nrow = indi, ncol = events)
    ba <- cumsum(pop)
    i <- 1
    while(i <= events){
        if (i == 1){
            ali[1:ba[i], i] <- 1
        } else {
            ali[(ba[i-1]+1):ba[i], i] <- 1
        }
        i = i +1
    }
    ali[is.na(ali)] <- 0
    ch <- apply(ali, 1, paste, collapse = '')
    sim <- data.frame(ch, ind = 1)
    sim <- list(formatted = sim, captures = pop[1:events])
    class(sim) <- 'HistRMarkLong'
    if (msg)
        cat('\nA total of', nrow(sim[[1]]), 'individuals have been processed.\n\n')
    
    return(sim)
}


##' Print objects of class HistRMarkLong to the console
##'
##' This method extends the generic `print` function for objects of
##' class \code{HistRMarkLong}. It print a summary of information into
##' the R console, preventing console overflow with long but informative
##' information.
##' @param x and object of class \code{HistRMarkLong}
##' @return a message printed in the console
##' @examples
##' print(myHist)
##' @method print HistRMarkLong
##' @author Fer Arce
##' @export
print.HistRMarkLong <- function(x){
    ##return(x[[1]])
    cat('\nA total of', sum(x[[2]]), 'unique individuals removed \nduring',
        length(x[[2]]), 'sessions have been processed to \nMARK long format as shown below.\n\n')
    print(head(x[[1]]))
    cat('\t...\n')
}



##' Convert objects of class HistRMarkLong to a data.frame
##'
##' This method extends the generic `as.data.frame` function for
##' objects of class \code{HistRMarkLong}. It extract the most
##' relevant component of the object, which is a data frame containing
##' the individual removal histories.
##' @param x an object of class \code{HistRMarkLong}
##' @return a data frame containig two columns. The first, named ch
##'     contains the removal histories, and the second, named ind,
##'     contain the number of individual asociated to each
##'     history. This naming approach matched the naming used in
##'     package \code{RMark}
##' @method as.data.frame HistRMarkLong
##' @examples
##' myCaptHist <- as.data.frame(myHist)
##' @author Fer Arce
##' @export
as.data.frame.HistRMarkLong <- function(x){
    return(x[[1]])
}
