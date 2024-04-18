##' This function generate individual trapping histories suitable to be analised by RMark (and theefore MARK) 
##'
##' 
##' @title Generate trapping histories for RMArk
##' @param b vector of class numeric containing a sequence of numbers representiong the total number of animals extracted per night
##' @param j nunmeric. Number of removal events considering for futher analysis
##' @return an object of class data.frame containing trapping histories formated to be used by RMark
##' @examples
##' exHist <- genHistRMark(3, c(200, 150, 125))
##' @author Fer Arce
genHistRMark <- function(b,j){
    indi <- sum(b)
    ali <- matrix(, nrow = indi, ncol = j)
    ba <- cumsum(b)
    i <- 1
    while(i <= j){
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
    return(sim)
}







