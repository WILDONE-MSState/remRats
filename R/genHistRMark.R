##' This function generate individual trapping histories suitable to be analised by RMark (and theefore MARK) 
##'
##' This is a detailed description
##' @title Generate trapping histories for RMArk
##' @param pob vector of class numeric containing a sequence of numbers representiong the total number of animals extracted per night
##' @param events numeric. Number of removal events considering for futher analysis (equal or smaller than the number of trappiong events)
##' @return an object of class data.frame containing trapping histories formated to be used by RMark
##' @examples
##' exHist <- genHistRMark(3, c(200, 150, 125))
##' @author Fer Arce
genHistRMark <- function(pob,events, msg = TRUE){
    indi <- sum(pob)
    ali <- matrix(, nrow = indi, ncol = events)
    ba <- cumsum(pob)
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
    sim <- list(sim, pob[1:events])
    class(sim) <- 'HistRMarkLong'
    if (msg)
        cat('\nA total of', nrow(sim), 'individuals have been processed.\n\n')
    
    return(sim)
}


##' .. content for \description{} (no empty lines) ..
##'
##' print method for class 'HistRMark'
##' @title 
##' @param object an object of class 'HistRMark'
##' @return 
##' @author Fer Arce
print.HistRMarkLong <- function(object){
    u <- data.frame(ch = object[[1]]$ch, ind = object[[1]]$ind)
    cat('\nA total of', sum(object[[2]]), 'unique individuals removed \nduring',
        length(object[[2]]), 'sessions have been processed to \nMARK long format as shown below.\n\n') 
    print(head(u))
    cat('\t...\n')
}




