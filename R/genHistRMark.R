##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param j 
##' @param b 
##' @return 
##' @examples
##' exHist <- genHistRMark(3, c(200, 150, 125))
##' @author Fer Arce
genHistRMark <- function(j,b){
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







