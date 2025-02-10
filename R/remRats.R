##' Wrapper function to make a full process of removal data using a single function call
##'
##' This is a wrapping function to facilitate the visualization of the analytical procedures implemented in remRats
##' @title remRats
##' @param pob1 a numeric vector contianing the number of individuals removed during a remioval experiment at each event. 
##' @return a list from recRemMLE
##' @author Fer Arce
##' @export
remRats <- function(pob1){
    simudata<-genHistRMark(pob1)
    popEst<-recRemMLE(simudata, events = 5)
    popEst$Nest[2:5] <- round(popEst$Nest[2:5], 0) 
    return(popEst)
}
