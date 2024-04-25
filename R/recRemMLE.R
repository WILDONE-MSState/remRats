##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param pob 
##' @param events 
##' @param recursive 
##' @return 
##' @author Fer Arce
recRemMLE <- function(pob, events, recursive = TRUE){
    if (events > length(pob)){
        ev <- events
        events <- length(pob)
        warning('\nNumber of requested events (',ev,') is too large for the data\nsupplied. It has been adjusted to its maximum possible \nvalue (',events,').\n\n')
    }
    out <- vector('list', events)
    for (i in 2:events){
        tmpSim <- genHistRMark(pob1, i, msg = FALSE)
        tmpSim <- tmpSim[tmpSim$ch != paste(rep(0,times =i), sep = '', collapse = ''), ]
        tmpPop<-removeMLE(tmpSim)
        tmpPop[[1]]$n.occ <- i
        out[i] <- tmpPop
    }
    res <- do.call(rbind, out)
    if (any(res$se == 0))
        warning('\nSome of the fitted values are singular and must be ignored.\nYou can identify them by looking to the reported standard errors.\n')
    return(res)
}
