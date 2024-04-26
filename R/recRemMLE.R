##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param pob 
##' @param events 
##' @param recursive 
##' @return 
##' @author Fer Arce
recRemMLE <- function(Data, events, recursive = TRUE){
    stopifnot(class(Data) == "HistRMarkLong")
    if (events > length(Data[[2]])){
        ev <- events
        events <- length(Data[[2]])
        warning('\nNumber of requested events (',ev,') is too large for the data\nsupplied. It has been adjusted to its maximum possible \nvalue (',events,').\n\n')
    }
    pob1 <- Data[[2]]
    out <- vector('list', events)
    for (i in 2:events){
        tmpSim <- genHistRMark(pob1, i, msg = FALSE)
        empties <- paste(rep(0,times =i), sep = '', collapse = '')
        kept <- tmpSim[[1]]$ch[tmpSim[[1]]$ch != empties]
        tmpSim[[1]] <- data.frame(ch = kept, ind = 1)
        tmpPop<-removeMLE(tmpSim)
        tmpPop[[2]][[1]]$n.occ <- i
        out[i] <- tmpPop$result
    }
    res <- do.call(rbind, out)
    if (any(res$se == 0))
        warning('\nSome of the fitted values are singular and must be ignored.\nYou can identify them by looking to the reported standard errors.\n')
    out <- list(data = Data, Nest = res)
    class(out) <- 'fittedRemMLERec'
    return(out)
}


print.fittedRemMLERec <- function(object){
    cat('\nEvolution of the Population size estimate\nat the start of the removal experiment:\n')
    print(object[[2]])
}


as.data.frame.fittedRemMLE <- function(object){
    return(object[[2]][[1]])
}
