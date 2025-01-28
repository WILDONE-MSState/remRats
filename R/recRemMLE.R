##' Function to fit recursively a closed population CMR model
##'
##'This function fit closed capture-mark-recapture models with extreme behavioural response (complete trap-shyness)as detailed in Otis et al. (1978). This function calls function \code{removeMLE} recursively an generate a sequence of estimates to see the evolution and estabilization of the Population size estimations and the relative shrinking of the confidence interval. 
##'
##' 
##' @title estimation of population size of removal experiments using MLE over an extreme Mtbh model.
##' @param Data an object of class \code{HistRMarkLong}
##' @param events maximum number of events to be considered
##' @param recursive Logical. If TRUE, it will make the fuill recursive from, to.
##' @references Otis, D. L., Burnham, K. P., White, G. C., and Anderson, D. R. (1978). Statistical inference from capture data on closed animal populations. *Wildlife monographs*, 62, 1-135.
##' @return an object of class \code{fittedRemMLERec} containing a list of two elements: \cr
##' \itemize{
##' \item Data: the object of class `HistRMarkLong` used as imput to the function
##' \item result: The actual results of recursively fitting the removal model to the supplied Data. It is a data frame containing a row per recursive fit with the following information:
##' \itemize{
##' \item estimate: the estimated population size
##' \item se: standard error of the estimate
##' \item lcl: lower 95% confidence interval value
##' \item ucl: upper 95% confidence interval value
##' \item n.occ: number of removing events
##' \item NCatch: Cumulative number of individuals removed
##' \item propCatch: proportion of the population already removed (againts every iteration of the Estimated N  hat)
##' }
##' }
##' @export
##' @examples
##' exPop <- recRemMLE(genHistRMark(c(200, 150, 125), 3),3)
##' @author Fer Arce
recRemMLE <- function(Data, events, recursive = TRUE){ #, goal = NULL)
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
    res$NCatch <- cumsum(Data$captures)[-1]
    res$propCatch <- round(res$NCatch/ res$estimate,2)
    ## if (is.numeric(goal))
    ##     res$goal <- ifelse(res$propCatch < goal, FALSE, TRUE)
    if (any(res$se == 0))
        warning('\nSome of the fitted values are singular and should be ignored.\nThey can be identified by looking at the reported standard errors.\n')
    out <- list(data = Data, Nest = res)
    class(out) <- 'fittedRemMLERec'
    return(out)
}

##' Printing method objects of class fittedRemMLERec to the console
##'
##' This method extend the generic `print` function for objects of
##' class \code{fittedRemMLERec}. It prints to the console the Estimated
##' population size of the population being monitored.
##' @param x an object of class \code{fittedRemMLERec}
##' @param ... further arguments to be passed to the class
##' @return prints a message in the console with the main results of the model:
##' \itemize{
##' \item estimate: estimated number of individuals present in the population at the begining of the removal experiment
##' \item se: estandard error of the estimate
##' \item lcl: 95\% lower confidence interval
##' \item ucl: 95\% upper confidence interval
##' }
##' @author Fer Arce
##' @method print fittedRemMLERec
##' @noRd
##' @examples
##' print(recRemMLE(genHistRMark(c(200, 150, 125), 3),3))
print.fittedRemMLERec <- function(x, ...){
    cat('\nEvolution of the Population size estimate\nat the start of the removal experiment as the number of remoival events increases:\n\n')
    print(round(x[[2]], 2))
    cat('\n')
}

##' Convert objects of class fittedRemMLERec to a data.frame
##'
##' This method extends the generic `as.data.frame` function for
##' objects of class \code{fittedRemMLERec}. It extracts the most
##' relevant component of the object, which is a data frame containing
##' the result of the recursively estimation of Population size.
##' @param x an object of class \code{fittedRemMLE}
##' @param row.names `NULL` or a character vector giving the row names for the data frame. Missing values are not allowed
##' @param optional logicalf `TRUE`, setting row names and converting column namesis optional.
##' @param ... further arguments to be passeds to the generic method
##' @method as.data.frame fittedRemMLERec
##' @return a data frame containing one row per recursive estimate  with the following columns:
##' \itemize{
##' \item estimate: estimated number of individuals present in the population at the begining of the removal experiment
##' \item se: estandard error of the estimate
##' \item lcl: 95\% lower confidence interval
##' \item ucl: 95\% upper confidence interval
##' }
##' @author Fer Arce
##' @noRd
##' @examples
##' recEst <- as.data.frame(recRemMLE(genHistRMark(c(200, 150, 125), 3),3))
as.data.frame.fittedRemMLERec <- function(x, row.names, optional, ...){
    return(x[[2]][[1]])
}
