##' This function fit a closed population CMR model 
##'
##'Runbs a model of type extreme behaviour as suggested in Otis et al. (1978)
##' @title estimation of population size of removal experiments using MLE
##' @param Data an object of class \code{HistRMarkLong}
##' @param engine not implemented
##' @param model not implemented
##' @references Otis, D. L., Burnham, K. P., White, G. C., and Anderson, D. R. (1978). Statistical inference from capture data on closed animal populations. *Wildlife monographs*, 62, 1-135.
##' @return an object of class \code{fittedRemMLE} containing a list of two elements: \cr
##' \itemize{
##' \item Data: the object of class `HistRMarkLong` used as imput to the function
##' \item result: The actual results of fitting the removal model to the supplied Data. It is a single row data frame containing the following information:
##' \itemize{
##' \item estimate: the estimated population size
##' \item se: standard error of the estimate
##' \item lcl: lower 95% confidence interval value
##' \item ucl: upper 95% confidence interval value
##' }
##' }
##' @examples
##' exPop <- removeMLE(exHist)
##' @author Fer Arce
removeMLE <- function(Data, engine = NULL, model = NULL){
    stopifnot(class(Data) == "HistRMarkLong")
    c.1 = list(formula=~1, fixed = 0)
    if(.Platform$OS.type == 'windows'){
        File <- 'nul'
    } else {
        File <- '/dev/null'
    }
    
    sink(file = File)
    data.mark <- data.frame(ch = Data[[1]]$ch, ind = 1)
    ren<-RMark::mark(data.mark,model="Closed",
                     model.parameters=list(c=c.1),
                     delete=TRUE, hessian = TRUE, silent = TRUE)
    sink()
    N <- ren$results$derived
    ## anyadir una class chula aqui
    output <- list(data = Data, result = N)
    class(output) <- 'fittedRemMLE'
    return(output)
}






##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param object 
##' @return 
##' @author Fer Arce
print.fittedRemMLE <- function(object){
    cat('\nPopulation size at the start of the\nremoval experiment:\n\n')
    print(object$result$`N Population Size`)
}


##' Convert objects of class fittedRemMLE to a data.frame
##'
##' This method extends the generic `as.data.frame` function for
##' objects of class \code{fittedRemMLE}. It extracts the most
##' relevant component of the object, which is a data frame containing
##' the result of the model estimation of Population size.
##' @param x an object of class \code{fittedRemMLE}
##' @method as.data.frame fittedRemMLE
##' @return a data frame containing one row with the following columns:
##' \itemize{
##' \item estimate: estimated number of individuals present in the population at the begining of the removal experiment
##' \item se: estandard error of the estimate
##' \item lcl: 95\% lower confidence interval
##' \item ucl: 95\% upper confidence interval
##' }
##' @author Fer Arce
##' @export
##' @examples
##' myEstimate <- as.data.frame(exPop)
as.data.frame.fittedRemMLE <- function(x){
    return(x[[2]][[1]])
}
