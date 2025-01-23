## add model = TRUE and sink the model data to a specific folder.




##' Function to fit a closed population CMR model 
##'
##'This function fit a closed capture-mark-recapture model with extreme behavioural response (complete trap-shyness). The rationale of this approach is that an extreme trap-shyness behavior will result in capture histories consisting on only one capture per individual and thus becoming a removal experiment (either released or removed, animals won't occupy the traps again -at least during the survery term). This approach is suggested in Otis et al. (1978) and has been extensively tested via simulations.
##'
##' This method fit a multinomial Mtbh model implenmented in MARK and called triough RMak package. Instructions on how to install RMark can be found in the PhiDot website http://www.phidot.org
##' @title estimation of population size of removal experiments using MLE over an extreme Mtbh model.
##' @param Data an object of class \code{HistRMarkLong}
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
##' @export
##' @examples
##' exPop <- removeMLE(genHistRMark(c(100,80,60),3))
##' @author Fer Arce
removeMLE <- function(Data){
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
    output <- list(Data = Data, result = N)
    class(output) <- 'fittedRemMLE'
    return(output)
}



## we keepinmg the model somehow
removeMLEOriginal <- function(Data){
    stopifnot(class(Data) == "HistRMarkLong")
    c.1 = list(formula=~1, fixed = 0)
    if(.Platform$OS.type == 'windows'){
        File <- 'nul'
    } else {
        File <- '/dev/null'
    }
    sink(file = File)
    data.mark <- data.frame(ch = Data[[1]]$ch, ind = 1)
    model<-RMark::mark(data.mark,model="Closed",
                     model.parameters=list(c=c.1),
                     delete=FALSE, hessian = TRUE, silent = TRUE)
    sink()
    N <- model$results$derived
    output <- list(Data = Data, result = N, model = model)
    class(output) <- 'fittedRemMLE'
    return(output)
}


removeMLETime <- function(Data){
    stopifnot(class(Data) == "HistRMarkLong")
    c.1 = list(formula=~1, fixed = 0)
    p.T = list(formula=~Time)
    ## if(.Platform$OS.type == 'windows'){
    ##     File <- 'nul'
    ## } else {
    ##     File <- '/dev/null'
    ## }
    ## sink(file = File)
    data.mark <- data.frame(ch = Data[[1]]$ch, ind = 1)
    ren<-mark(data = data.mark,model="Closed", model.parameters=list(c=c.1, p = p.T), delete=FALSE, hessian = TRUE, silent = TRUE)
    sink()
    N <- ren$results$derived
    output <- list(Data = Data, result = N, model = ren)
    class(output) <- 'fittedRemMLE'
    return(output)
}


removeMLEtime <- function(Data){
    stopifnot(class(Data) == "HistRMarkLong")
    c.1 = list(formula=~1, fixed = 0)
    p.t = list(formula=~time)
    if(.Platform$OS.type == 'windows'){
        File <- 'nul'
    } else {
        File <- '/dev/null'
    }
    sink(file = File)
    data.mark <- data.frame(ch = Data[[1]]$ch, ind = 1)
    ren<-RMark::mark(data.mark,model="Closed",
                     model.parameters=list(c=c.1, p = p.t),
                     delete=FALSE, hessian = TRUE, silent = TRUE)
    sink()
    N <- ren$results$derived
    output <- list(Data = Data, result = N, model = ren)
    class(output) <- 'fittedRemMLE'
    return(output)
}



##' Printing method objects of class fittedRemMLE to the console
##'
##' This method extend the generic `print` function for objects of
##' class \code{fittedRemMLE}. It prints to the console the Estimated
##' population size of the population being monitored.
##' @param x an object of class \code{fittedRemMLE}
##' @param ... further generic arguments to be pased to the generic method 
##' @return prints a message in the console with the main results of the model:
##' \itemize{
##' \item estimate: estimated number of individuals present in the population at the begining of the removal experiment
##' \item se: estandard error of the estimate
##' \item lcl: 95\% lower confidence interval
##' \item ucl: 95\% upper confidence interval
##' }
##' @author Fer Arce
##' @method print fittedRemMLE
##' @export
  ##' @examples
##' print(removeMLE(genHistRMark(c(100,80,60),3)))
print.fittedRemMLE <- function(x, ...){
    cat('\nPopulation size at the start of the\nremoval experiment:\n\n')
    print(x$result$`N Population Size`)
}


##' Convert objects of class fittedRemMLE to a data.frame
##'
##' This method extends the generic `as.data.frame` function for
##' objects of class \code{fittedRemMLE}. It extracts the most
##' relevant component of the object, which is a data frame containing
##' the result of the model estimation of Population size.
##' @param x an object of class \code{fittedRemMLE}
##' @param row.names `NULL` or character vector containing row names for the data frame.
##' @param optional logical. If `TRUE`, setting row names and converrting column names is optional.
##' @param ... additional arguments to be passed to or from methods.
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
##' myEstimate <- as.data.frame(removeMLE(genHistRMark(c(100,80,60),3)))
as.data.frame.fittedRemMLE <- function(x, row.names, optional, ...){
    return(x[[2]][[1]])
}
