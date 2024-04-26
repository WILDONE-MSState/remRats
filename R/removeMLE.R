##' This function fit a closed population CMR model 
##'
##'Runbs a model of type extreme behaviour as suggested in Otis et al.
##' @title Population estimation
##' @param Data an object of class...
##' @param engine to be developed, if NULL, mark is used
##' @param model to be filled for covartiates
##' @returnPopulatioin size estimate
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
    ren<-RMark::mark(data.mark,model="Closed",model.parameters=list(c=c.1),delete=TRUE, hessian = TRUE, silent = TRUE)
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
    cat('\nPopulation size at the start of the\nremoval experiment:\n')
    print(object$result$`N Population Size`)
}
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param object 
##' @return 
##' @author Fer Arce
as.data.frame.fittedRemMLE <- function(object){
    return(object[[2]][[1]])
}
