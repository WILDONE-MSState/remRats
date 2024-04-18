##' This function fit a closed population CMR model 
##'
##'Runbs a model of type extreme behaviour as suggested in Otis et al.
##' @title Population estimation
##' @param Data an object of class...
##' @param engine to be developed, if NULL, mark is used
##' @param model to be filled for covartiates
##' @return
##' @examples
##' exPop <- removeMLE(exHist)
##' @author Fer Arce
removeMLE <- function(Data, engine = NULL, model = NULL){
    require(RMark)
    c.1 = list(formula=~1, fixed = 0)
    if(.Platform$OS.type == 'windows'){
        File <- 'nul'
    } else {
        File <- '/dev/null'
    }
    
    sink(file = File)
    ren<-mark(Data,model="Closed",model.parameters=list(c=c.1),delete=TRUE, hessian = TRUE, silent = TRUE)
    sink()
    N <- ren$results$derived
    ## anyadir una class chula aqui
    return(N)
}
