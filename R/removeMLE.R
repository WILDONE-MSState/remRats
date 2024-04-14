##' .. content for \description{} (no empty lines) ..
##'
##'Runbs a model of type extreme behaviour as suggested in Otis et al.
##' @title Population estimation
##' @param Data an object of class...
##' @param engine 
##' @param model 
##' @return
##' @examples
##' exPop <- removeMLE(exHist)
##' @author Fer Arce
removeMLE <- function(Data, engine = NULL, model = NULL){
    require(RMark)
    c.1 = list(formula=~1, fixed = 0)
    ren<-mark(Data,model="Closed",model.parameters=list(c=c.1),delete=TRUE, hessian = TRUE, silent = TRUE)
    N <- ren$results$derived
    ## anyadir una class chula aqui
    return(N)
}
