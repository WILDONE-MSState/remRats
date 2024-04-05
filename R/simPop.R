##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param pops Numer of populations to replicate simulations
##' @param N population size
##' @param spatial 
##' @return 
##' @author Fer Arce
simPop <- function(pops = 1, N, spatial = FALSE){
    if (spatial == FALSE) {
        x = rpois(n = pops, lambda = N)
        inds <- data.frame(ID = paste0('IND_ ', 1:x), X = rep(1,x))
        class(inds) <- 'simPop'
        ## anyadir de atributo los Ls, units etc
    }
    return(inds)
}

