##' .. content for \description{} (no empty lines) ..
##'
##' simpop generates population realizatiions of indivisduals to be removed. Each realization is obtained by extracting a random value from a distribution Poisson with parameter lambda = Population size. Predefined values set up to 0, forcing users to actually specify both n and N
##' @title simPop
##' @param pops Numer of populations to replicate simulations
##' @param N Expected population population size
##' @param spatial 
##' @return 
##' @author Fer Arce
simPop <- function(pops = 0, N = 0, spatial = FALSE){
    if (pops == 0)
        return(cat('no population requested to simulate'))
    if (spatial == FALSE) {
        x = rpois(n = pops, lambda = N)
        if (length(x) > 0){
            inds <- data.frame(ID = paste0('IND_ ', 1:x), X = rep(1,x))
            class(inds) <- 'simPop'
            return(inds)
        } else {
            return(cat('Simulated population of 0 individuals \nPlease try again with actual param values\n'))
        }
    }
}

