##' Function for simulation of removal datasets
##'
##' Simulation of removal experiments under basic constraints, following a binomial secuence of removals. It allow to set an upper limit in the number of individuals trapping per event to simulate experiments where trap saturation occur (Limited number of single-catch trap and). This function does not allow to simulate populations  with time or individual-level varying parameters. It is defined and mantained for the purpose of simple checking and large simulations.
##' @title simple removal simulation
##' @param N Initial population size
##' @param p capturability (Capture probability (equal) for each individual, in the interval (0,1))
##' @param j number of removal events
##' @param max.capt maximum number of individuals potentially removed per event (i.e number of single catch traps in mammal trapping projects). It is set by default to the initial population size subject to extraction
##' @return a vector containing the sequence of the number of captures per removal event
##' @examples
##' simEx <- remSim(100, 0.5, 4, 50)
##' @author Fer Arce
##' @export
remSim<-function(N,p,j, max.capt = N){
    Capt<-numeric(j)
    if (length(p) == 1) {
        p <- rep(p, j)
    } else if (length(p) != j) {
        stop("Length of p must be either 1 or equal to j.")
    }
    for (i in 1:j){
        trial <- sum(rbinom(N-sum(Capt),1,prob=p[i]))
        Capt[i]<- ifelse(trial > max.capt, max.capt, trial)
    }
    return(Capt)
}
