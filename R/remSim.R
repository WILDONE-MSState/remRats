##' Function for simulation of removal processes of closed populations
##'
##' Simulation of removal experiments under basic constraints, following a binomial secuence of removals. It allow to set an upper limit in the number of individuals trapping per event to simulate experiments where trap saturation occur (Limited number of single-catch trap and elevated density of highly catchable targets.
##' The function simulates removal sampling data over \eqn{j} occasions.
##' On each occasion \eqn{i}, the number of individuals captured is modeled as:
##' \deqn{C_i \sim \text{Binomial}(N - \sum_{k=1}^{i-1} C_k,\ p_i)}
##' where \eqn{N} is the initial population size, and \eqn{p_i} is the detection probability 
##' at occasion \eqn{i}. If \eqn{p} is given as a scalar, it is assumed constant across all \eqn{j} occasions.
#' The argument \code{max.capt} can be used to set a maximum number of individuals captured on any occasion (specially for single-catch trap approaches).
##' @title Simple removal simulation
##' @param N Initial (closed) population size
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
