##' Function to simulate removal datasets
##'
##' Simple simulation of removal experiments under basic constraints, following a binomial secuence of removals, with the posibility of adding a cap up to simulate experiments where trap saturation occur. To prevent this behavior to occur, max.capt should be fixed to a ridiculously unrealistic large value. This function does not allow to simulate populations  with time or individual-level varying parameters. It is defined and mantained for the purpose of simple checking and large simulations.
##' @title simple removal simulation
##' @param N Initial population size
##' @param p capturability (probabilidad of copture, equal for each individual, in the interval (0,1))
##' @param j number of removal events
##' @param max.capt maximum number of individuals potentially removed per event (i.e number of single catch traps)
##' @return a vector of total number of captures per removal event
##' @examples
##' simEx <- remSim(100, 0.5, 4, 50)
##' @author Fer Arce
##' @export
remSim<-function(N,p,j, max.capt){
    Capt<-numeric(j)
    for (i in 1:j){
        trial <- sum(rbinom(N-sum(Capt),1,prob=p))
        Capt[i]<- ifelse(trial > max.capt, max.capt, trial)
    }
    return(Capt)
}
