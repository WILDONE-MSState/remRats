##' Simple function to simulate removal datasets
##'
##' it needs somehow to stop at zero and fill with zeroes after all individuals have been removed
##' @title Removal simulation
##' @param N population size (initial)
##' @param p capturability (probabilidad of caopture, equal for each individual, in the interval (0,1))
##' @param j number of removal occasions
##' @return a vector of total number of captures per removal event
##' @examples
##' simEx <- remSim(100, 0.5, 4)
##' @author Fer Arce
remSim<-function(N,p,j){
    Capt<-numeric(j)
    for (i in 1:j){
        Capt[i]<-sum(rbinom(N-sum(Capt),1,prob=p))
    }
    return(Capt)
}
