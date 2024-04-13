##' function to simulate simple removal datasets
##'
##' it needs somehow to stop at zero and fill with zeroes after all individuals have been removed
##' @title 
##' @param N population size
##' @param p capturability
##' @param j number of removal occasions
##' @return
##' @author Fer Arce
remSim<-function(N,p,j){
    Capt<-numeric(j)
    for (i in 1:j){
        Capt[i]<-sum(rbinom(N-sum(Capt),1,prob=p))
    }
    return(Capt)
}
