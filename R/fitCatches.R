##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Fitting a gam model to cumulative numbers of removed individuals.
##' @param Data
##' @param k smoothing parameter - number of knots (It cannot be larger than n-1 occasions)
##' @return 
##' @author Fer Arce
##' @export
fitCatches <- function(Data, k = 5){
    capts <- cumsum(pob1)
    n <- 1:length(pob1)
    if (k >= (length(capts) - 1))
        k = length(capts) - 1
    model <- gam(capts~s(n),bs = 'cr', k = k)
    return(model)
}
