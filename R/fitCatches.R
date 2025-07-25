##' Function that defines the gam structured used.
##'
##' This function fits a gam to a processed object. The object of this
##' function is to define the gam structure (mgvc model) over the
##' cumulative number of animals removed, aimed to smoith a curve with
##' will be projected in order to evakluate current and future
##' performance of the removal experiment in regards the aims of the
##' project. This function is not expected to be called by the end
##' user
##' @title Fitting a gam model to cumulative numbers of removed
##'     individuals.
##' @param Data an object of class fittedRemMLERec
##' @param k smoothing parameterfot the gam function - number of knots
##'     (It cannot be larger than n-1 occasions)
##' @param sub if prresent, subset of data to be be used for fitting
##'     the gam model
##' @return the object of a fitted gam model.
##' @author Fer Arce
fitCatches <- function(Data, k = 5, sub = NULL) {
    capts <- as.vector(Data$Nraw$Ncumu)
    if (!is.null(sub))
        capts = capts[1:sub]
    n <- 1:length(capts)
    if (k >= (length(capts) - 1))
        k = length(capts) - 1
    model <- gam(capts~s(n, k = k), bs = 'cr', method = 'REML')
    return(model)
}
