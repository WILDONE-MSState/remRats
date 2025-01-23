##' .. content for \description{} (no empty lines) ..
##'
##' sanity check of results
##' @title result checking
##' @param rec 
##' @return 
##' @author Fer Arce
checkRec <- function(rec){
    ## find weeird stuff
    ## 1 need to find - flag singular values
    trends <- sign(diff(pob1))
    if (any(trends > 0))
        toIgnore <- which(trends > 0)
## if we dounf it
    if (exists('toIgnore'))
        ## we need to remove the weird value for definning the margins, not for plotting
        margins <- list( y = res$ucl[order(res$ucl, decreasing = TRUE)][[2]])
}
