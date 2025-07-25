##' Function for making Y axis reasonable for ploting
##'
##' sanity check of results for making reasonable plots
##' @title result checking
##' @param rec results from a recursive analysis 
##' @return should returbn an opbject with the information about abnormal confidence intervals
##' @author Fer Arce
##' @noRd
checkRec <- function(rec){
    ## find weeird stuff
    ## 1 need to find - flag singular values
    trends <- sign(diff(rec))
    if (any(trends > 0))
        toIgnore <- which(trends > 0)
## if we dounf it
    if (exists('toIgnore'))
        ## we need to remove the weird value for definning the margins, not for plotting
        margins <- list( y = rec$ucl[order(rec$ucl, decreasing = TRUE)][[2]])
}
