
##' Function to fit and project a gam curve to the cumulative number of individuals removed.
##'
##' This function fits a gam model to the provided number of
##' cumulative captures, and then project the model fit to near future
##' trapping events
##' @title
##' @param Data an object of class 'fittedRemMLERec'.
##' @param proj numbers of future days to be projected.
##' @param sub optionalfurther passes to fitCatches and inside this function
##' @param ... further arguments to be imposed to fitCatches (like
##'     smoothing parameter or subset od data to fit the gam)
##' @return
##' @author Fer Arce
##' @export
projCatches <- function(Data, proj = 1, sub = NULL ...) {
    model <- fitCatches(Data, ...)
    ## algo como if exists
    le <- nrow(Data$Nraw)
    if (!is.null(sub))
        le <- sub
    test <- data.frame(n = 1:(le + proj))
    pred <- predict(model, test,type = 'response', se = TRUE)
    pred[[1]] <- floor(pred[[1]])
    pred[[2]] <- round(pred[[2]], 1)
    pred <- as.data.frame(pred)
    pred$lcl <- round(pred$fit - 1.96*pred$se.fit, 1)
    pred$ucl <- round(pred$fit + 1.96*pred$se.fit, 1)
    pred$n.occ <- 1:nrow(pred)
    return(pred)
}
