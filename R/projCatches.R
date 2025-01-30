
##' Function to fit and project a gam curve.
##'
##' .. content for \details{} ..
##' @title 
##' @param Data an object of class 'fittedRemMLERec'.
##' @param proj numbers of future days to be projected 
##' @param ... further arguments to be imposed to fitCatches
##' @return 
##' @author Fer Arce
##' @export
projCatches <- function(Data, proj, ...) {
    model <- fitCatches(Data, ...)
    test <- data.frame(n = 1:(nrow(Data$Nraw) + proj))
    pred <- predict(model, test,type = 'response', se = TRUE)
}   
