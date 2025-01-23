## function for projecticng the gam from above
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param Data 
##' @param proj 
##' @param ... 
##' @return 
##' @author Fer Arce
projCatches <- function(Data, proj, ...){
    model <- fitCatches(Data, ...)
    test <- data.frame(n = 1:(length(Data) + proj))
    pred <- predict(model, test,type = 'response', se = TRUE)
}
