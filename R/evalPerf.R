3## ## need to incorp[orate goal, and it also need to project itself to the future to see when the goal will be met.
##' .. content for \description{} (no empty lines) ..
##'
##' This function evaluates the current progress of the removal experiment according to a given goal of trapping a  proportion of the population with a fixed nunmber of trapping events.
##' @title 
##' @param Data an object of class fittedRemMLERec. If not, it has to be called.
##' @param fixed Number of originally fixed trapping days
##' @param goal proportion of the population aimed to be collected
##' @return a data frame
##' @author Fer Arce
##' @noRd
evalPerf <- function(Data, fixed = 5, goal = .9){
    ## via remRats
    Data$Nest$goal <- ifelse(Data$Nest$propCatch < goal, FALSE, TRUE)
    ret <- c(which.max(Data$Nest$goal) + 1,
      ## via Fixed sanmpling
      Data$Nest$goal[fixed -1],
      Data$Nest$propCatch[fixed -1])
    ret[4] <- round(fixed/ret[1],2)
    names(ret) <- c('daysToAchieve', 'Fixed', 'propFixed', 'ratio')
    ret <- as.data.frame(ret)
    return(ret)
}
