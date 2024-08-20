3## ## need to incorp[orate goal
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param Data an object of class fittedRemMLERec
##' @param fixed Number of originally fixed trapping days
##' @param goal proportion of the population to be collected
##' @return 
##' @author Fer Arce
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
