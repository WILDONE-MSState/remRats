##' This function generates an estimate of population size after two
##' consecutive renmoval sessions using Moran-Zippin estimator
##'
##' Function to estimate the size of a population subject of two
##' consecutive removal occasions. This function is provided for
##' reference, as it relies in strong asumptions like equal
##' catchability of individuals. Its use is strongly discouraged, and
##' removal experiments should always consist of at least 3 removal
##' events. The use of function \code{removeMLE} with at leasdt three
##' sampling events should be the prefered approach as it adds more
##' flexibility.
##'
##' A major problem of the Moran-Zippin estimator is that it requires
##' that the number of individuals removed in the second event must be
##' smaller than the number removed in the first event. Otherwise it
##' will return a negative population size estimate and unsensical
##' confidence interval values if n2 > n1 or Inf if n1 = n2.
##'
##' @references Moran, P. A. P. (1951). A mathematical theory of
##'     animal trapping. Biometrica, 38, 307-311.
##' 
##' Zippin, C. (1956). An evaluation of the removal method of
##' estimating animal populations. Biometrics, 12, 163-189.
##' 
##' Zippin, C. (1958). The removal method of population
##' estimation. Journal of Wildlife Management, 22, 82-90.
##' 
##' @title Moran-Zippin estimator for two consecutive removal events
##' @param n1 Number of individuals removed in the first event
##' @param n2 Number of individuals removed in the second event
##' @return a data frame containing the following values:
##' \itemize{
##' \item Estimate: estimated population size
##' \item sd: standard error of the Estimate
##' \item lcl: lower value of the confidence interval
##' \item ucl: upper value of confidence interval
##' }
##' @examples
##' MZest(84,61)
##' @export
##' @author Fer Arce
MZest<-function(n1,n2){
    if (n1 <= n2)
        warning('Your data is not suitable to use a Moran-Zipping estimator')
    N.pop<- (n1^2) /(n1-n2)
    Sd.pop<- (n1*n2*sqrt(n1+n2)) / (n1-n2)^2
    lcl<-N.pop-1.96*Sd.pop
    ucl<-N.pop+1.96*Sd.pop
    totC <- n1 + n2
    a <- data.frame(Estimate = N.pop,
                    sd = Sd.pop,lcl,ucl, totC)
    return(a)
}
