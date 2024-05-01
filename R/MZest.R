##' This function generates an estimate of population size after two consecutive renmoval
##' sessions using Moran-Zippin estimator
##'
##' Function to estimate the size of a population subject of two consecutive removal occasions. This function is provided for reference, and its use is strongly discouraged. The use of function \code{removeMLE} should be the prefered approach as it adds more flexibility..
##'
##' @references Moran, P. A. P. (1951). A mathematical theory of animal trapping. Biometrica, 38, 307-311.
##' 
##' Zippin, C. (1956). An evaluation of the removal method of estimating animal populations. Biometrics, 12, 163-189.
##' 
##' Zippin, C. (1958). The removal method of population estimation. Journal of Wildlife Management, 22, 82-90.
##' @title Moran-Zippin estimator for two consecutive removal experiments
##' @param n1 Number of individuals removed in the first event
##' @param n2 Number of individuals removed in the second event
##' @return an object of \code{class data.frame} returning the following values: \cr
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
    N.pop<- (n1^2) /(n1-n2)
    Sd.pop<- (n1*n2*sqrt(n1+n2)) / (n1-n2)^2
    lcl<-N.pob-1.96*Sd.pob
    ucl<-N.pob+1.96*Sd.pob
    totC <- n1 + n2
    a <- data.frame(Estimate = N.pob,
                    sd = Sd.pob,lcl,ucl, totC)
    return(a)
}
