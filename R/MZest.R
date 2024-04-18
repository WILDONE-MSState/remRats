##' Estimation of population size after two consecutive renmoval
##' sessions using Moran-Zippin estimator
##'
##' Function to estimate the size of a population subject of two consecutive removal occasions. This should not be used, but it is kept here because sometimes only two night occassions are performed. More info of the formulas in:
##' Moran, P. A. P. (1951). A mathematical theory of animal trapping. Biometrica, 38, 307-311.
##' Otis, D. L., Burnham, K. P., White, G. C., and Anderson, D. R. (1978). Statistical inference from capture data on closed animal populations. Wildlife monographs, 62, 1-135.
##' Zippin, C. (1956). An evaluation of the removal method of estimating animal populations. Biometrics, 12, 163-189.
##' Zippin, C. (1958). The removal method of population estimation. Journal of Wildlife Management, 22, 82-90.
##' @title Moran-Zippin estimator for two consecutive trapping experiments
##' @param n1 numeric. First occasion catches
##' @param n2 numeric. Second occasion catches
##' @return an object of class data.frame returning the following values
##' @author Fer Arce
MZest<-function(n1,n2){
    N.pob<- (n1^2) /(n1-n2)
    Sd.pob<- (n1*n2*sqrt(n1+n2)) / (n1-n2)^2
    lcl<-N.pob-1.96*Sd.pob
    ucl<-N.pob+1.96*Sd.pob
    totC <- n1 + n2
    a <- data.frame(N.pob,Sd.pob,lcl,ucl, totC,
                    propL = round(100*totC / ucl),
                    propM = round(100*totC / N.pob),
                    propU = round(100*totC / lcl))
    return(a)
}
