source('R/remSim.R')
source('R/genHistRMark.R')
source('R/removeMLE.R')
source('R/recRemMLE.R')

(pob1<-remSim(N=500,p=.15,j=5))

(simudata<-genHistRMark(pob1, 5))

class(simudata)


(pop.res1<-removeMLE(simudata))
## there seems to be issues when there is zero catches.
rec <- recRemMLE(pob1, events = 6)

library(mgcv)




summary(simudata)

## k tiener que ser como mucho 1 punto menos a el numero de noches
## al fional hay que llamar esta funcion d emanera que proyecte hasta que se cumpla el obnjetivo, usando la estima de la poblacion y el crecimiento en capturas.

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param Data an object of class 'recrem'
##' @param catches 
##' @param events numbeer of future steps to be projected
##' @param k smoothing parameter (as much as n-1)
##' @param rec an object of class 'recrem'
##' @return 
##' @author Fer Arce
projetCatches <- function(Data, catches,  events, k){    
    capts <- cumsum(pob1)
    n <- 1:length(pob1)
    model <- gam(rec$estimate~s(rec$n.occ, bs = 'cr',k = 3))
    model <- gam(capts~s(n, bs = 'cr',k = 4))
    test <- data.frame(n = 1:10)
    pred <- predict(model, test,type = 'response', se = TRUE)

    ## not finished
}

plot(model)
points(capts, type = 'b', pch = 19)

## if there is an increase between first and seccond,
## then it goes nuts, we should ignore those estimates


pob1 <- c(56, 58,50,47,42)

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

    
margins <- list(y = max(res$ucl))


## analize and evaluiate the outcome

breakPoint <- 75 # we want 75% of the population being sampled
target <- floor(res$estimate[events - 1]*(breakPoint/100))


## check if target has been matched
absCatches <- cumsum(pob1)
any(absCatches > target)



plot(cumsum(pob1), type = 'b', ylim = c(0, margins$y), panel.first = abline( h = target, col = 'firebrick', lwd = 5, lty = 2), cex = .5, pch = 19)
polygon(c(2:5, 5:2), c(res$lcl, rev(res$ucl)), col = adjustcolor('dodgerblue', alpha = .5))
points(2:5, res$estimate, type = 'l', lwd = 3.5, lty = 4, col = 'white')
points(2:5, res$estimate, type = 'l', lwd = 2.5, lty = 1, col = 'dodgerblue3')


