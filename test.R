source('R/remSim.R')
source('R/genHistRMark.R')
source('R/removeMLE.R')
source('R/recRemMLE.R')

(pob1<-remSim(N=500,p=.15,j=5, max.capt = 500))
## si maxcapt esta ausente, que sea igual al total de la pob

(simudata<-genHistRMark(pob1, 5))

print(simudata)


## generate a fake dataset of leprenauch

(pop.res1<-removeMLE(simudata))
u <- as.data.frame(pop.res1)
## there seems to be issues when there is zero catches.
rec <- recRemMLE(simudata, events = 6)


help(as.data.frame)
## install.packages('mra')

## install.packages('Rcapture')

## k tiener que ser como mucho 1 punto menos a el numero de noches
## al fional hay que llamar esta funcion d emanera que proyecte hasta que se cumpla el obnjetivo, usando la estima de la poblacion y el crecimiento en capturas.

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Fitting a gam model to cumulative numbers of removed individuals.
##' @param Data
##' @param catches 
##' @param events numbeer of future steps to be projected
##' @param k smoothing parameter (as much as n-1)
##' @return 
##' @author Fer Arce
##' @export
fitCatches <- function(Data, catches,  events, k){    
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


library(Rcapture)
data('hare')

desc <- descriptive(hare)
plot(desc)

head(hare)

closedp(hare)


u <- closedp.Mtb(hare)
help(closedp.Mtb)

data in matrix format
dfreq = FALSE

method = "BFGS"


X = hare

closedp.Mtb
############################################
  ## Validation des arguments fournis en entree
  Rcapture:::valid.one(dfreq,"logical")
  Xvalid<-Rcapture:::valid.X(X,dfreq)
    X <- Xvalid$X
    t <- Xvalid$t
  if (t < 3) stop("model Mtb cannot be fitted with 2 capture occasions")  
  ############################################

## necersitamos una matrix de datos de fiormato matrix, y el numero de ocasiones

Y <- Rcapture:::histfreq.t(X,dfreq=dfreq) #

  if (any(Y>170)) stop("model Mtb cannot be fitted with capture history frequencies larger than 170")  
  
  n <- sum(na.rm=TRUE,Y)
  histpos <- Rcapture:::histpos.t(t)
  nbcap <- rowSums(histpos)

## these are the names for time varyinbg parameters.
  pnames <- paste("p",1:t,sep="")
  cnames <- paste("c",2:t,sep="")


  ### Modele Mtb
  
  # matrices d'indicatices de non capture (wi=0) avant la premiere capture, i=1,...,t-1
  I1 <- matrix(c(0,0,1),ncol=1)  
  for(i in (3:t))
    I1 <- cbind(c(rep(0, 2^(i-1)), rep(1,((2^(i-1))-1))), 
                rbind(matrix(rep(0,(2^(i-1))*(i-2)),nrow=2^(i-1)),I1))
  # matrice d'indicatrices de la premiere capture
  I2 <- matrix(rep(0,(2^t-1)*t),ncol=t)  
  poscapt1<-rep(1,2^(t-1))
  for(i in 2:t) poscapt1 <- c(poscapt1,rep(i,2^(t-i)))
  for(i in 1:(2^t-1)) I2[i,poscapt1[i]] <- 1        
  # matrice d'indicatrices de capture (wi=1) apres la premiere capture
  I3 <- histpos.t(t)[,-1]  
  for(i in (2^(t-1)+1):(2^t-1)) 
    I3[i,1:(poscapt1[i]-1)]<-rep(0,length(I3[i,1:(poscapt1[i]-1)]))
  # matrice d'indicatrices de non-capture (wi=0) apres la premiere capture
  I4 <- 1-histpos.t(t)[,-1]  
  for(i in (2^(t-1)+1):(2^t-1)) 
    I4[i,1:(poscapt1[i]-1)]<-rep(0,length(I4[i,1:(poscapt1[i]-1)]))
  
  # valeurs initiales des parametres (N->n, tous les p -> 0.5 et ci=pi)
  init_loglinparam.Mtb <- as.vector(c(log(n),rep(0,t+1)))
  
  # Fonction de log-vraisemblance
ll_loglinparam.Mtb <- function(par,Y,I1,I2,I3,I4) {
    logmu <- par[1] +
        I1%*%log(1/(1+exp(par[2:t]))) + 
        I2%*%log(exp(par[2:(t+1)])/(1+exp(par[2:(t+1)]))) + 
        I3%*%log(exp(par[3:(t+1)]+par[t+2])/(1+exp(par[3:(t+1)]+par[t+2]))) + 
        I4%*%log(1/(1+exp(par[3:(t+1)]+par[t+2])))
                                        # Sortie : log-vraisemblance
    t(Y)%*%logmu - sum(na.rm=TRUE,exp(logmu)) - sum(na.rm=TRUE,log(factorial(Y))) 
}

                                        # Optimisation de la fonction de log-vraisemblance en fonction des parametres loglineaires
  optim.out <- Rcapture:::optim.call(par = init_loglinparam.Mtb, fn = ll_loglinparam.Mtb, method=method, Y = Y, I1 = I1, I2 = I2, I3 = I3, I4 = I4, control=list(fnscale = -1, maxit = 1000))


#############################################################################


  # Fonction de log-vraisemblance
ll_loglinparam.Mtb <- function(par,Y,I1,I2,I3,I4) {
    logmu <- par[1] +
        I1%*%log(1/(1+exp(par[2:t]))) + 
        I2%*%log(exp(par[2:(t+1)])/(1+exp(par[2:(t+1)]))) + 
        I3%*%log(exp(par[3:(t+1)]+par[t+2])/(1+exp(par[3:(t+1)]+par[t+2]))) + 
        I4%*%log(1/(1+exp(par[3:(t+1)]+par[t+2])))
                                        # Sortie : log-vraisemblance
    t(Y)%*%logmu - sum(na.rm=TRUE,exp(logmu)) - sum(na.rm=TRUE,log(factorial(Y))) 
}

                                        # Optimisation de la fonction de log-vraisemblance en fonction des parametres loglineaires
  optim.out <- Rcapture:::optim.call(par = init_loglinparam.Mtb, fn = ll_loglinparam.Mtb, method=method, Y = Y, I1 = I1, I2 = I2, I3 = I3, I4 = I4, control=list(fnscale = -1, maxit = 1000))







#############################################################################
















  # Si la commande a genere une erreur, on arrete l'execution de la fonction
  if (!is.null(optim.out$error))
    stop("error when fitting the model: ", optim.out$error)  
  
  # Statistiques d'ajustement du modele
  optimo <- optim.out$optimo
  logmu <- optimo$par[1] + I1%*%log(1/(1+exp(optimo$par[2:t]))) + 
    I2%*%log(exp(optimo$par[2:(t+1)])/(1+exp(optimo$par[2:(t+1)]))) + 
    I3%*%log(exp(optimo$par[3:(t+1)]+optimo$par[t+2])/(1+exp(optimo$par[3:(t+1)]+optimo$par[t+2]))) + 
    I4%*%log(1/(1+exp(optimo$par[3:(t+1)]+optimo$par[t+2])))
  se <- sqrt(exp(2*optimo$par[1])*solve(-optimo$hessian)[1,1]-exp(optimo$par[1]))
  dev <- 2*sum(na.rm=TRUE,Y*(pmax(log(Y),0)-logmu))
  df <- 2^t-1-(t+2)
  AIC <- -2*optimo$value + 2*(t+2)
  BIC <- -2*optimo$value + log(n)*(t+2)
  
  # Preparation des sorties
  infoFit <- getInfo(err = optim.out$error, warn = optim.out$warnings)
  tableau <- matrix(c(exp(optimo$par[1]),se,dev,df,AIC,BIC,infoFit),nrow=1)
  
  # Autres parametres
  parMtb <- matrix(c(exp(optimo$par[1]), exp(optimo$par[2:(t+1)])/(1+exp(optimo$par[2:(t+1)])),
                     exp(optimo$par[3:(t+1)]+optimo$par[t+2])/
                       (1+exp(optimo$par[3:(t+1)]+optimo$par[t+2]))),nrow=1)
  colnames(parMtb) <- c("N",pnames,cnames)


  # Preparation des sorties
  dimnames(tableau) <- list("Mtb",c("abundance","stderr","deviance","df","AIC","BIC","infoFit"))
  ans <- list(n=n, t=t, results=tableau, optim = optimo, optim.warn = optim.out$warnings, 
              parMtb=parMtb)
  class(ans) <- "closedp.Mtb"
  ans














data('hare')

desc <- descriptive(hare)
plot(desc)

head(hare)

closedp(hare)











# Define the negative log-likelihood function
negLogLik <- function(params, data) {
  N <- round(params[1])  # Ensuring N is an integer
  p <- params[2]
  
  observed <- rowSums(data)  # Total number of captures per individual
  K <- ncol(data)            # Number of capture occasions
  n <- nrow(data)            # Number of captured individuals
  
  if (N < n || p <= 0 || p >= 1) {
    return(Inf)  # Return a large number if parameters are not feasible
  } else {
    # Log likelihood
    logL <- lgamma(N + 1) - lgamma(N - n + 1) - lgamma(n + 1)
    logL <- logL + sum(observed * log(p) + (K - observed) * log(1 - p))
    return(-logL)  # Return negative log likelihood for minimization
  }
}

# Function to fit the CMR model using MLE with optim
fitCMR <- function(data) {
  n <- nrow(data)           # Number of captured individuals
  maxN <- n * 20             # Set a reasonable maximum for N based on your understanding
  
  # Initial parameter estimates
  initParams <- c(n, 0.5)  # Starting guesses for N and p
  
  # Optimizing using the "L-BFGS-B" method to handle bounds
  fit <- optim(initParams, negLogLik, method = "L-BFGS-B", lower = c(n, 0.01), upper = c(maxN, 0.99), data = data)
  
  return(list(N = round(fit$par[1]), p = fit$par[2], convergence = fit$convergence, value = -fit$value))
}

# Example usage
# Assuming `capture_matrix` is your matrix of 0s and 1s
# capture_matrix <- matrix(c(...), nrow = number_of_individuals, ncol = number_of_sampling_times, byrow = TRUE)

result <- fitCMR(hare)
print(result)





# Define the negative log-likelihood function with time-varying capture probabilities
negLogLikTimeVarying <- function(params, data) {
  N <- round(params[1])  # Ensuring N is an integer
  ps <- params[-1]       # Capture probabilities for each time period
  
  if (N < nrow(data) || any(ps <= 0) || any(ps >= 1)) {
    return(Inf)  # Return a large number if parameters are not feasible
  } else {
    logL <- lgamma(N + 1) - lgamma(N - nrow(data) + 1) - lgamma(nrow(data) + 1)
    observed <- rowSums(data)  # Total number of captures per individual
    for (i in 1:ncol(data)) {
      logL <- logL + sum(data[, i] * log(ps[i]) + (1 - data[, i]) * log(1 - ps[i]))
    }
    return(-logL)  # Return negative log likelihood for minimization
  }
}

# Function to fit the CMR model with time-varying p using MLE with optim
fitCMRTimeVarying <- function(data) {
  maxN <- nrow(data) * 2       # Set a reasonable maximum for N
  initParams <- c(nrow(data), rep(0.5, ncol(data)))  # Starting guesses for N and p
  
  # Optimizing using the "L-BFGS-B" method to handle bounds
  bounds <- list(lower = c(nrow(data), rep(0.01, ncol(data))), upper = c(maxN, rep(0.99, ncol(data))))
  fit <- optim(initParams, negLogLikTimeVarying, method = "L-BFGS-B", lower = bounds$lower, upper = bounds$upper, data = data)
  
  return(list(N = round(fit$par[1]), ps = fit$par[-1], convergence = fit$convergence, value = -fit$value))
}

# Example usage
# capture_matrix <- matrix(c(...), nrow = number_of_individuals, ncol = number_of_sampling_times, byrow = TRUE)
result <- fitCMRTimeVarying(hare)
print(result)







#############################################################################
# Define the negative log-likelihood function with time-varying capture probabilities
negLogLikTimeVaryingBehavior <- function(params, data) {
  N <- round(params[1])  # Ensuring N is an integer
  half_index <- (length(params) - 1) / 2 + 1
  ps <- params[2:half_index]  # Capture probabilities before first capture
  qs <- params[(half_index + 1):length(params)]  # Capture probabilities after first capture
  
  if (N < nrow(data) || any(ps <= 0) || any(ps >= 1) || any(qs <= 0) || any(qs >= 1)) {
    return(Inf)  # Return a large number if parameters are not feasible
  } else {
    logL <- lgamma(N + 1) - lgamma(N - nrow(data) + 1) - lgamma(nrow(data) + 1)
    first_capture <- apply(data, 1, function(x) min(which(x == 1), na.rm = TRUE, default = NA))
    
    for (i in 1:ncol(data)) {
      uncaptured_logL <- sum(data[, i] == 1 & is.na(first_capture), na.rm = TRUE) * log(ps[i])
      recaptured_logL <- sum(data[, i] == 1 & !is.na(first_capture) & first_capture < i, na.rm = TRUE) * log(qs[i])
      no_capture_logL <- sum(data[, i] == 0, na.rm = TRUE) * log(1 - c(ps[i], qs[i])[1 + (first_capture < i)])
      
      logL <- logL + uncaptured_logL + recaptured_logL + no_capture_logL
    }
    
    return(-logL)  # Return negative log likelihood for minimization
  }
}



# Define the negative log-likelihood function with time-varying capture probabilities
negLogLikTimeVaryingBehavior <- function(params, data) {
  N <- round(params[1])  # Ensuring N is an integer
  half_index <- (length(params) - 1) / 2 + 1
  ps <- params[2:half_index]  # Capture probabilities before first capture
  qs <- params[(half_index + 1):length(params)]  # Capture probabilities after first capture
  
  if (N < nrow(data) || any(ps <= 0) || any(ps >= 1) || any(qs <= 0) || any(qs >= 1)) {
    return(Inf)  # Return a large number if parameters are not feasible
  } else {
    logL <- lgamma(N + 1) - lgamma(N - nrow(data) + 1) - lgamma(nrow(data) + 1)
    first_capture <- apply(data, 1, function(x) which(x == 1)[1] )

    for (i in 1:ncol(data)) {
      uncaptured_indices = is.na(first_capture)
      recaptured_indices = !uncaptured_indices & first_capture < i
      
      uncaptured_logL = ifelse(any(uncaptured_indices), sum(data[uncaptured_indices, i] * log(ps[i])), 0)
      recaptured_logL = ifelse(any(recaptured_indices), sum(data[recaptured_indices, i] * log(qs[i])), 0)
      no_capture_logL = sum(data[, i] == 0) * log(1 - c(ps[i], qs[i])[1 + (first_capture < i & !is.na(first_capture))])

      logL <- logL + uncaptured_logL + recaptured_logL + no_capture_logL
    }
    
    return(-logL)  # Return negative log likelihood for minimization
  }
}

# Continue using the rest of the setup as previously defined...





# Function to fit the CMR model with time-varying behavior using MLE with optim
fitCMRTimeVaryingBehavior <- function(data) {
  maxN <- nrow(data) * 2       # Set a reasonable maximum for N
  num_times <- ncol(data)
  initParams <- c(nrow(data), rep(0.5, 2 * num_times))  # Starting guesses for N, ps, and qs
  
  # Optimizing using the "L-BFGS-B" method to handle bounds
  bounds <- list(lower = c(nrow(data), rep(0.01, 2 * num_times)), upper = c(maxN, rep(0.99, 2 * num_times)))
  fit <- optim(initParams, negLogLikTimeVaryingBehavior, method = "L-BFGS-B", lower = bounds$lower, upper = bounds$upper, data = data)
  
  return(list(N = round(fit$par[1]), ps = fit$par[2:(num_times + 1)], qs = fit$par[(num_times + 2):(2 * num_times + 1)], convergence = fit$convergence, value = -fit$value))
}

# Example usage
# capture_matrix <- matrix(c(...), nrow = number_of_individuals, ncol = number_of_sampling_times, byrow = TRUE)
result <- fitCMRTimeVaryingBehavior(hare)
print(result)








#################
################
# Define the negative log-likelihood function with time-varying capture probabilities
negLogLikTimeVaryingBehavior <- function(params, data) {
  N <- round(params[1])  # Ensuring N is an integer
  ps <- params[2:(ncol(data) + 1)]  # Capture probabilities for each capture occasion
  qs <- c(ps[1], params[(ncol(data) + 2):length(params)])  # Adjusted to include a placeholder for the first period

  if (N < nrow(data) || any(ps <= 0) || any(ps >= 1) || any(qs <= 0) || any(qs >= 1)) {
    return(Inf)  # Return a large number if parameters are not feasible
  } else {
    logL <- lgamma(N + 1) - lgamma(N - nrow(data) + 1) - lgamma(nrow(data) + 1)
    first_capture <- apply(data, 1, function(x) which(x == 1)[1] )

    for (i in 1:ncol(data)) {
      uncaptured_indices = is.na(first_capture)
      recaptured_indices = !uncaptured_indices & first_capture < i

      uncaptured_logL = ifelse(any(uncaptured_indices), sum(data[uncaptured_indices, i] * log(ps[i])), 0)
      recaptured_logL = ifelse(any(recaptured_indices), sum(data[recaptured_indices, i] * log(qs[i])), 0)
      no_capture_logL = sum(data[, i] == 0) * log(1 - c(ps[i], qs[i])[1 + (first_capture < i & !is.na(first_capture))])

      logL <- logL + uncaptured_logL + recaptured_logL + no_capture_logL
    }

    return(-logL)  # Return negative log likelihood for minimization
  }
}

# Function to fit the CMR model with time-varying behavior using MLE with optim
fitCMRTimeVaryingBehavior <- function(data) {
  maxN <- nrow(data) * 2       # Set a reasonable maximum for N
  num_times <- ncol(data)
  initParams <- c(nrow(data), rep(0.5, num_times + num_times - 1))  # Starting guesses for N, ps, and one less qs

  # Optimizing using the "L-BFGS-B" method to handle bounds
  bounds <- list(lower = c(nrow(data), rep(0.01, num_times + num_times - 1)), upper = c(maxN, rep(0.99, num_times + num_times - 1)))
  fit <- optim(initParams, negLogLikTimeVaryingBehavior, method = "L-BFGS-B", lower = bounds$lower, upper = bounds$upper, data = data)

  return(list(N = round(fit$par[1]), ps = fit$par[2:(num_times + 1)], qs = c(fit$par[num_times + 1], fit$par[(num_times + 2):(2 * num_times)]), convergence = fit$convergence, value = -fit$value))
}

# Example usage
# capture_matrix <- matrix(c(...), nrow = number_of_individuals, ncol = number_of_sampling_times, byrow = TRUE)
result <- fitCMRTimeVaryingBehavior(hare)
# print(result)



#############3
##############

# Define the negative log-likelihood function with constant capture probabilities p and q
negLogLikConstantPQ <- function(params, hare) {
  N <- round(params[1])  # Ensuring N is an integer
  p <- params[2]         # Constant capture probability before first capture
  q <- params[3]         # Constant capture probability after first capture

  if (N < nrow(data) || p <= 0 || p >= 1 || q <= 0 || q >= 1) {
    return(Inf)  # Return a large number if parameters are not feasible
  } else {
    logL <- lgamma(N + 1) - lgamma(N - nrow(data) + 1) - lgamma(nrow(data) + 1)
    first_capture <- apply(hare, 1, function(x) which(x == 1)[1] )

    for (i in 1:ncol(hare)) {
      uncaptured_indices = is.na(first_capture)
      recaptured_indices = !uncaptured_indices & first_capture < i

      uncaptured_logL = ifelse(any(uncaptured_indices), sum(data[uncaptured_indices, i] * log(p)), 0)
      recaptured_logL = ifelse(any(recaptured_indices), sum(data[recaptured_indices, i] * log(q)), 0)
      no_capture_logL = sum(hare[, i] == 0) * log(1 - ifelse(is.na(first_capture) | first_capture >= i, p, q))

      logL <- logL + uncaptured_logL + recaptured_logL + no_capture_logL
    }

    return(-logL)  # Return negative log likelihood for minimization
  }
}

# Function to fit the CMR model using MLE with optim
fitCMRConstantPQ <- function(hare) {
  maxN <- nrow(hare) * 20       # Set a reasonable maximum for N
  initParams <- c(nrow(hare), 0.5, 0.5)  # Starting guesses for N, p, and q

  # Optimizing using the "L-BFGS-B" method to handle bounds
  fit <- optim(initParams, negLogLikConstantPQ, method = "L-BFGS-B", data = hare)

  return(list(N = round(fit$par[1]), p = fit$par[2], q = fit$par[3], convergence = fit$convergence, value = -fit$value))
}

# Example usage
# capture_matrix <- matrix(c(...), nrow = number_of_individuals, ncol = number_of_sampling_times, byrow = TRUE)
result <- fitCMRConstantPQ(hare)
print(result)




# Define the negative log-likelihood function with constant capture probabilities p and q
negLogLikConstantPQ <- function(params, data) {
  N <- round(params[1])  # Ensuring N is an integer
  p <- params[2]         # Constant capture probability before first capture
  q <- params[3]         # Constant capture probability after first capture

  if (N < nrow(data) || p <= 0 || p >= 1 || q <= 0 || q >= 1) {
    return(Inf)  # Return a large number if parameters are not feasible
  } else {
    logL <- lgamma(N + 1) - lgamma(N - nrow(data) + 1) - lgamma(nrow(data) + 1)
    first_capture <- apply(data, 1, function(x) which(x == 1)[1] )

    # Initialize the log likelihood sum
    log_likelihood_sum <- 0

    for (i in 1:ncol(data)) {
      uncaptured_indices = is.na(first_capture)
      recaptured_indices = !uncaptured_indices & first_capture < i

      # Calculate the likelihood contributions for each case
      uncaptured_logL = ifelse(any(uncaptured_indices), sum(log(p) * data[uncaptured_indices, i]), 0)
      recaptured_logL = ifelse(any(recaptured_indices), sum(log(q) * data[recaptured_indices, i]), 0)
      no_capture_logL = sum(log(1 - ifelse(is.na(first_capture) | first_capture >= i, p, q)) * (data[, i] == 0))

      # Add contributions to the total log likelihood
      log_likelihood_sum <- log_likelihood_sum + uncaptured_logL + recaptured_logL + no_capture_logL
    }

    # Return the negative of the summed log likelihood
    return(-log_likelihood_sum)
  }
}

# Continue with the previous setup of the function to fit the model...


# Define the negative log-likelihood function with time-varying capture probabilities
negLogLikTimeVaryingPQ <- function(params, data) {
  N <- round(params[1])  # Ensuring N is an integer
  num_periods <- ncol(data)
  ps <- params[2:(num_periods + 1)]  # Capture probabilities for each time period before first capture
  qs <- params[(num_periods + 2):(2 * num_periods + 1)]  # Capture probabilities for each time period after first capture

  if (N < nrow(data) || any(ps <= 0) || any(ps >= 1) || any(qs <= 0) || any(qs >= 1)) {
    return(Inf)  # Return a large number if parameters are not feasible
  } else {
    logL <- lgamma(N + 1) - lgamma(N - nrow(data) + 1) - lgamma(nrow(data) + 1)
    first_capture <- apply(data, 1, function(x) which(x == 1)[1])

    log_likelihood_sum <- 0

    for (i in 1:num_periods) {
      uncaptured_indices = is.na(first_capture)
      recaptured_indices = !uncaptured_indices & first_capture < i

      # Calculate the likelihood contributions for each case
      uncaptured_logL = ifelse(any(uncaptured_indices), sum(log(ps[i]) * data[uncaptured_indices, i]), 0)
      recaptured_logL = ifelse(any(recaptured_indices), sum(log(qs[i]) * data[recaptured_indices, i]), 0)
      no_capture_logL = sum(log(1 - ifelse(is.na(first_capture) | first_capture >= i, ps[i], qs[i])) * (data[, i] == 0))

      # Add contributions to the total log likelihood
      log_likelihood_sum <- log_likelihood_sum + uncaptured_logL + recaptured_logL + no_capture_logL
    }

    return(-log_likelihood_sum)  # Return negative log likelihood for minimization
  }
}

# Function to fit the CMR model using MLE with optim
fitCMRTimeVaryingPQ <- function(data) {
  num_periods <- ncol(data)
  maxN <- nrow(data) * 2       # Set a reasonable maximum for N
  initParams <- c(nrow(data), rep(0.5, num_periods * 2))  # Starting guesses for N, ps, and qs

  # Optimizing using the "L-BFGS-B" method to handle bounds
  bounds_lower <- c(nrow(data), rep(0.01, num_periods * 2))
  bounds_upper <- c(maxN, rep(0.99, num_periods * 2))
  fit <- optim(initParams, negLogLikTimeVaryingPQ, method = "L-BFGS-B", lower = bounds_lower, upper = bounds_upper, data = data)

  return(list(N = round(fit$par[1]), ps = fit$par[2:(num_periods + 1)], qs = fit$par[(num_periods + 2):(2 * num_periods + 1)], convergence = fit$convergence, value = -fit$value))
}

# Example usage
                                        # capture_matrix <- matrix(c(...), nrow = number_of_individuals, ncol = number_of_sampling_times, byrow = TRUE)
result <- fitCMRTimeVaryingPQ(hare)
print(result)




# Define the negative log-likelihood function with time-varying capture probabilities
negLogLikTimeVaryingPQ <- function(params, data) {
  N <- round(params[1])  # Ensuring N is an integer
  num_periods <- ncol(data)
  ps <- params[2:(num_periods + 1)]  # Capture probabilities before first capture for each period
  qs <- params[(num_periods + 2):(2 * num_periods + 1)]  # Capture probabilities after first capture for each period

  if (N < nrow(data) || any(ps <= 0) || any(ps >= 1) || any(qs <= 0) || any(qs >= 1)) {
    return(Inf)  # Infeasible parameters result in an infinite log-likelihood penalty
  } else {
    logL <- lgamma(N + 1) - lgamma(N - nrow(data) + 1) - lgamma(nrow(data) + 1)
    first_capture <- apply(data, 1, function(x) min(which(x == 1), na.rm = TRUE, default = NA))

    log_likelihood_sum <- 0

    for (i in 1:num_periods) {
      uncaptured_indices = is.na(first_capture)
      recaptured_indices = !uncaptured_indices & first_capture < i

      # Calculate the likelihood contributions for each case
      uncaptured_logL = if(any(uncaptured_indices)) sum(log(ps[i]) * data[uncaptured_indices, i]) else 0
      recaptured_logL = if(any(recaptured_indices)) sum(log(qs[i]) * data[recaptured_indices, i]) else 0
      no_capture_logL = sum(log(1 - ifelse(is.na(first_capture) | first_capture >= i, ps[i], qs[i])) * (data[, i] == 0))

      # Add contributions to the total log likelihood
      log_likelihood_sum <- log_likelihood_sum + uncaptured_logL + recaptured_logL + no_capture_logL
    }

    return(-log_likelihood_sum)  # Return negative log likelihood for minimization
  }
}

# Function to fit the CMR model using MLE with optim
fitCMRTimeVaryingPQ <- function(data) {
  num_periods <- ncol(data)
  maxN <- nrow(data) * 2
  initParams <- c(nrow(data), rep(0.5, num_periods * 2))  # Initial guesses for N, ps, and qs

  # Optimizing using "L-BFGS-B" method to handle bounds
  bounds_lower <- c(nrow(data), rep(0.01, num_periods * 2))
  bounds_upper <- c(maxN, rep(0.99, num_periods * 2))
  fit <- optim(initParams, negLogLikTimeVaryingPQ, method = "L-BFGS-B", lower = bounds_lower, upper = bounds_upper, data = data)

  return(list(N = round(fit$par[1]), ps = fit$par[2:(num_periods + 1)], qs = fit$par[(num_periods + 2):(2 * num_periods + 1)], convergence = fit$convergence, value = -fit$value))
}

# Example usage
# capture_matrix <- matrix(c(...), nrow = number_of_individuals, ncol = number_of_sampling_times, byrow = TRUE)
result <- fitCMRTimeVaryingPQ(hare)
print(result)




# Define the negative log-likelihood function with time-varying capture probabilities
negLogLikTimeVaryingPC <- function(params, data) {
  N <- round(params[1])  # Ensuring N is an integer
  num_periods <- ncol(data)
  ps <- params[2:(num_periods + 1)]  # Capture probabilities before first capture for each period
  cs <- params[(num_periods + 2):(2 * num_periods + 1)]  # Capture probabilities after first capture for each period (using 'c' for subsequent captures)

  if (N < nrow(data) || any(ps <= 0) || any(ps >= 1) || any(cs <= 0) || any(cs >= 1)) {
    return(Inf)  # Infeasible parameters result in an infinite log-likelihood penalty
  } else {
    logL <- lgamma(N + 1) - lgamma(N - nrow(data) + 1) - lgamma(nrow(data) + 1)
    first_capture <- apply(data, 1, function(x) min(which(x == 1), na.rm = TRUE, default = NA))

    log_likelihood_sum <- 0

    for (i in 1:num_periods) {
      uncaptured_indices = is.na(first_capture)
      recaptured_indices = !uncaptured_indices & first_capture < i

      uncaptured_logL = if(any(uncaptured_indices)) sum(log(ps[i]) * data[uncaptured_indices, i]) else 0
      recaptured_logL = if(any(recaptured_indices)) sum(log(cs[i]) * data[recaptured_indices, i]) else 0
      no_capture_logL = sum(log(1 - ifelse(is.na(first_capture) | first_capture >= i, ps[i], cs[i])) * (data[, i] == 0))

      log_likelihood_sum <- log_likelihood_sum + uncaptured_logL + recaptured_logL + no_capture_logL
    }

    return(-log_likelihood_sum)  # Return negative log likelihood for minimization
  }
}

# Function to fit the CMR model using MLE with optim
fitCMRTimeVaryingPC <- function(data) {
  num_periods <- ncol(data)
  maxN <- nrow(data) * 2
  initParams <- c(nrow(data), rep(0.5, num_periods * 2))  # Initial guesses for N, ps, and cs

  # Optimizing using "L-BFGS-B" method to handle bounds
  bounds_lower <- c(nrow(data), rep(0.01, num_periods * 2))
  bounds_upper <- c(maxN, rep(0.99, num_periods * 2))
  fit <- optim(initParams, negLogLikTimeVaryingPC, method = "L-BFGS-B", lower = bounds_lower, upper = bounds_upper, data = data)

  return(list(N = round(fit$par[1]), ps = fit$par[2:(num_periods + 1)], cs = fit$par[(num_periods + 2):(2 * num_periods + 1)], convergence = fit$convergence, value = -fit$value))
}

# Example usage
# capture_matrix <- matrix(c(...), nrow = number_of_individuals, ncol = number_of_sampling_times, byrow = TRUE)
result <- fitCMRTimeVaryingPC(hare)
print(result)


###
###
###


# Define the negative log-likelihood function with time-varying capture probabilities
negLogLikTimeVaryingPC <- function(params, data) {
  N <- round(params[1])  # Ensuring N is an integer
  num_periods <- ncol(data)
  ps <- params[2:(num_periods + 1)]  # Capture probabilities before first capture for each period
  cs <- params[(num_periods + 2):(2 * num_periods + 1)]  # Capture probabilities after first capture for each period

  if (N < nrow(data) || any(ps <= 0) || any(ps >= 1) || any(cs <= 0) || any(cs >= 1)) {
    return(Inf)  # Infeasible parameters result in an infinite log-likelihood penalty
  } else {
    logL <- lgamma(N + 1) - lgamma(N - nrow(data) + 1) - lgamma(nrow(data) + 1)
    first_capture <- apply(data, 1, function(x) which(x == 1)[1])

    log_likelihood_sum <- 0

    for (i in 1:num_periods) {
      uncaptured_indices = is.na(first_capture)
      recaptured_indices = !uncaptured_indices & first_capture < i

      uncaptured_logL = if(any(uncaptured_indices)) sum(log(ps[i]) * data[uncaptured_indices, i]) else 0
      recaptured_logL = if(any(recaptured_indices)) sum(log(cs[i]) * data[recaptured_indices, i]) else 0
      no_capture_logL = sum(log(1 - ifelse(is.na(first_capture) | first_capture >= i, ps[i], cs[i])) * (data[, i] == 0))

      log_likelihood_sum <- log_likelihood_sum + uncaptured_logL + recaptured_logL + no_capture_logL
    }

    return(-log_likelihood_sum)  # Return negative log likelihood for minimization
  }
}

# Function to fit the CMR model using MLE with optim
fitCMRTimeVaryingPC <- function(data) {
  num_periods <- ncol(data)
  maxN <- max(nrow(data) * 2, 100)  # Increasing this might help if true N is higher than twice the number of captures
  minN <- nrow(data)  # Minimum N could be the number of unique captures
  initParams <- c(maxN / 2, rep(0.5, num_periods * 2))  # More balanced initial guess for N

  # Optimizing using "L-BFGS-B" method to handle bounds
  bounds_lower <- c(minN, rep(0.01, num_periods * 2))
  bounds_upper <- c(maxN, rep(0.99, num_periods * 2))
  fit <- optim(initParams, negLogLikTimeVaryingPC, method = "L-BFGS-B", lower = bounds_lower, upper = bounds_upper, data = data)

  # Diagnostic printouts for parameter evolution
  cat("Optimization Convergence:", fit$convergence, "\n")
  cat("Final Parameter Estimates:", fit$par, "\n")

  return(list(N = round(fit$par[1]), ps = fit$par[2:(num_periods + 1)], cs = fit$par[(num_periods + 2):(2 * num_periods + 1)], convergence = fit$convergence, value = -fit$value))
}

# Example usage
# capture_matrix <- matrix(c(...), nrow = number_of_individuals, ncol = number_of_sampling_times, byrow = TRUE)
result <- fitCMRTimeVaryingPC(hare)
# print(result)
