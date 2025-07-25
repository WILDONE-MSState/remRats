##' Maximum Likelihood Estimation for Removal Models
##'
##' This function estimates population size (\code{N}) and detection probability (\code{p})
##' from removal data using maximum likelihood. It also computes confidence intervals for both parameters
##' and the AIC for model evaluation.
##'
##' @details
##' The function assumes a closed population and constant detection probability across sampling occasions.
##' Given a vector of counts (\code{Data}) from a removal study, it fits the following model:
##'
##' \deqn{C_t \sim \text{Binomial}(R_t, p), \quad R_t = N - \sum_{j=1}^{t-1} C_j}
##'
##' The likelihood is given by:
##' \deqn{
##'   L(N, p) = \prod_{t=1}^{T} \binom{R_t}{C_t} p^{C_t} (1 - p)^{R_t - C_t}
##' }
##'
##' and the log-likelihood:
##' \deqn{
##'   \ell(N, p) = \sum_{t=1}^{T} \left[ \log \binom{R_t}{C_t} + C_t \log p + (R_t - C_t) \log(1 - p) \right]
##' }
##'
##' The MLE is obtained by profiling over \code{N} and optimizing over \code{p}. Confidence intervals are:
##' \itemize{
##'   \item CI for \eqn{p}: from the curvature (second derivative) of the profile likelihood (Wald approximation)
##'   \item CI for \eqn{N}: from profile likelihood with threshold \eqn{0.5 \cdot \chi^2_{1, \alpha}}
##' }
##'
##' AIC is computed as:
##' \deqn{\text{AIC} = 2k - 2 \log L}
##' where \eqn{k = 2} (parameters \code{N} and \code{p}).
##'
##' @title Multinomial MLE estimation
##' @param Data A vector of class \code{numeric} contianing the count of individuals removed in each occasion (e.g., \code{c(83, 42, 21)})
##' @param level numeric, level for confidence intervals (default = 0.95)
##' @param upperLimit Maximum additional value above \code{sum(Data)} to search for \code{N}
##'
##' @return An object of class \code{list} with:
##' \item{Nest}{MLE of the population size \code{N}}
##' \item{ci_N}{Confidence interval for \code{N} (profile likelihood)}
##' \item{mle_p}{MLE of detection probability \code{p}}
##' \item{ci_p}{Confidence interval for \code{p}}
##' \item{se_p}{Standard error for \code{p}}
##' \item{AIC}{Akaike Information Criterion for the fitted model}
##'
##' @author Fer Arce
##' @export

mleEst <- function(Data, level = 0.95, upperLimit = 1000) {
  T <- length(Data)
  Ntot <- sum(Data)
  
  # Step 1: Negative log-likelihood of full model
  nll <- function(N, Data) {
    N <- round(N)
    if (N < Ntot) return(Inf)

    Inll <- function(p) {
      remaining <- N
      ll <- 0
      for (i in 1:T) {
        ci <- Data[i]
        ll <- ll + dbinom(ci, remaining, p, log = TRUE)
        remaining <- remaining - ci
      }
      return(-ll)
    }

    opt <- optimize(Inll, c(1e-6, 1 - 1e-6))
    return(opt$objective)
  }

  # Step 2: MLE for N
  optN <- optimize(function(N) nll(N, Data),
                   interval = c(Ntot, Ntot + upperLimit),
                   tol = 1e-4)
  
  Nest <- round(optN$minimum)
  min_neglogL <- optN$objective

  # Step 3: MLE for p given N
  Inll <- function(p) {
    remaining <- Nest
    ll <- 0
    for (i in 1:T) {
      ci <- Data[i]
      ll <- ll + dbinom(ci, remaining, p, log = TRUE)
      remaining <- remaining - ci
    }
    return(-ll)
  }
  opt_p <- optimize(Inll, c(1e-6, 1 - 1e-6))
  mle_p <- opt_p$minimum

  # Step 4: Approximate SE and CI for p (from curvature of likelihood)
  h <- 1e-5
  d2 <- (Inll(mle_p + h) - 2 * Inll(mle_p) + Inll(mle_p - h)) / h^2
  se_p <- sqrt(1 / d2)

  z <- qnorm(1 - (1 - level) / 2)
  ci_p <- c(mle_p - z * se_p, mle_p + z * se_p)
  ci_p <- pmin(pmax(ci_p, 0), 1)  # constrain between 0 and 1

  # Step 5: CI for N via profile likelihood
  chi_sq_threshold <- qchisq(level, df = 1) / 2

  lower_N <- Nest
  while (lower_N > Ntot && 
         nll(lower_N - 1, Data) - min_neglogL <= chi_sq_threshold) {
    lower_N <- lower_N - 1
  }

  upper_N <- Nest
  while (nll(upper_N + 1, Data) - min_neglogL <= chi_sq_threshold) {
    upper_N <- upper_N + 1
  }

  logLik <- -min_neglogL
  k <- 2  # N and p
  AIC <- 2 * k - 2 * logLik
  
  return(list(
    Nest = Nest,
    ci_N = c(lower_N, upper_N),
    mle_p = mle_p,
    ci_p = ci_p,
    se_p = se_p,
    AIC = AIC
  ))
}



## fit_removal_DA_MLE <- function(y, covariate = NULL, cov = FALSE, M_factor = 2) {
##   n <- nrow(y)
##   T <- ncol(y)
##   M <- n * M_factor
  
##   # Augment data with 0 rows
##   y_aug <- rbind(y, matrix(0, nrow = M - n, ncol = T))
  
##   # Covariate
##   if (cov) {
##     if (is.null(covariate)) stop("Covariate vector required if cov = TRUE")
##     x <- c(covariate, rep(0, M - n))
##   } else {
##     x <- rep(0, M)
##   }
  
##   # Likelihood function
##   loglik <- function(params) {
##     psi <- plogis(params[1])             # inclusion probability
##     alpha <- params[2]                   # intercept for logit(p)
##     beta <- if (cov) params[3] else 0    # slope if covariate used
    
##     logL <- 0
##     for (i in 1:M) {
##       lp <- alpha + beta * x[i]
##       p <- plogis(lp)
      
##       # Prob of observed y[i,] if individual is real (z_i = 1)
##       prob_y_given_z1 <- prod(dbinom(y_aug[i, ], size = 1, prob = p))
      
##       # Prob if individual is fake (z_i = 0) — they must have all 0s
##       prob_y_given_z0 <- as.numeric(all(y_aug[i, ] == 0))  # 1 if all 0s, else 0
      
##       # Mixture
##       prob_i <- psi * prob_y_given_z1 + (1 - psi) * prob_y_given_z0
      
##       # Add to log likelihood
##       logL <- logL + log(prob_i + 1e-10)  # stability
##     }
##     return(-logL)  # negative log-likelihood for minimization
##   }

##   # Initial parameter guesses
##   init <- if (cov) c(qlogis(0.5), 0, 0) else c(qlogis(0.5), 0)

##   # Optimize
##   opt <- optim(init, loglik, method = "BFGS", hessian = TRUE)

##   # Extract estimates
##   est <- opt$par
##   se <- sqrt(diag(solve(opt$hessian)))
##   psi_hat <- plogis(est[1])
##   alpha_hat <- est[2]
##   beta_hat <- if (cov) est[3] else NA

##   list(
##     psi = psi_hat,
##     alpha = alpha_hat,
##     beta = beta_hat,
##     logLik = -opt$value,
##     N_hat = round(M * psi_hat),
##     SEs = se,
##     cov_used = cov
##   )
## }

