
####################mixpnorm###############################################

#' Title
#'
#' @param q 
#' @param mean1 
#' @param sd1 
#' @param mean2 
#' @param sd2 
#' @param mixprob 
#'
#' @returns
#' @export
#'
#' @examples
pnormmix <- function(q, mean1, sd1, mean2, sd2, mixprob) {
  p1 <- pnorm(q, mean = mean1, sd = sd1) # CDF of the first normal distribution
  p2 <- pnorm(q, mean = mean2, sd = sd2) # CDF of the second normal distribution
  return(mixprob * p1 + (1 - mixprob) * p2) # Mixture CDF
}

####################mixdnorm##############################################

#' Title
#'
#' @param x 
#' @param mean1 
#' @param sd1 
#' @param mean2 
#' @param sd2 
#' @param mixprob 
#'
#' @returns
#' @export
#'
#' @examples
dnormmix <- function(x, mean1, sd1, mean2, sd2, mixprob) {
  d1 <- dnorm(x, mean = mean1, sd = sd1) # PDF of the first normal distribution
  d2 <- dnorm(x, mean = mean2, sd = sd2) # PDF of the second normal distribution
  return(mixprob * d1 + (1 - mixprob) * d2) # Mixture PDF
}

####################mixqnorm###############################################

#' Title
#'
#' @param p 
#' @param mean1 
#' @param sd1 
#' @param mean2 
#' @param sd2 
#' @param mixprob 
#'
#' @returns
#' @export
#'
#' @examples
qnormmix <- function(p, mean1, sd1, mean2, sd2, mixprob) {
  # Function to compute the quantile for a given probability p
  qfun <- function(p) {
    if (p == 0) {
      return(-Inf) # p = 0 corresponds to -infinity
    } else if (p == 1) {
      return(Inf) # p = 1 corresponds to +infinity
    } else {
      # Use uniroot to find the root of the equation pnormmix(x) - p = 0
      root <- uniroot(function(x) pnormmix(x, mean1, sd1, mean2, sd2, mixprob) - p,
                      lower = -10, upper = 10)
      return(root$root) # The root is the quantile corresponding to p
    }
  }
  # Vectorize the quantile function for multiple p values
  Vectorize(qfun)(p)
}

####################mixrnorm###############################################
#' Title
#'
#' @param n 
#' @param mean1 
#' @param sd1 
#' @param mean2 
#' @param sd2 
#' @param mixprob 
#'
#' @returns
#' @export
#'
#' @examples
rnormmix <- function(n, mean1, sd1, mean2, sd2, mixprob) {
  # Randomly sample from the two normal distributions based on the mixing probability
  sample_from_first <- rbinom(n, 1, mixprob) # Decide which distribution to sample from
  x1 <- rnorm(sum(sample_from_first == 1), mean = mean1, sd = sd1) # Samples from the first nor
  mal
  x2 <- rnorm(sum(sample_from_first == 0), mean = mean2, sd = sd2) # Samples from the second no
  rmal
  return(c(x1, x2)) # Return the combined samples
}
