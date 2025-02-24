library(latex2exp)
library(ggplot2)

#' Function for performing an inverse gumbel transformation
#' 
#' @param u Random variable sampled from uniform distribution U~(0,1). Value we 
#' are applying the inverse gumbel cdf on.
#' @param mu Mean of the gumbel distribution also known as location parameter
#' @returns gumble-distributed random variable

inverse_transformation <- function(u, mu){
  c <- log(log(2))
  x <- mu + c -log(-log(u))
  return(x)
}

#' Function for sampling random variables from a gumbel distribution 
#' @param n number of sampled random variables
#' @param mu location parameter (median) of gumbel distribution
#' @returns vector of n random variables from gumbel distribution with mu as median
sample_gumbel <- function(n, mu){
  uniform_rvs <- runif(n, 0, 1)
  gumbel_rvs <- sapply(uniform_rvs, inverse_transformation, mu=mu)
  return(gumbel_rvs)
}

#' Function for performing hypothesis test given a list of random variables
#' (Tests if the median of the distribution is not 0)
#' @param gumbel_rvs vector of random variables
#' @returns p-value of test
hypothesis_test <- function(gumbel_rvs){
  x <- sum(ifelse(gumbel_rvs > 0, 1, 0))
  result <- binom.test(x, length(gumbel_rvs), p = 0.5, alternative = "greater")
  return(result$p.value)
}

#'Function for performing sign test given.
#'@param n number of samples to feature
#'@param mu location parameter of gimbel distribution for which we test
#'@param alpha significance level we use in the hypothesis test
#'@returns power value of sign_test
sign_test <- function(n, mu, alpha){
  rvs <- lapply(1:1000, function(i) sample_gumbel(n = n, mu = mu))
  p_values <- sapply(rvs, hypothesis_test)
  power <- mean(ifelse(p_values <= alpha, 1, 0))
  return(power)
}

n <- 13
alpha <- 0.05
mus <- seq(0, 2, by = 0.02)
result_list <- lapply(mus, function(mu) sign_test(n = n, mu = mu, alpha = alpha))
plot(x = mus, y = result_list, type = "b", xlab = TeX("$\\mu$ values"), ylab = "Power values", col = "blue", 
     main = TeX("Sign power values for gimbel distributions with varying $\\mu$"))

