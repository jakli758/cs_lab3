f <- function(x){
  if (x > 0){
    return (120*x^5*exp(-x))
  }
  else{
    stop("Function undefined for x <= 0.")
  }
}
x <- seq(1,10,0.01)
x_app <- sapply(x,f)
plot(x, x_app)
# a.

set.seed(42)

# Metropolis-Hastings Algorithm
metropolis_normal <- function(n_iter = 10000, start = 1, proposal_sd = 0.1) {
  x <- numeric(n_iter)
  x[1] <- start
  accept <- 0
  
  for (t in 2:n_iter) {
    proposal <- rnorm(1, mean = x[t - 1], sd = proposal_sd)
    
    # f undefined for x <= 0
    if (proposal > 0) { 
      rate <- min(1, f(proposal) / f(x[t - 1]))
      if (runif(1) < rate) {
        x[t] <- proposal
        accept <- accept + 1
      } else {
        x[t] <- x[t - 1]
      }
    } else {
      x[t] <- x[t - 1]
    }
  }
  
  return(list(chain = x, acceptance_rate = accept / n_iter))
}

result_a <- metropolis_normal(n_iter=10000)
samples_a <- result_a$chain
acceptance_rate_a <- result_a$acceptance_rate

# Plot the chain and histogram
plot(samples_a, type = "l", main = "Trace Plot (Part a)", xlab = "Iteration", ylab = "X_t")
hist(samples_a, probability = TRUE, breaks = 50, main = "Histogram of Samples (Part a)", xlab = "X_t")
cat("Acceptance rate (Part a):", acceptance_rate_a, "\n")

# Very high burn-in period, compare with n_iter=100000 --> hist finally shows shape of target function




# b. 

metropolis_chisq <- function(n_iter = 10000, start = 1) {
  x <- numeric(n_iter)
  x[1] <- start
  accept <- 0
  
  for (t in 2:n_iter) {
    proposal <- rchisq(1, floor(x[t - 1] + 1))
    rate <- min(1, f(proposal) / f(x[t - 1]))
    if (runif(1) < rate) {
      x[t] <- proposal
      accept <- accept + 1
    } else {
      x[t] <- x[t - 1]
    }
  }
  
  list(samples = x, acceptance_rate = accept / n_iter)
}

# Run the algorithm
result_b <- metropolis_chisq()
samples_b <- result_b$samples
acceptance_rate_b <- result_b$acceptance_rate

# Plot the chain and histogram
plot(samples_b, type = "l", main = "Trace Plot (Part b)", xlab = "Iteration", ylab = "X_t")
hist(samples_b, probability = TRUE, breaks = 50, main = "Histogram of Samples (Part b)", xlab = "X_t")
cat("Acceptance rate (Part b):", acceptance_rate_b, "\n")



# c.
metropolis_gamma <- function(n_iter = 10000, start = 1, shape = 3, rate = 1) {
  x <- numeric(n_iter)
  x[1] <- start
  accept <- 0
  
  for (t in 2:n_iter) {
    proposal <- rgamma(1, shape = shape, rate = rate)
    alpha <- min(1, f(proposal) / f(x[t - 1]))
    if (runif(1) < alpha) {
      x[t] <- proposal
      accept <- accept + 1
    } else {
      x[t] <- x[t - 1]
    }
  }
  
  list(samples = x, acceptance_rate = accept / n_iter)
}

# Run the algorithm
result_c <- metropolis_gamma()
samples_c <- result_c$samples
acceptance_rate_c <- result_c$acceptance_rate

# Plot the chain and histogram
plot(samples_c, type = "l", main = "Trace Plot (Part c)", xlab = "Iteration", ylab = "X_t")
hist(samples_c, probability = TRUE, breaks = 50, main = "Histogram of Samples (Part c)", xlab = "X_t")
cat("Acceptance rate (Part c):", acceptance_rate_c, "\n")

###########




# Plot the histogram of samples
hist(samples_a[8000:10000], probability = TRUE, breaks = 50, 
     main = "Histogram of Samples with Target Density", 
     xlab = "X", col = rgb(0.2, 0.4, 0.6, 0.5), border = "white")



