box_muller <- function(input_vec){
  u_1 <- input_vec[1]
  u_2 <- input_vec[2]
  z_1 <- sqrt(-2*log(u_1))*cos(2*pi*u_2)
  z_2 <- sqrt(-2*log(u_1))*sin(2*pi*u_2)
  return(c(z_1, z_2))
}

univariate_vector <- function(){
  return(c(runif(1), runif(1))) 
}

start_time <- proc.time()
for(i in (1:100)){
  uniform_vec <- univariate_vector() * 0.6
  normal_vec <- box_muller(uniform_vec)
}
end_time <- proc.time()

print(end_time - start_time)

start_time <- proc.time()

n <- 10000000  # Number of vectors


x_1 <- rnorm(0.6, mean = 0, sd = sqrt(0.6))
x_2 <- rnorm(0.6, mean = 0, sd = sqrt(0.6))

# Generate independent normal samples with mean 0 and variance 0.6


end_time <- proc.time()

print(end_time - start_time)

library(mvtnorm)  # For multivariate normal sampling
set.seed(42)      # For reproducibility

n <- 1000  # Number of samples

# Define means
mu1 <- c(0, 0)
mu2 <- c(1.5, 1.2)

# Define covariance matrices
sigma1 <- matrix(c(0.6, 0, 0, 0.6), nrow = 2)
sigma2 <- matrix(c(0.5, 0, 0, 0.5), nrow = 2)

# Randomly assign samples to one of the two distributions (50% probability)
component <- rbinom(n, size = 1, prob = 0.5)

# Generate samples based on the assigned component
samples <- matrix(0, nrow = n, ncol = 2)
for (i in 1:n) {
  if (component[i] == 0) {
    samples[i, 1] <- rnorm(1, mean = mu1[1], sd = sqrt(sigma1[1,1]))
    samples[i, 2] <- rnorm(1, mean = mu1[2], sd = sqrt(sigma1[2,2]))
  } else {
    samples[i, 1] <- rnorm(1, mean = mu2[1], sd = sqrt(sigma2[1,1]))
    samples[i, 2] <- rnorm(1, mean = mu2[2], sd = sqrt(sigma2[2,2]))
    
  }
}

# Plot the generated samples
plot(samples, col = ifelse(component == 0, "blue", "red"), pch = 16,
     main = "Scatter Plot of Bivariate Normal Mixture",
     xlab = expression(x[1]), ylab = expression(x[2]))
legend("topright", legend = c("Component 1 (0,0)", "Component 2 (1.5,1.2)"), 
       col = c("blue", "red"), pch = 16)
