# Question 2:

# a)  Function to plot the boundary
plot_boundary <- function(w, col="blue") {
  # Range of x1 values
  xv <- seq(-1, 1, by=0.01) * 1/sqrt(1 - w^2 / 4)  
  # Compute the corresponding x2 values
  lower_bound <- -(w*xv - sqrt(xv^2*(w^2-4)+4))/2
  upper_bound <- -(w*xv + sqrt(xv^2*(w^2-4)+4))/2
  
  
  plot(xv, xv, type="n", xlab=expression(x[1]), ylab=expression(x[2]), las=1,
       main=paste("Boundary of Bivariate Distribution for w =", w))
  
  lines(xv, lower_bound, lwd=2, col=col)
  lines(xv, upper_bound, lwd=2, col=col)
}

plot_boundary(1.9999)

# c) Gibbs sampling
# Helper function that samples one x-value given the other x-value

set.seed(1234) # reproducability
sample_dependent <- function(x_known, w) {
  lower_bound <- (-w * x_known - sqrt(x_known^2 * (w^2 - 4) + 4)) / 2
  upper_bound <- (-w * x_known + sqrt(x_known^2 * (w^2 - 4) + 4)) / 2
  runif(1, lower_bound, upper_bound) # random number between the bounds
}

# Gibbs Sampling function
gibbs_sampler <- function(n, w) {
  samples <- matrix(0, nrow=n, ncol=2)
  
  # Initial values (can be any valid point)
  x1 <- 0
  x2 <- 0
  
  for (i in 1:n) {
    x1 <- sample_dependent(x2, w)
    x2 <- sample_dependent(x1, w)
    samples[i, ] <- c(x1, x2)
  }
  
  return(samples)
}

n <- 1000
w <- 1.9999
samples <- gibbs_sampler(n, w)

# Plot the sampled points
plot(samples, col="blue", pch=20, xlab=expression(X[1]), ylab=expression(X[2]), main="Gibbs Sampling of Bivariate Uniform Distribution")

# e) Transformed boundary plotting
plot_boundary_transformed <- function(w, col="blue") {
  # Range of x1 values
  a_sq <- 4/(2-w)
  b <- sqrt(4/(2+w))
  a <- sqrt(a_sq)
  u1_seq <- seq(-a, a, length.out = 300)
  
  plot(u1_seq, u1_seq, type="n", xlab=expression(x[1]), ylab=expression(x[2]), las=1,
       main=paste("Boundary of Bivariate Distribution for w =", w))

  
  lower_bound <- -b*sqrt(1-u1_seq^2/a_sq)
  upper_bound <- b*sqrt(1-u1_seq^2/a_sq)
  lines(u1_seq, lower_bound , lwd=2, col=col)
  lines(u1_seq, upper_bound, lwd=2, col=col)
}

plot_boundary_transformed(1.9999)
