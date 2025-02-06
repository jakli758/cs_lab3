# function

f <- function(x) {
  if (x < -1 | x > 1){
    return(0)
  }
  else if (-1 <= x & x <= 0) {
    return(x+1)
  }
  else {
    return (1-x)
  }
}



# plot function
x <- seq(-2,2,0.01)
y <- sapply(x, f)
plot(x,y, type="l")

# a. rejection sampling

g <- function(x) {
  if (x < -1 || x > 1){
    return(0)
  }
  else{
    return(1.15)
  }
}

e <- function(x) {
  alpha <- 0.9
  return (alpha*g(x))
}

y_env <-  sapply(x, e)
lines(x, y_env, col="red")

rejection_sampling <- function(n) {
  samples <- numeric(n)
  c <- 1
  while(c < n) {
    sample_Y <- runif(1, -1 ,1)
    sample_U <- runif(1, 0, 1)
    
    if(sample_U <= f(sample_Y)){
      samples[c] <- sample_Y
      c <- c + 1 
    }
  }
  return(samples)
}

set.seed(42)  
samples <- rejection_sampling(10000)
hist(samples, breaks=50,probability = TRUE, main = "Histogram of Samples with Rejection Sampling", xlab = "x")
lines(x, y, col="red")

# b. composition sampling

composition_sampling <- function(n) {
  samples <- numeric(n)
  c <- 1
  
  while (c < n){
    u <- runif(1)
    y <- 1 - sqrt(1 - u)
    
    # with prob 0.5 take Y or -Y for this sample
    if (runif(1) < 0.5) {
      samples[c] <- y
    } else {
      samples[c] <- -y
    }
    c <- c + 1
  }
  return(samples)
}

set.seed(42)  
samples <- composition_sampling(10000)
hist(samples, breaks=50,probability = TRUE, main = "Histogram of Samples with Composition Sampling", xlab = "x")
lines(x, y, col="red")


# c. two uniformly distributions
substract_sampling <- function(n){
  u1 <- runif(n)
  u2 <- runif(n)
  
  samples <- u1 - u2
  
  return(samples)
}

set.seed(42)  
samples <- substract_sampling(10000)
hist(samples, breaks=50,probability = TRUE, main = "Histogram of Samples", xlab = "x")
lines(x, y, col="red")

# d. comparison of methods