# Statistics CBW 2022

library("ggplot2")

# Central Limit Theorem

# Set random seed
set.seed(123)

# Simulate rolling a dice 10 times, and count the number of 3's observed
rbinom(1, 10, 1/6)

# Simulate 10 students rolling a dice 10 times, count the number of 3's you observed
rbinom(10, 10, 1/6) 

# Set the number of points and experiments
n <- 1000

# Initialize the vector of standardized sample means
means <- rep(0, n)
means_std <- rep(0, n)

# Sample from distribution
for (i in 1:n) {
      
      # Throw 10 times a dice
      x <- rbinom(n, 10, 0.5)
      
      # Compute mean value
      means[i] <- mean(x)
      means_std[i] <- sqrt(n) * mean(x)
      
}

# Plot means as histogram
hist(x,
     main = "Histogram of las experiment values",
     xlab = paste("Number of successes"),
     ylab = paste("Frequency"),
     col = "steelblue")

hist(means,
     main = "Histogram of means",
     xlab = paste("Mean values"),
     ylab = paste("Frequency"),
     col = "steelblue")

# Overlay normal distribution N(0,1)
curve(dnorm(means_std),
      lwd = 2,
      add = TRUE,
      col = "darkred")
