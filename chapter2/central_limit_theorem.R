# LMS Introduction to statistics & hypothesis testing
# Jesús Urtasun Elizari - LMS Bioinformatics
# November 2023

library("ggplot2")

# Chapter 2. The central limit theorem

# i) Small sample size .......................................................

# Sample size and number of repetitions
n = 5
reps <- 1000

# Initialize the vector of sample means and std sample means
sample_mean <- rep(0, reps)
std_sample_mean <- rep(0, reps)

# Loop over repetitions
for (i in 1:reps) {
      x <- rbinom(n, 1, 0.5)
      sample_mean[i] <- mean(x)
      std_sample_mean[i] <- sqrt(n) * (mean(x) - 0.5) / 0.5
}

# Histogram of mean values
hist(std_sample_mean,
     col = "steelblue",
     freq = FALSE,
     breaks = 40,
     xlim = c(-3, 3),
     ylim = c(0, 0.8),
     xlab = paste("n =", n),
     main = "")

# Overlay normal distribution N(0,1)
curve(dnorm(x),
      lwd = 2,
      col = "darkred",
      add = TRUE)

# ii) Large sample size .......................................................

# Sample size and number of repetitions
n = 50
reps <- 1000

# Initialize the vector of sample means and std sample means
sample_mean <- rep(0, reps)
std_sample_mean <- rep(0, reps)

# Loop over repetitions
for (i in 1:reps) {
      x <- rbinom(n, 1, 0.5)
      sample_mean[i] <- mean(x)
      std_sample_mean[i] <- sqrt(n) * (mean(x) - 0.5) / 0.5
}

# Histogram of mean values
hist(std_sample_mean,
     col = "steelblue",
     freq = FALSE,
     breaks = 40,
     xlim = c(-3, 3),
     ylim = c(0, 0.8),
     xlab = paste("n =", n),
     main = "")

# Overlay normal distribution N(0,1)
curve(dnorm(x),
      lwd = 2,
      col = "darkred",
      add = TRUE)

# iii) Iterate over sample sized ..............................................

# Set sample sizes
sample.sizes <- c(5, 20, 75, 100)

# Subdivide plot panel
par(mfrow = c(2, 2))

# Iterate over sample sizes
for (n in sample.sizes) {
      
      # Initialize the vector of sample means and std sample means
      sample_mean <- rep(0, reps)
      std_sample_mean <- rep(0, reps)
      
      # Loop over repetitions
      for (i in 1:reps) {
            x <- rbinom(n, 1, 0.5)
            sample_mean[i] <- mean(x)
            std_sample_mean[i] <- sqrt(n) * (mean(x) - 0.5) / 0.5
      }
      
      # Histogram of mean values
      hist(std_sample_mean,
           col = "steelblue",
           freq = FALSE,
           breaks = 40,
           xlim = c(-3, 3),
           ylim = c(0, 0.8),
           xlab = paste("n =", n),
           main = "")
      
      # Overlay normal distribution N(0,1)
      curve(dnorm(x),
            lwd = 2,
            col = "darkred",
            add = TRUE)

}
