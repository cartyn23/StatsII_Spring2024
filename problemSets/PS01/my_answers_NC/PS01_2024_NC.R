
calculate_p_value <- function(data) {
  # Create empirical distribution
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  
  # Calculate the theoretical CDF assuming a standard normal distribution
  theoreticalCDF <- pnorm(data)
  
  # Calculate the test statistic D
  D <- max(abs(empiricalCDF - theoreticalCDF))
  
  # Initialize p-value
  p_value <- 0
  
  # Loop to calculate the sum in the formula
  for (k in 1:1000) {  
    p_value <- p_value + sqrt(2 * pi) / D * exp(-(((2 * k) - 1)^2) * pi^2 / (8 * D^2))
  }
  
  return(p_value)
}

set.seed(123)
data <- rcauchy(1000)

p_value <- calculate_p_value(data)
p_value
D

# Perform Kolmogorov-Smirnov test
ks_result <- ks.test(data, "pnorm")

# Print the result
print(ks_result)

sprintf("%.20f",5.652523e-29)
sprintf("%.20f",2.22e-16)


set.seed(123)
data <- data.frame(x = runif(200,1,10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Using LM
model <- lm(y ~ x, data = data)
summary(model)

plot(data$x, data$y)
abline(model, col = "red")

# Using BFGS


nr_function <- function(beta, x, y) {
  y_hat <- beta[1] + beta[2] * x
  sum((y - y_hat)^2)
}
initial_guess <- c(0, 1) # Initial guess for intercept and slope
result <- optim(par = initial_guess, fn = nr_function, x = data$x, y = data$y, method = "BFGS")

# Extract the coefficients
coefficients <- result$par
print(coefficients)


