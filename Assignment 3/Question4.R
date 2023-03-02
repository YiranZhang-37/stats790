# Question 4
# Part (a) 
# Simulate data from a surface on the unit square with Gaussian noise
my_simulation <- function(n, noise_mean = 0, noise_sd = 1){
  # n is the number of observations required
  # noise_mean is the mean of the noise term, default is 0
  # noise_sd if the sd of the noise term, default is 1
  
  # draw numbers from unit square x-axis
  x <- runif(n)
  # draw numbers from unit square y-axis
  y <- runif(n)
  # noise term from Gaussian distribution
  noise <- rnorm(n, noise_mean, noise_sd)
  # create a smooth surface using a high-order bivariate polynomial
  z <- 5*x^7*y^6 + 2*x^6*y^5 + x^5 + 7*x^3*y^2 + 2*x^2*y + x*y^2 + y^3 + 3*y^4*x + y^8
  # smooth surface with gaussian noise
  z_noise <- z + noise
  # return a data frame with three columns: x, y, and z
  data.frame(x = x, y = y, z = z_noise)
}

# Example of the simulation
my_simulation(8, 0.5, 2.5)$z

# Part (b)
# Import libraries
library(mgcv)

# Function below returns the true value of z, used to compare with the prediction
true_z <- function(x, y){
  5*x^7*y^6 + 2*x^6*y^5 + x^5 + 7*x^3*y^2 + 2*x^2*y + x*y^2 + y^3 + 3*y^4*x + y^8
}

# 250 simulations
simulation_num <- 250

# (i) method = "GCV.Cp".
# Create an empty matrices to store the computed value for each simulation
comp_time1 <- matrix(NA, nrow=simulation_num, ncol=1)
bias1 <- matrix(NA, nrow=simulation_num, ncol=1)
variance1 <- matrix(NA, nrow=simulation_num, ncol=1)
mse1 <- matrix(NA, nrow=simulation_num, ncol=1)

for (i in 1:simulation_num) {
  # Simulate 500 random samples for each simulation with standard gaussian noise
  my_data <- my_simulation(500, 0, 1)
  
  # Fit a GAM model using z ~ te(x,y) formula and method = "GCV.Cp"
  start_time <- Sys.time() # time before running model
  model1 <- gam(z ~ te(x,y,bs = "gp"), data = my_data, method = "GCV.Cp")
  end_time <- Sys.time() # time after running model
  comp_time1[i] <- as.numeric(end_time - start_time) # computation time
  
  # Create test set using runif()
  x_test <- runif(50)
  y_test <- runif(50)
  test_set <- as.data.frame(cbind(x = x_test, y = y_test))
  
  # Prediction
  pred <- predict(model1, newdata = test_set) # predicted z value
  true <- true_z(x_test, y_test) # true value of z for comparison
  
  # Compute bias, variance, and MSE of the predictions
  bias1[i] <- mean(pred - true)
  variance1[i] <- mean((pred - mean(pred))^2)
  mse1[i] <- mean((pred - true)^2)
}

mean(comp_time1)
mean(bias1)
mean(variance1)
mean(mse1)

# (ii) method = "REML".
# Create an empty matrices to store the computed value for each simulation
comp_time2 <- matrix(NA, nrow=simulation_num, ncol=1)
bias2 <- matrix(NA, nrow=simulation_num, ncol=1)
variance2 <- matrix(NA, nrow=simulation_num, ncol=1)
mse2 <- matrix(NA, nrow=simulation_num, ncol=1)

for (i in 1:simulation_num) {
  # Simulate 500 random samples for each simulation with standard gaussian noise
  my_data <- my_simulation(500, 0, 1)
  
  # Fit a GAM model using z ~ te(x,y) formula and method = "REML"
  start_time <- Sys.time()
  model2 <- gam(z ~ te(x,y,bs = "gp"), data = my_data, method = "REML")
  end_time <- Sys.time()
  comp_time2[i] <- as.numeric(end_time - start_time) # computation time
  
  # Create test set using runif()
  x_test <- runif(50)
  y_test <- runif(50)
  test_set <- as.data.frame(cbind(x = x_test, y = y_test))
  
  # Prediction
  pred <- predict(model2, newdata = test_set) # predicted z value
  true <- true_z(x_test, y_test) # true value of z for comparison
  
  # Compute bias, variance, and MSE of the predictions
  bias2[i] <- mean(pred - true)
  variance2[i] <- mean((pred - mean(pred))^2)
  mse2[i] <- mean((pred - true)^2)
}

mean(comp_time2)
mean(bias2)
mean(variance2)
mean(mse2)

# Create a comparison table for two methods
compare <- matrix(c(mean(comp_time1), mean(bias1), mean(variance1), mean(mse1), 
                    mean(comp_time2), mean(bias2), mean(variance2), mean(mse2)), 
                  ncol = 2, nrow = 4)
colnames(compare) <- c("GCV.Cp", "REML")
rownames(compare) <- c("computation time", "bias", "variance", "mse")
compare

# Generally, both methods have similar computation time, bias, variance and mse. 
# REML has slightly greater values in all outputs.