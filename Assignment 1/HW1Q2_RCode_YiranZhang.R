# Following is the R code for STATS 790 Homework 1 Question 2
# Reproduce a figure from ESL Chapter 2
# The chosen figure is figure 2.1.

library(MASS) # multivariate normal random sample generator
library(dplyr)

set.seed(400119421)

# Generate 10 means from multivariate normal distribution
# for blue and orange respectively
blue <- mvrnorm(n=10,mu=c(1,0),Sigma=matrix(c(1, 0, 0, 1), ncol=2))
orange <- mvrnorm(n=10,mu=c(0,1),Sigma=matrix(c(1, 0, 0, 1), ncol=2))

# Draw 100 samples from the list above with probability 1/10
# The idea here is to draw 100 samples from the index, and obtain samples from
# blue/orange using the 100 index.
blue_sample <- sample(nrow(blue), 100, replace=TRUE, prob=rep(0.1, 10))
blue_means <- blue[blue_sample, ]

orange_sample <- sample(nrow(orange), 100, replace=TRUE, prob=rep(0.1, 10))
orange_means <- orange[orange_sample, ]

# Generate observations for blue and orange using the means sampled above
blue_obs <- matrix(ncol=2, nrow=100) # to store the observations from blue means
for (i in 1:100){
  blue_obs[i,] <- mvrnorm(n=1, mu=blue_means[i,], 
                          Sigma=matrix(c(1/5, 0, 0, 1/5), ncol=2))
} # for each pair of means, generate one pair of multivariate normal random sample

# same idea for orange observations
orange_obs <- matrix(ncol=2, nrow=100)
for (i in 1:100){
  orange_obs[i,] <- mvrnorm(n=1, mu=orange_means[i,], 
                            Sigma=matrix(c(1/5, 0, 0, 1/5), ncol=2))
}

# Create dataset by combining blue and orange observations
# Transfer into data frame type to fit linear model
blue_orange <- as.data.frame(rbind(blue_obs, orange_obs))

# Linear regression
linear_model <- lm(V2 ~ V1, data=blue_orange)

b0 <- linear_model$coefficients[1] # the coefficient beta_0
b1 <- linear_model$coefficients[2] # the coefficient beta_1

# Fitted linear regression function
linear <- function(x) b0+b1*x

# Generate the background dots in the figure
x <- seq(from = -1.5, to = 4, by = 0.05)
y <- seq(from = -2, to = 3.5, by = 0.05)
background <- expand.grid(x,y) # entire background
y_hat <- linear(x) 
# y_hat is the filter for the linear regression line 
# that separate blue and orange dots on the background

blue_pt <- (background %>% filter(Var2 < y_hat)) # lower part is blue dots
orange_pt <- (background %>% filter(Var2 > y_hat)) # upper part is orange dots

# Figure reproduction
plot(blue_obs
     , col='deepskyblue' # set point color to blue
     , xlim = c(-1.5,4) # expand x axis to fit orange observations later
     , ylim = c(-2,3.5) # expand y axis to fit orange observations later
     , xlab='', ylab='' # clear x and y labels
     , lwd=3, cex = 1.5 # increase size and thickness of the observations
     , xaxt='n', yaxt='n') # clear the x and y axis number labels
points(orange_obs, col='orange', lwd = 3, cex = 1.5) # add orange observations
abline(linear_model, col = 'black', lwd = 3) # add the decision boundary
points(orange_pt, col='orange', pch=20, cex = 0.3) # add orange background dots
points(blue_pt, col='deepskyblue', pch=20, cex = 0.3) # add blue background dots

