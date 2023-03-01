# Question 2
# Import libraries
library(splines)
library(ggplot2)

# Import South Africa coronary heart disease data.
url <- "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/SAheart.data"
heart <- read.csv(url, row.names = 1)

# Create a list of 5 knots location for the bases
# Knots that can equally divide the tobacco range are chosen
# There are 107 out of 462 tobacco values are 0, 107/462 = 0.2316
# So the first knot need to be greater than 0.2316 to avoid NA values in glm
knots <- quantile(heart$tobacco, probs = c(0.24, 0.32, 0.48, 0.64, 0.8))

# B-spline base
b_spline <- bs(heart$tobacco, knots = knots)

# Natural spline base
n_spline <- ns(heart$tobacco, knots = knots)

# Truncated polynomial base
# Below is the function that creates truncated polynomial spline base
# it is modified based on Dr. Bolker's code
truncpolyspline <- function(x, knots) {
  trunc_fun <- function(k) (x > k)*(x-k)^3
  S <- sapply(knots, trunc_fun)
  S <- cbind(x, x^2, x^3, S)
  return(S)
}

poly_spline <- truncpolyspline(heart$tobacco, knots) # create basis matrix

# Fit logistic regression
# b-spline
logistic_b <- glm(chd ~ b_spline, data = heart, family = 'binomial')
# natural spline
logistic_n <- glm(chd ~ n_spline, data = heart, family = 'binomial')
# truncated polynomial spline
logistic_poly <- glm(chd ~ poly_spline, data = heart, family = 'binomial')

summary(logistic_b)
summary(logistic_n)
summary(logistic_poly)

# Predict without using predict() function on log-odds scale
# b-spline
X_b <- model.matrix(logistic_b) # design matrix for b-spline
coef_b <- as.vector(logistic_b$coefficients) # coefficients vector for b-spline
Y_b <- X_b %*% coef_b # predicted value
var_Y_b <- diag(X_b %*% vcov(logistic_b) %*% t(X_b)) # predicted variance
se_Y_b <- as.matrix(sqrt(var_Y_b)) # predicted se
upper_b <- Y_b + se_Y_b # upper bound of the CI
lower_b <- Y_b - se_Y_b # lower bound of the CI

# Create a dataframe for b-spline x, predicted y, upper and lower bound of y.
df_b <- as.data.frame(cbind(heart$tobacco, Y_b, upper_b, lower_b))
colnames(df_b) <- c('tobacco', 'pred', 'upper', 'lower')

# Plot for b-spline
ggplot(df_b, aes(x=tobacco)) +  geom_point(aes(y = pred)) + # scatter plot for Y_b
  geom_line(aes(y = upper), linetype = 'dashed', color = 'red') + # add upper bound
  geom_line(aes(y = lower), linetype = 'dashed', color = 'red') +  # add lower bound
  labs(x = "Tobacco consumption", y = "Fitted value in log-odds scale", 
       title = "B-Spline prediction") # add axis labels

# natural spline
X_n <- model.matrix(logistic_n) # design matrix for b-spline
coef_n <- as.vector(logistic_n$coefficients) # coefficients vector for b-spline
Y_n <- X_n %*% coef_n # predicted value
var_Y_n <- diag(X_n %*% vcov(logistic_n) %*% t(X_n)) # predicted variance
se_Y_n <- as.matrix(sqrt(var_Y_n)) # predicted se
upper_n <- Y_n + se_Y_n # upper bound of the CI
lower_n <- Y_n - se_Y_n # lower bound of the CI

# Create a dataframe for natural spline x, predicted y, upper and lower bound of y.
df_n <- as.data.frame(cbind(heart$tobacco, Y_n, upper_n, lower_n))
colnames(df_n) <- c('tobacco', 'pred', 'upper', 'lower')

# Plot for natural spline
ggplot(df_n, aes(x=tobacco)) +  geom_point(aes(y = pred)) + # scatter plot for Y_b
  geom_line(aes(y = upper), linetype = 'dashed', color = 'red') + # add upper bound
  geom_line(aes(y = lower), linetype = 'dashed', color = 'red') +  # add lower bound
  labs(x = "Tobacco consumption", y = "Fitted value in log-odds scale", 
       title = "Natural Spline prediction") # add axis labels

# truncated polynomial spline
X_poly <- model.matrix(logistic_poly) # design matrix for b-spline
coef_poly <- as.vector(logistic_poly$coefficients) # coefficients vector for b-spline
Y_poly <- X_poly %*% coef_poly # predicted value
var_Y_poly <- diag(X_poly %*% vcov(logistic_poly) %*% t(X_poly)) # predicted variance
se_Y_poly <- as.matrix(sqrt(abs(var_Y_poly))) # predicted se
upper_poly <- Y_poly + se_Y_poly # upper bound of the CI
lower_poly <- Y_poly - se_Y_poly # lower bound of the CI

# Create a dataframe for truncated polynomial spline x, predicted y, 
# upper and lower bound of y.
df_poly <- as.data.frame(cbind(heart$tobacco, Y_poly, upper_poly, lower_poly))
colnames(df_poly) <- c('tobacco', 'pred', 'upper', 'lower')

# Plot for truncated polynomial spline
ggplot(df_poly, aes(x=tobacco)) +  geom_point(aes(y = pred)) + # scatter plot for Y_b
  geom_line(aes(y = upper), linetype = 'dashed', color = 'red') + # add upper bound
  geom_line(aes(y = lower), linetype = 'dashed', color = 'red') +  # add lower bound
  labs(x = "Tobacco consumption", y = "Fitted value in log-odds scale", 
       title = "Truncated Polynomial Spline prediction") # add axis labels
