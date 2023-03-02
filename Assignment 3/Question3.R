# First write a truncated polynomial spline base function
# This function is a modified version from the lecture notes
truncPoly <- function(x, df, natural = TRUE) {
  # x is the predictor
  # df is degree of freedom
  # natural is a boolean argument determining if natural constraint is needed
  if (natural){
    K <- df # df = K for natural, K is the number of knots
    knots <- quantile(x, seq(0.1, 0.9, length = K)) # obtain K knots
    # d_k(X) is a matrix with n rows and K-1 columns
    d_k <- matrix(NA, nrow = length(x), ncol = K-1)
    for (i in 1:(K-1)){
      numerator <- (x > knots[i])*(x-knots[i])^3 - (x > knots[K])*(x-knots[K])^3
      denominator <- knots[K]-knots[i]
      d_k[,i] <- numerator/denominator # ESL(5.5)
    }
    # N_{k+2}(X) is a matrix with n rows and K-2 columns
    N_kplus2 <- matrix(NA, nrow = length(x), ncol = K-2)
    for (i in 1:(K-2)){
      N_kplus2[,i] <- d_k[,i] - d_k[,K-1] # ESL(5.4)
    }
    # Final matrix of basis, N_2(X) = X, N_{k+2}(X) defined as above
    N <- cbind(x, N_kplus2)
    return(N)
  }
  else {
    K <- df - 4 # df = K+4 for regular
    knots <- quantile(x, seq(0.1, 0.9, length = K)) # obtain K knots
    trunc_fun <- function(k) (x > k)*(x-k)^3
    S <- sapply(knots[1:(df-2)], trunc_fun)
    S <- cbind(x, x^2, x^3, S) # regular truncated polynomial spline
    return(S)
  }
}

# Plot an example of regular and natural bases
x <- seq(0, 10, length = 101) # example from lecture
regular <- truncPoly(x, df = 5, natural = FALSE) # regular bases
natural <- truncPoly(x, df = 5, natural = TRUE) # natural bases
# Plots
par(mfrow = c(1, 2))
matplot(scale(regular), type = "l", main = "Without Natural Constraints")
matplot(scale(natural), type = "l", main = "With Natural Constraints")

