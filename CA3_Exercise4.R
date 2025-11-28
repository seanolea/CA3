# Part 1

df <- with(mtcars, data.frame(y = mpg, x1 = disp, x2 = hp, x3 = wt))

# Part 2

nll_lm <- function(data, par)
{
  n <- nrow(data)
  p <- ncol(data) - 1
  
  beta <- par[1:(p + 1)]
  sigma <- par[p + 2]
  
  if (sigma <= 0) return(Inf)
  
  X <- as.matrix(cbind(1, data[, -1]))
  y <- data$y
  
  residuals <- y - X %*% beta
  
  nll <- -sum(dnorm(residuals, mean = 0, sd = sigma, log = TRUE))
  
  return(nll)
}

# Part 3

par0 <- c(mean(df$y), rep(0, ncol(df) - 1), sd(df$y))
result <- optim(par0, nll_lm, data = df, method = "L-BFGS-B", lower = -Inf, upper = Inf)
result$par

# Part 4

# It was necessary to implement the negative log-likelihood function because optim() was made for getting the min of functions. By minimizing the negative log-likelihood, we effectively maximize the log-likelihood, which is the goal in maximum likelihood estimation.

# Part 5

X <- as.matrix(cbind(1, df[, -1]))

beta_hat <- solve(crossprod(X), crossprod(X,df$y))

beta_hat

# Part 6

residuals <- df$y - X %*% beta_hat

sigma_hat <- sqrt(sum(residuals^2) / nrow(df))

sigma_hat

sigma_opt <- result$par[length(result$par)]

sigma_opt

# Part 7

# The difference arises because the MLE of sigma in the optim() function is based on maximizing the likelihood, which uses n in the denominator, while the manual calculation uses the unbiased estimator.

# Part 8

hessian <- optim(par0, nll_lm, data = df, method = "L-BFGS-B", lower = -Inf, upper = Inf, hessian = TRUE)

cov_matrix <- solve(hessian$hessian)

std_errors <- sqrt(diag(cov_matrix))[1:(ncol(df))]

std_errors

## EXCERCISE 4 CODE

lm_model <- lm(y ~ x1 + x2 + x3, data = df)

beta_hat_lm <- coef(lm_model)

sigma_hat_lm <- summary(lm_model)$sigma

beta_hat_lm
sigma_hat_lm
