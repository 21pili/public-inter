library(maxLik)
library(mvtnorm)

# Load data
data <- readRDS("INTERMEDIATE/transports.rds")

# Log-likelihood function
logLik_joint_refined <- function(params, data) {
    # Parameters
    gamma <- params[1:6]  # Coefficients for FP_star
    beta <- params[7:14]  # Coefficients for cost function
    sigma <- params[15]   # Standard deviation of epsilon
    rho <- params[16]     # Correlation between epsilon and eta

    # Derived parameters
    sigma_eta <- 1  # Variance of eta fixed to 1

    # Latent variable FP_star
    FP_star <- with(data, gamma[1] + gamma[2] * log(NCITIES) +
                          gamma[3] * RIGHT + gamma[4] * TRANS +
                          gamma[5] * AGIR + gamma[6] * KEOLIS)

    # Probabilities for INCENT
    prob_IN <- pnorm(FP_star)  # P(INCENT = 1)
    prob_OUT <- 1 - prob_IN    # P(INCENT = 0)

    # Translog cost function
    predicted_costs <- with(data, beta[1] +
        beta[2] * log_PKO +
        beta[3] * log_PL_PM +
        beta[4] * 0.5 * (log_PKO^2) +
        beta[5] * 0.5 * (log_PL_PM^2) +
        beta[6] * log_PKO * log_PL_PM +
        beta[7] * YEAR +
        beta[8] * INCENT)

    # Residuals of the cost function
    residuals <- log(data$COSTS) - predicted_costs

    # Joint density: Bivariate normal
    joint_density <- dmvnorm(
        cbind(residuals, FP_star),
        sigma = matrix(c(sigma^2, rho * sigma, rho * sigma, sigma_eta^2), ncol = 2)
    )

    # Log-likelihood
    logLik <- sum(log(prob_IN[data$INCENT == 1])) +
              sum(log(prob_OUT[data$INCENT == 0])) +
              sum(log(joint_density))

    return(logLik)
}

# Initial guesses for parameters
init_params <- c(rep(0.01, 14), 0.5, 0.05)  # gamma (6) + beta (8) + sigma + rho

# Define constraints
A <- matrix(c(
    # sigma - 0.01 >= 0
    rep(0, 14), 1, 0,   # sigma at position 15
    # rho + 0.99 >= 0
    rep(0, 15), 1,      # rho at position 16
    # 0.99 - rho >= 0
    rep(0, 15), -1      # rho at position 16
), ncol = 16, byrow = TRUE)

# Right-hand side for constraints
B <- c(0.01, -0.99, 0.99)

# Maximize log-likelihood with constraints
result <- maxLik(
    logLik = logLik_joint_refined, start = init_params, data = data,
    constraints = list(ineqA = A, ineqB = B), method = "BFGS"
)

# Summary of results
summary(result)
