# Charger les bibliothèques nécessaires
library(maxLik)
library(mvtnorm)

# Charger les données
data <- readRDS("INTERMEDIATE/transports.rds")

# Fonction de log-vraisemblance conjointe
logLik_joint_refined <- function(params, data) {
  # Paramètres
  gamma <- params[1:6]  # Coefficients pour FP_star
  beta <- params[7:13]  # Coefficients pour la fonction de coût
  sigma <- params[14]   # Écart-type de epsilon
  rho <- params[15]     # Corrélation entre epsilon et eta

  # Paramètres dérivés
  sigma_eta <- 1        # Variance de eta fixée à 1

  # Calcul de la variable latente FP_star
  FP_star <- with(data, gamma[1] + gamma[2] * log(NCITIES) +
                          gamma[3] * RIGHT + gamma[4] * TRANS +
                          gamma[5] * AGIR + gamma[6] * KEOLIS +
                          beta[1] +
                          beta[2] * log_PKO +
                            beta[3] * log_PL_PM +
                            beta[4] * (log_PKO^2) +
                            beta[5] * (log_PL_PM^2) +
                            beta[6] * log_PKO * log_PL_PM)

  # Probabilités associées à INCENT
  prob_IN <- pnorm(FP_star)   # P(INCENT = 1)
  prob_OUT <- 1 - prob_IN     # P(INCENT = 0)

  # Fonction de coût translog complète
  predicted_costs <- with(data, beta[1] +
    beta[2] * log_PKO +
    beta[3] * log_PL_PM +
    beta[4] * (log_PKO^2) +
    beta[5] * (log_PL_PM^2) +
    beta[6] * log_PKO * log_PL_PM +
    beta[7] * INCENT)

  # Résidus de la fonction de coût
  residuals <- log(data$COSTS) - predicted_costs

  # Densité conjointe : normale bivariée
  joint_density <- dmvnorm(
    cbind(residuals, FP_star),
    sigma = matrix(c(sigma^2, rho * sigma, rho * sigma, sigma_eta^2), ncol = 2)
  )

  # Log-vraisemblance
  logLik <- sum(log(prob_IN[data$INCENT == 1])) +
            sum(log(prob_OUT[data$INCENT == 0])) +
            sum(log(joint_density))

  return(logLik)
}

logLik_joint <- function(params, data) {
  # Paramètres
  gamma <- params[1:6]  # Coefficients pour FP_star
  beta <- params[7:13]  # Coefficients pour la fonction de coût
  sigma <- params[14]   # Écart-type de epsilon
  rho <- params[15]     # Corrélation entre epsilon et eta

  # Paramètres dérivés
  sigma_eta <- 1        # Variance de eta fixée à 1

  # Calcul de la variable latente FP_star
  FP_star <- with(data, gamma[1] + gamma[2] * log(NCITIES) +
                          gamma[3] * RIGHT + gamma[4] * TRANS +
                          gamma[5] * AGIR + gamma[6] * KEOLIS)

  # Probabilités associées à INCENT
  prob_IN <- pnorm(FP_star)   # P(INCENT = 1)
  prob_OUT <- 1 - prob_IN     # P(INCENT = 0)

  # Fonction de coût translog complète
  predicted_costs <- with(data, beta[1] +
    beta[2] * log_PKO +
    beta[3] * log_PL_PM +
    beta[4] * (log_PKO^2) +
    beta[5] * (log_PL_PM^2) +
    beta[6] * log_PKO * log_PL_PM +
    beta[7] * INCENT)

  # Résidus de la fonction de coût
  residuals <- log(data$COSTS) - predicted_costs

  # Densité conjointe : normale bivariée
  joint_density <- dmvnorm(
    cbind(residuals, FP_star),
    sigma = matrix(c(sigma^2, rho * sigma, rho * sigma, sigma_eta^2), ncol = 2)
  )

  # Log-vraisemblance
  logLik <- sum(log(prob_IN[data$INCENT == 1])) +
            sum(log(prob_OUT[data$INCENT == 0])) +
            sum(log(joint_density))

  return(logLik)
}

# Guesses initiaux pour les paramètres
init_params <- c(rep(0.01, 13), 0.5, 0.05)  # 6 (gamma) + 7 (beta) + sigma + rho

# Définir les contraintes linéaires
A <- matrix(c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,  # sigma - 0.01 >= 0
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  # rho + 0.99 >= 0
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1 # 0.99 - rho >= 0
), ncol = 15, byrow = TRUE)

B <- c(-0.01, 0.99, 0.99)

# Maximisation de la log-vraisemblance avec contraintes linéaires et méthode BFGS
result <- maxLik(logLik = logLik_joint, start = init_params, data = data,
                 constraints = list(ineqA = A, ineqB = B), method = "BFGS")

result_refined <- maxLik(logLik = logLik_joint_refined, start = init_params, data = data,
                 constraints = list(ineqA = A, ineqB = B), method = "BFGS")

# Résumé des résultats
summary(result)
summary(result_refined)
