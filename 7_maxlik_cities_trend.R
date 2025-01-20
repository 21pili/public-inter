# Charger les bibliothèques nécessaires
library(maxLik)
library(mvtnorm)

# Charger les données
data <- readRDS("INTERMEDIATE/transports.rds")

# Fonction de log-vraisemblance conjointe avec les intercepts spécifiques aux villes
logLik_joint_city <- function(params, data) {
  # Paramètres
  gamma <- params[1:6] # Coefficients pour FP_star
  beta <- params[7:(14 + length(unique(data$CITY)))] # Coefficients pour la fonction de coût
  sigma <- params[15 + length(unique(data$CITY))] # Écart-type de epsilon
  rho <- params[16 + length(unique(data$CITY))] # Corrélation entre epsilon et eta

  # Paramètres dérivés
  sigma_eta <- 1 # Variance de eta fixée à 1
  # Générer les dummies pour les villes
  city_dummies <- model.matrix(~ CITY - 1, data = data)
  city_params <- beta[9:(8 + ncol(city_dummies))]

  # Calcul de la variable latente FP_star
  FP_star <- with(data,
    gamma[1] + gamma[2] * log(NCITIES) +
    gamma[3] * RIGHT + gamma[4] * TRANS +
    gamma[5] * AGIR + gamma[6] * KEOLIS +
    beta[1] +
    beta[2] * log_PKO +
    beta[3] * log_PL_PM +
    beta[4] * 0.5 * (log_PKO^2) +
    beta[5] * 0.5 * (log_PL_PM^2) +
    beta[6] * log_PKO * log_PL_PM +
    beta[8] * TREND +
    city_dummies %*% city_params)

  # Probabilités associées à INCENT
  prob_IN <- pnorm(FP_star) # P(INCENT = 1)
  prob_OUT <- 1 - prob_IN # P(INCENT = 0)

  # Fonction de coût translog complète avec les dummies pour les villes

  predicted_costs <- with(data, 
    beta[1] +
    beta[2] * log_PKO +
    beta[3] * log_PL_PM +
    beta[4] * (log_PKO^2) +
    beta[5] * (log_PL_PM^2) +
    beta[6] * log_PKO * log_PL_PM +
    beta[7] * INCENT +
    beta[8] * TREND +
    city_dummies %*% city_params)

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

# Ajuster les paramètres initiaux pour inclure les dummies de villes
n_cities <- length(unique(data$CITY))
init_params_city <- c(rep(0.01, 14 + n_cities), 0.5, 0.05)
init_params_city <- c(rep(0.01, 14 + n_cities), 0.5, 0.05)

# Définir les contraintes linéaires
A_city <- matrix(c(
  rep(0, 14), rep(0, n_cities), 1, 0, # sigma - 0.01 >= 0
  rep(0, 14), rep(0, n_cities), 0, 1, # rho + 0.99 >= 0
  rep(0, 14), rep(0, n_cities), 0, -1 # 0.99 - rho >= 0
), ncol = 16 + n_cities, byrow = TRUE)

B_city <- c(-0.01, 0.99, 0.99)


# Maximisation de la log-vraisemblance avec contraintes linéaires et méthode BFGS
model <- maxLik(
  logLik = logLik_joint_city, start = init_params_city, data = data,
  constraints = list(ineqA = A_city, ineqB = B_city), method = "BFGS"
)

summary(model)
