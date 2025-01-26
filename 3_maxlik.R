# Charger les bibliothèques nécessaires
library(maxLik)
library(mvtnorm)

# Charger les données
data <- readRDS("INTERMEDIATE/transports.rds")

# Fonction de log-vraisemblance conjointe avec les intercepts spécifiques aux villes
n_g <- 7 # Nombre de variables explicatives pour FP_star
logLik_joint_city <- function(params, data) {
  # Paramètres
  gamma <- params[1:n_g] # Coefficients pour FP_star
  beta <- params[n_g + 1:(n_g + 8 + length(unique(data$CITY)))] # Coefficients pour la fonction de coût
  sigma <- params[n_g + 9 + length(unique(data$CITY))] # Écart-type de epsilon
  rho <- params[n_g + 10 + length(unique(data$CITY))] # Corrélation entre epsilon et eta

  # Paramètres dérivés
  sigma_eta <- 1 # Variance de eta fixée à 1
  # Générer les dummies pour les villes
  city_dummies <- model.matrix(~ CITY - 1, data = data)
  city_params <- beta[9:(8 + ncol(city_dummies))]

  # Calcul de la variable latente FP_star
  FP_star <- with(
    data,
      gamma[1] * TRANS +
      gamma[2] * AGIR + gamma[3] * KEOLIS +
      gamma[4] * CONNEX +
      gamma[5] * RIGHT +
      gamma[6] * log(NCITIES) +
      gamma[7] * TREND +
      beta[1] +
      beta[2] * log_PKO +
      beta[3] * log_PL_PM +
      beta[4] * 0.5 * (log_PKO^2) +
      beta[5] * 0.5 * (log_PL_PM^2) +
      beta[6] * log_PKO * log_PL_PM +
      beta[8] * TREND)

  # Probabilités associées à INCENT
  prob_IN <- pnorm(FP_star) # P(INCENT = 1)
  prob_OUT <- 1 - prob_IN # P(INCENT = 0)

  # Fonction de coût translog complète avec les dummies pour les villes

  predicted_costs <- with(
    data,
      beta[1] +
      beta[2] * log_PKO +
      beta[3] * log_PL_PM +
      beta[4] * 0.5 * (log_PKO^2) +
      beta[5] * 0.5 * (log_PL_PM^2) +
      beta[6] * log_PKO * log_PL_PM +
      beta[7] * INCENT +
      beta[8] * TREND +
      city_dummies %*% city_params
  )

  # Résidus de la fonction de coût
  residuals <- data$log_COSTS_PM - predicted_costs

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
init_params_city <- c(rep(0.01, n_g + 8 + n_cities), 0.5, 0.05)

# Définir les contraintes linéaires
A_city <- matrix(c(
  rep(0, n_g + 8), rep(0, n_cities), 1, 0, # sigma - 0.01 >= 0
  rep(0, n_g + 8), rep(0, n_cities), 0, 1, # rho + 0.99 >= 0
  rep(0, n_g + 8), rep(0, n_cities), 0, -1 # 0.99 - rho >= 0
), ncol = n_g + 10 + n_cities, byrow = TRUE)

B_city <- c(-0.01, 0.99, 0.99)


# Maximisation de la log-vraisemblance avec contraintes linéaires et méthode BFGS
model <- maxLik(
  logLik = logLik_joint_city, start = init_params_city, data = data,
  constraints = list(ineqA = A_city, ineqB = B_city),
  control = list(iterlim = 2000),
  method = "BFGS"
)

summary(model)
