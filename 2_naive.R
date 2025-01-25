#Libraries
library(plm)        # For panel data models
library(systemfit)  # For systems of equations
library(AER)        # For IV regression
library(dplyr)      # For data manipulation

data <- readRDS("INTERMEDIATE/transports.rds")


translog_model <- plm(
  log_COSTS_PM ~
    log_PKO + log_PL_PM +
    I(0.5 * log_PKO^2) + I(0.5 * log_PL_PM^2) +
    I(log_PKO * log_PL_PM) +
    TREND +
    INCENT,
  data = data,
  index = "CITY_id",  # Effets fixes uniquement au niveau des villes
  model = "within"
)

summary(translog_model)

translog_model_fe <- lm(
  log_COSTS_PM ~
    log_PKO + log_PL_PM +
    I(0.5 * log_PKO^2) + I(0.5 * log_PL_PM^2) +
    I(log_PKO * log_PL_PM) +
    TREND + INCENT +
    factor(CITY_id),  # Effets fixes explicites
  data = data
)

summary(translog_model_fe)
