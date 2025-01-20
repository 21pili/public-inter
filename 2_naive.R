#Libraries
library(plm)        # For panel data models
library(systemfit)  # For systems of equations
library(AER)        # For IV regression
library(dplyr)      # For data manipulation

data <- readRDS("INTERMEDIATE/transports.rds")


# Model with reparameterization for homogeneity
translog_model <- plm(
  log_COSTS_PM ~
    log_PKO + log_PL_PM +
    I(0.5 * log_PKO^2) + I(0.5 * log_PL_PM^2) +
    I(log_PKO * log_PL_PM) +
    TREND +
    INCENT,
  data = data,
  index = c("CITY_id"),  # Panel data: firm and time
  model = "within" ## Ou pooling
)

# Summarize the results
summary(translog_model)
