# Libraries
library(dplyr)
library(ggplot2)

# Imports
data <- readRDS("INTERMEDIATE/transports.rds")

# Statistiques descriptives pour toutes les variables numériques

# Génère le résumé des statistiques avec deux chiffres significatifs
stats <- function(data) {
    summary <- data %>%
        select(where(is.numeric)) %>%
        reframe(
            var = colnames(.),
            median = sapply(., function(x) median(x)),
            mean = sapply(., function(x) mean(x)),
            sd = sapply(., function(x) sd(x)),
            min = sapply(., function(x) min(x)),
            max = sapply(., function(x) max(x))
        )
    return(summary)
}
# Afficher les résultats
describe <- stats(data) %>% filter(
        (var != "CITY_id") &
        (var != "YEAR") &
        (var != "TREND")
)


#### Descriptive Graphs

# Histogramme pour la variable `COSTS`
ggplot(data, aes(x = COSTS)) +
    geom_histogram(binwidth = 1000, fill = "#476682", color = "white") +
    theme_minimal() +
    labs(title = "Distribution des coûts", x = "COSTS", y = "Fréquence")

# Boxplot pour visualiser les coûts par type de contrat
ggplot(data, aes(x = as.factor(INCENT), y = COSTS)) +
    geom_boxplot(fill = "#56ddb7") +
    theme_minimal() +
    labs(title = "Coûts par type de contrat", x = "INCENT (0: Non, 1: Oui)", y = "COSTS")

# Boxplot pour visualiser les coûts par type de contrat
ggplot(data, aes(x = as.factor(RIGHT), y = COSTS)) +
    geom_boxplot(fill = "#4568a0") +
    theme_minimal() +
    labs(title = "Coûts par idéologie politique", x = "RIGHT = 1, LEFT = 0", y = "COSTS")
