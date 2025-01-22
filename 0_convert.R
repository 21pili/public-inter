# Charger le package readxl
library(readxl)
library(dplyr)

# Spécifiez le chemin vers le fichier xls
file_path <- "DATA/Base077.xls"

# Affichez les noms des feuilles dans le fichier Excel
sheet_names <- excel_sheets(file_path)
data <- as.data.frame(read_excel(file_path, sheet = 1)) %>%
    mutate(
        CITY_id = NUM...3
    ) %>%
    select(CITY, YEAR, CITY_id, TREND, TRANS, AGIR, CONNEX,
    KEOLIS, INCENT, PUBLIC, RIGHT, NCITIES, POPU, LINES,
    LENGHT, PARC, EMPLOY, DRIVERS, PKO, LABOR, COSTS, GASI) %>%
  ## Normalize and log-transform variables
  mutate(
    PM = GASI, #Price of materials
    Wages = LABOR/EMPLOY,  # Wage rates
    PL_PM = (LABOR / EMPLOY) / PM,  # Wage rate normalized by GASI
    log_COSTS_PM = log(COSTS / PM),   # Log of normalized costs
    log_PKO = log(PKO),               # Log of output
    log_PL_PM = log(PL_PM)           # Log of normalized wage rate
  )


saveRDS(data, "INTERMEDIATE/transports.rds")
