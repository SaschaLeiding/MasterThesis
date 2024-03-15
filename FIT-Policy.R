"
this Script is to
"

# Install & Load Packages
{
  #install.packages("tidyverse")
  #install.packages("xtable")
  
  library(tidyverse)
  library(xtable)
}

# Load Data
{
  dta_policy <- readRDS("./Data/dta_policy.rds")
  #dta_internat <- readRDS("./Data/dta_internat.rds")
}

# Define variables for flexibility in Code
{
  ghg <- 'CO2ElectricityHeat'
  energy_type <- ifelse(!!sym(ghg) == 'CO2ElectricityHeat', 'realcostEnergyElectricityHeat', 'realcostEnergy')
  y_var <- 'envregulation'
}

# Plot Fit-Exposure - fit-tariff x energy use per industry
# Plot normalized GHG development & normalized Environmental regulation tax & FIT-Exposure for 'Total Manuf' <- two y-axis

# Calculate FIT-Exposure
# Sum by fuel type of change in Electricity Generation x FIT in given year
{
  dta_exposure <- dta_policy %>%
    filter(!is.na(NACE_Name)) %>%
    select(NACE_Name, year, !!sym(ghg), UseElectricity,!!sym(energy_type),
           starts_with('FIT'), starts_with('Elect_'))
}