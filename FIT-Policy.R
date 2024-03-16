"
this Script is to
"

# Install & Load Packages
{
  #install.packages("tidyverse")
  #install.packages("xtable")
  #install.packages("lfe")
  
  library(tidyverse)
  library(xtable)
  library(lfe)
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
    mutate(across(starts_with('Elect_'), ~.x/Elect_Total, .names="Share{.col}")) %>%
    select(!c("Elect_Hydro", "Elect_Geothermal", "Elect_Wind","Elect_SolarThermal",
              "Elect_Solar", "Elect_Marine", "Elect_Biomass", "Elect_Waste")) %>%
    mutate(across(starts_with('ShareElect'), ~.x*UseElectricityShare, .names="Tot{.col}"),
           across(starts_with('TotShareElect'), ~.x*Elect_Total, .names="Exp{.col}")) %>%
    rename_with(~gsub("ExpTotShareElect_", "Exposure_", .x), starts_with("ExpTotShareElect_")) %>%
    rename_with(~gsub("^FIT(.+)$", "FIT_\\1", .x), starts_with("FIT"))
  
  # Identify the FIT columns and their matching Exposure columns
  fit_columns <- colnames(dta_exposure)[str_detect(colnames(dta_exposure), "^FIT_")]
  exposure_columns <- str_replace(fit_columns, "FIT_", "Exposure_")
  
  # Generate new column names for the results
  new_column_names <- str_replace(fit_columns, "FIT_", "FITxExposure_")
  
  # FIT = DKK/kWh
  # Exposure = kWh
  # => FIT x Exposure = DKK
  # Multiply matching columns and add them to the dataframe
  for (i in seq_along(fit_columns)) {
    dta_exposure <- dta_exposure %>%
      mutate(!!new_column_names[i] := .data[[fit_columns[i]]] * .data[[exposure_columns[i]]])
  }
  
  dta_totalexposure <- dta_exposure %>%
    rowwise() %>%
    mutate(TotalExposure = sum(c_across(starts_with('FITxExposure_')), na.rm = TRUE)) %>%
    ungroup()
}

testtt <- dta_totalexposure %>% filter(!is.na(NACE_Name)) %>%
  filter(NACE_Name != 'Total Manufacturing') %>%
  select(year, NACE_Name, !!sym(y_var), !!sym(x_var)) %>%
  mutate(TotalExposure = TotalExposure/1000000)
testtt <- dta_exposure %>% filter(NACE_Name == 'Total Manufacturing') %>%
  select(year, UseElectricity, CO2ElectricityHeat)

# Run Fixed_Effects Estimation
{
  model_fe <- felm(envregulation ~ TotalExposure | # Model Variable
                     year + NACE_Name | # Fixed Effects
                     0 | # Instrument
                     0, # Variables for Cluster-robust Standard errors
                   data = testtt, cmethod = 'cgm2')
  summary(model_fe)
  model_simple <- lm(envregulation ~ TotalExposure, data = testtt)
  summary(model_simple)
}