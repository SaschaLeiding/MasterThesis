"
this Script is to
"

# Install & Load Packages
{
  #install.packages("tidyverse")
  #install.packages("xtable")
  #install.packages("lfe")
  #install.packages("stargazer")
  
  library(tidyverse)
  library(xtable)
  library(lfe)
  library(stargazer)
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
  base_year <- '2003'
}

# Plot Fit-Exposure - fit-tariff x energy use per industry
# Plot normalized GHG development & normalized Environmental regulation tax & FIT-Exposure for 'Total Manuf' <- two y-axis

# Calculate FIT-Exposure
# Sum by fuel type of change in Electricity Generation x FIT in given year
{
  dta_exposure <- dta_policy %>%
    mutate(across(starts_with('Elect_'), ~.x/Elect_Total, .names="Share{.col}")) %>% # Calc. Electricity Share of each fuel
    select(!c("Elect_Hydro", "Elect_Geothermal", "Elect_Wind","Elect_SolarThermal",
              "Elect_Solar", "Elect_Marine", "Elect_Biomass", "Elect_Waste")) %>%
    mutate(across(starts_with('ShareElect'), ~.x*UseElectricityShare, .names="Tot{.col}"), # multiply Use Share with Fuel Share
           across(starts_with('TotShareElect'), ~.x*Elect_Total, .names="Exp{.col}")) %>% # Electricity Exposure: multiply exposure in shares with Electricity Production
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

testtt <- dta_totalexposure %>%
  filter(!is.na(NACE_Name)) %>%
  filter(NACE_Name != 'Total Manufacturing') %>%
  #filter(!(year %in% c(2008,2009))) %>%
  select(year, NACE_Name, !!sym(y_var), TotalExposure, UseElectricity,
         electBaseprice, electVATfreeprice, electFinalprice) %>% #electVATfreeprice #electFinalprice
  group_by(NACE_Name) %>%
  mutate(TotalExposure = (TotalExposure+0.01)/1000000,
         CostsElectBaseprice = electBaseprice * UseElectricity,
         norm_Exposure = (TotalExposure/TotalExposure[year == base_year])*100,
         norm_ElectPrice = (CostsElectBaseprice/CostsElectBaseprice[year == base_year])*100) %>%
  ungroup()

# Run Fixed_Effects Estimation
{
  # Pooled OLS
  model_simple <- lm(envregulation ~ norm_Exposure  + norm_ElectPrice , data = testtt)
  summary(model_simple)
  
  # Fixed Effects with only FIT Exposure and year FE
  model_fe_simple <- felm(envregulation ~ norm_Exposure | # Model Variable
                        year | # Fixed Effects
                        0 | # Instrument
                        NACE_Name, # Variables for Cluster-robust Standard errors
                      data = testtt, cmethod = 'cgm2')
  summary(model_fe_simple)
  
  # Fixed Effects with FIT Exposure & Electricity and year FE
  model_fe_full <- felm(envregulation ~ norm_Exposure + norm_ElectPrice | # Model Variable
                     year | # Fixed Effects
                     0 | # Instrument
                     NACE_Name, # Variables for Cluster-robust Standard errors
                   data = testtt, cmethod = 'cgm2')
  summary(model_fe_full)
  
  # Fixed Effects with FIT Exposure & Electricity and year and sector FE
  model_doublefe_full <- felm(envregulation ~ norm_Exposure + norm_ElectPrice | # Model Variable
                          year + NACE_Name| # Fixed Effects
                          0 | # Instrument
                          NACE_Name, # Variables for Cluster-robust Standard errors
                        data = testtt, cmethod = 'cgm2')
  summary(model_doublefe_full)
}
  
# Export to LATEX
{
  stargazer(model_simple, model_fe_simple, model_fe_full, model_doublefe_full, 
            title="Comparison of Model Results", 
            header=FALSE, 
            type="latex", 
            model.numbers=TRUE, 
            column.labels=c("(1)", "(2)", "(3)", "(4)"), 
            covariate.labels=c("Exposure", "Electricity Costs"),
            omit.stat=c("LL", "ser", "f"), 
            align=TRUE)
}

bilat_11 <- c(0.1053, 0.0434, 0.0143, 0.0545, 0.0264, 0.066, 0.0288, 0.0268,
              0.0547, 0.0455, 0.0670, 0.0419, 0.0305, 0.0122, 0.0677, 0.0146,
              0.0226)
beta <- c(0.1389, 0.0592, 0.0195, 0.1028, 0.0603, 0.0948, 0.0364, 0.0218, 0.0488,
         0.0576, 0.0582, 0.0500, 0.0420, 0.0243, 0.0984, 0.0422, 0.0448)
vship_01 <-c(0.1070 , 0.0473, 0.0148, 0.0557, 0.0281, 0.0681, 0.0296, 0.0273, 0.0567,
             0.0463, 0.0701, 0.0456, 0.0337, 0.0133, 0.0743, 0.0157, 0.0246)
vship_02 <- c( 0.0350, 0.0117, 0.0047, 0.0258, 0.0141, 0.0250, 0.0089, 0.0052, 0.0113,
               0.0142, 0.0150, 0.0120, 0.0092, 0.0064, 0.0208, 0.0126, 0.0097)


