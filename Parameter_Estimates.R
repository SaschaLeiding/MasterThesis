"
This Script is to do the Parameter Estimates per industry and time.

The input for this script is the data 'dta_analysis.rds' from the script 
'Import_Merge.R'.

The output for this script are Table X and X.

Attention: Variable 'ghg' must be the same in all Scripts
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
  dta_analysis <- readRDS("./Data/dta_analysis.rds")
}

# Define variables for flexibility in Code
ghg <- 'GHGinclBiomass' # Greenhouse gas for the analysis to allow flexibility in choice
costs <- 'realexpend' # Define column used as Costs for calculations
alpha <- 0.011 # mean Pollution elasticity
base_year <- 2005 # Base year for parameter

# Create Data with parameters
{
  "
  Note: Greenhouse gases are in 1,000 tonnes, whereas output and costs data is in million DKK
  "
  dta_parameter <- dta_analysis %>%
    filter(NACE_Name != 'Total Manufacturing' | ISIC_Name != 'Total Manufacturing') %>%
    
    # Calculate Parameters with Shapiro and Walker (2018) Estimation method
    group_by(year, classsystem) %>%
    mutate(tonsPollCost = (!!sym(ghg)*1000)/!!sym(costs), # Calculating tons pollution per dollar costs
           meantonsPollCost = sum(tonsPollCost, na.rm=TRUE)/ifelse(classsystem=='NACE', 11, 16),
           pollutionelasticity = alpha * (tonsPollCost/meantonsPollCost), # Calculating the Pollution elasticity with the mean alpha defined prior
           inputshare = !!sym(costs)/realoutput, # Calculating the input share with costs defined prior
           elasticitysubstitution = 1/(1-inputshare)) %>% # Calculating the Elasticity of Substitution by taking the ratio of the value of shipments to production costs, hence relying on assumption that firms engage in monopolistic competition
    ungroup() %>%
    
    # Calculate Pollution Elasticity with ZEW (2023) Estimation Method
    group_by(NACE_Name, ISIC_Name) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(energyshare = realcostEnergy/realoutput,
           
           chngOutputEnergyNACE = (lead(energyshare) - energyshare)/(lead(realoutput) - realoutput),
           elasticityOutputEnergyNACE = (1/chngOutputEnergyNACE) * (energyshare/realoutput),
           
           chngEmissionEnergyNACE = (lead(realcostEnergy) - realcostEnergy)/(lead(!!sym(ghg)) - !!sym(ghg)),
           elasticityEmissionEnergyNACE = (1/chngEmissionEnergyNACE) * (realcostEnergy/!!sym(ghg)),
           pollutionelasticityNACE = elasticityOutputEnergyNACE/elasticityEmissionEnergyNACE) %>%
  ungroup()
  
  # Test whether pollution Elasticity has been correctly calculated
  mean((dta_parameter %>% filter(year == base_year & classsystem=='NACE'))$pollutionelasticity)
  mean((dta_parameter %>% filter(year == base_year & classsystem=='ISIC'))$pollutionelasticity)
  
  # Add Label to column 'tonspollcost'
  attr(dta_parameter$tonsPollCost, 'label') <- 'Tons pollution per 1,000,000 DKK'
}

# Exporting all Parameters for base year to LATEX
{
  # Create a Table for LATEX format
  table_ALLparameters <- xtable(x = (dta_parameter %>%
                                  filter(year == base_year & classsystem == 'ISIC') %>%
                                  arrange(ISIC_Code) %>%
                                  select(ISIC_Name, tonsPollCost,
                                         pollutionelasticity, pollutionelasticityNACE,
                                         inputshare, elasticitysubstitution)),
                           digits = c(2,2,2,4,4,2,2)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table_ALLparameters) <- c("Industry", "Tons pollution per m DKK costs",
                             "Pollution elasticity", "Pollution elasticity NACE",
                             "Input Share", "Elasticity of Substitution")
  
  # Print Parameters table in LATEX format
  print(table_ALLparameters, include.rownames=FALSE)
}

# Exporting elasticities from ZEW method to LATEX
{
  # Create a Table for LATEX format
  table_elasticities <- xtable(x = (dta_parameter %>%
                                       filter(year == base_year & classsystem == 'NACE') %>%
                                       arrange(NACE_Code) %>%
                                       select(NACE_Name, elasticityOutputEnergyNACE,
                                              elasticityEmissionEnergyNACE, pollutionelasticityNACE)),
                                digits = c(2,2,3,3,3)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table_elasticities ) <- c("Industry", "Energy Output elasticity",
                                  "Energy Emission elasticity", "Pollution elasticity")
  
  # Print Parameters table in LATEX format
  print(table_elasticities , include.rownames=FALSE)
}