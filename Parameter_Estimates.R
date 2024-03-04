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
  dta_inter <- dta_analysis %>%
    filter(NACE_Name != 'Total Manufacturing' | ISIC_Name != 'Total Manufacturing') %>%
    
    # ISIC: Calculate Parameters with Shapiro and Walker(2018) Estimation method
    group_by(year, classsystem) %>%
    mutate(tonsPollCost = (!!sym(ghg)*1000)/!!sym(costs), # Calculating tons pollution per dollar costs
           meantonsPollCost = sum(tonsPollCost, na.rm=TRUE)/ifelse(classsystem=='NACE', 11, 16),
           pollutionelasticityISIC = alpha * (tonsPollCost/meantonsPollCost), # Calculating the Pollution elasticity with the mean alpha defined prior
           inputshare = !!sym(costs)/realoutput, # Calculating the input share with costs defined prior
           elasticitysubstitution = 1/(1-inputshare)) %>% # Calculating the Elasticity of Substitution by taking the ratio of the value of shipments to production costs, hence relying on assumption that firms engage in monopolistic competition
    ungroup() %>%
    
    # NACE_1: Calculate Pollution Elasticity with ZEW (2023) Estimation Method
    group_by(NACE_Name, ISIC_Name) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(lnrealoutput = log(realoutput),
           lnrealcostEnergy = log(realcostEnergy),
           lnghg = log(!!sym(ghg)),
           energyshare = realcostEnergy/realoutput,
           lnenergyshare = log(energyshare),
           
           chngOutputEnergyNACE = (lead(energyshare) - energyshare)/(lead(realoutput) - realoutput),
           elasticityOutputEnergyNACE1 = (1/chngOutputEnergyNACE) * (energyshare/realoutput),
           
           chngEmissionEnergyNACE = (lead(realcostEnergy) - realcostEnergy)/(lead(!!sym(ghg)) - !!sym(ghg)),
           elasticityEmissionEnergyNACE1 = (1/chngEmissionEnergyNACE) * (realcostEnergy/!!sym(ghg)),
           pollutionelasticityNACE1 = elasticityOutputEnergyNACE1/elasticityEmissionEnergyNACE1) %>%
  ungroup()
  
  # Test whether pollution Elasticity has been correctly calculated
  mean((dta_inter %>% filter(year == base_year & classsystem=='NACE'))$pollutionelasticityISIC)
  mean((dta_inter %>% filter(year == base_year & classsystem=='ISIC'))$pollutionelasticityISIC)
  
  # Add Label to column 'tonspollcost'
  attr(dta_inter$tonsPollCost, 'label') <- 'Tons pollution per 1,000,000 DKK'
}

dta_elast <- data.frame(NACE_Name = character(0), 
                        elasticityOutputEnergyNACE2 = numeric(0), 
                        elasticityEmissionEnergyNACE2 = numeric(0),
                        pollutionelasticityNACE2 = numeric(0))

for(i in na.omit(unique(dta_inter$NACE_Name))){
  dta_model <- dta_inter %>%
    filter(NACE_Name == i & !is.na(NACE_Name) & year < 2017)
  
  model_test_OutputEnergy <- lm(energyshare ~ lnrealcostEnergy, data = dta_model)
  model_test_EmissionsEnergy <- lm(lnghg ~ lnrealcostEnergy, data = dta_model)
  
  elasticityOutputEnergyNACE2 <- coef(model_test_OutputEnergy)[2]
  elasticityEmissionEnergyNACE2 <- coef(model_test_EmissionsEnergy)[2]
  pollutionelasticityNACE2 = elasticityOutputEnergyNACE2/elasticityEmissionEnergyNACE2
  
  dta_elast <- rbind(dta_elast,
                     data.frame(NACE_Name = i,
                                elasticityOutputEnergyNACE2 = elasticityOutputEnergyNACE2, 
                                elasticityEmissionEnergyNACE2 = elasticityEmissionEnergyNACE2,
                                pollutionelasticityNACE2 = pollutionelasticityNACE2))
}

# Add 'dta_elast' to dta_inter to create 
{
  dta_parameter <- dta_inter %>%
    left_join(dta_elast, join_by(NACE_Name == NACE_Name))
}

dta_US <- data.frame(ISIC_NAME = na.omit(unique(dta_parameter$ISIC_Name)),
                     USpollelasti = c(0.5557, 0.0205, 0.0212, 0.0019, 0.0040,
                                      0.0047, 0.0015, 0.0014, 0.0016, NA,
                                      0.0303, 0.0019, 0.0223, 0.0048, 0.0022,
                                      0.0103))
dta_GER <- data.frame(NACE_Name = na.omit(unique(dta_parameter$NACE_Name)),
                      GERenergyOutputElasticity = c(0.061, 0.034, 0.007, 0.016,
                                                 0.010, 0.065, 0.059, 0.022,
                                                 0.018, 0.008, 0.031),
                      GERenergyEmissionElasticity = c(0.993, 0.993, 1.001, 0.974,
                                                   1.011, 0.946, 0.962, 1.012,
                                                   1.002, 1.001, 0.873),
                      GERpollutionelasticity = c(0.063, 0.041, 0.009, 0.020,
                                                 0.010, 0.078, 0.058, 0.024,
                                                 0.019, 0.008, 0.038))
# Table 2: Exporting all Parameters for base year to LATEX
{
  # Create a Table for LATEX format
  table2_ALLparameters <- xtable(x = (dta_parameter %>%
                                  filter(year == base_year & classsystem == 'ISIC') %>%
                                  arrange(ISIC_Code) %>%
                                  select(ISIC_Name, tonsPollCost,
                                         pollutionelasticityISIC, pollutionelasticityNACE1,
                                         inputshare, elasticitysubstitution)),
                           digits = c(2,2,2,4,4,2,2)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table2_ALLparameters) <- c("Industry", "Tons pollution per m DKK costs",
                             "Pollution elasticity", "Pollution elasticity NACE",
                             "Input Share", "Elasticity of Substitution")
  
  # Print Parameters table in LATEX format
  print(table2_ALLparameters, include.rownames=FALSE)
}

# Table 3: Exporting all Pollution Elasticities
{
  
}

# Table 4: Exporting elasticities from ZEW method to LATEX
{
  # Create a Table for LATEX format
  table_elasticities <- xtable(x = (dta_parameter %>%
                                       filter(year == base_year & classsystem == 'NACE') %>%
                                       arrange(NACE_Code) %>%
                                       select(NACE_Name, elasticityOutputEnergyNACE2,
                                              elasticityEmissionEnergyNACE2, pollutionelasticityNACE2)),
                                digits = c(2,2,3,3,3)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table_elasticities ) <- c("Industry", "Energy Output elasticity",
                                  "Energy Emission elasticity", "Pollution elasticity")
  
  # Print Parameters table in LATEX format
  print(table_elasticities , include.rownames=FALSE)
}