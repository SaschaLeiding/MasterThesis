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
  dta_decomp <- readRDS("./Data/dta_ZEW.rds")
}

# Define variables for flexibility in Code
ghg <- 'GHGinclBiomass' # Greenhouse gas for the analysis to allow flexibility in choice
costs <- 'realexpend' # Define column used as Costs for calculations
alpha <- 0.011 # mean Pollution elasticity
group_ind <- 11 # either 69 or 117 or 11 (ZEW)
excl_117 <- c("Total", "Households", "Total industries")
excl_69 <- c("Total")
base_year <- 2005 # Base year for parameter

# Create Data with parameters
{
  "
  Note: Greenhouse gases are in 1,000 tonnes, whereas output and costs data is in million DKK
  "
  dta_parameter <- dta_decomp %>%
    #filter(!(classif %in% excl_117) & group == group_ind) %>%
    #filter(!is.na(ZEW_Code)) %>%
    filter(ZEW_Name != 'Total Manufacturing') %>%
    group_by(year) %>%
    mutate(tonsPollCost = (!!sym(ghg)*1000)/!!sym(costs), # Calculating tons pollution per dollar costs
           meantonsPollCost = sum(tonsPollCost, na.rm=TRUE)/group_ind,
           pollutionelasticity = alpha * (tonsPollCost/meantonsPollCost), # Calculating the Pollution elasticity with the mean alpha defined prior
           inputshare = !!sym(costs)/realoutput, # Calculating the input share with costs defined prior
           elasticitysubstitution = 1/(1-inputshare)) %>% # Calculating the Elasticity of Substitution by taking the ratio of the value of shipments to production costs, hence relying on assumption that firms engage in monopolistic competition
    ungroup()
  
  # Test whether pollution Elasticity has been correctly calculated
  mean((dta_parameter %>% filter(year == base_year))$pollutionelasticity)
  
  attr(dta_parameter$tonsPollCost, 'label') <- 'Tons pollution per 1,000,000 DKK'
  
  
  # test ZEW Method with Point-slope Elasticity
  dta_parameter_ZEW <- dta_parameter %>%
    group_by(ZEW_Name) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(energyshare = realcostEnergy/realoutput,
           chngOutputEnergyZEW = (lead(energyshare) - energyshare)/(lead(realoutput) - realoutput),
           elasticityOutputEnergyZEW = (1/chngOutputEnergyZEW) * (energyshare/realoutput),
           
           chngEmissionEnergyZEW = (lead(realcostEnergy) - realcostEnergy)/(lead(!!sym(ghg)) - !!sym(ghg)),
           elasticityEmissionEnergyZEW = (1/chngEmissionEnergyZEW) * (realcostEnergy/!!sym(ghg)),
           
           pollutionelasticityZEW = elasticityOutputEnergyZEW/elasticityEmissionEnergyZEW) %>%
    select(ZEW_Name, year,
           elasticityOutputEnergyZEW, elasticityEmissionEnergyZEW,
           pollutionelasticity, pollutionelasticityZEW) %>%
    filter(year == base_year)
}

# Exporting Parameters for base year to LATEX
{
  # Create a Table for LATEX format
  table_parameters <- xtable(x = (dta_parameter %>%
                                  #mutate(classif = substr(classif, 8, 1000000L)) %>%
                                  filter(year == base_year) %>%
                                  select(ZEW_Name, tonsPollCost, pollutionelasticity, inputshare, elasticitysubstitution)),
                           digits = c(2,2,2,4,2,2)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table_parameters) <- c("Industry", "Tons pollution per m DKK costs",
                             "Pollution elasticity", "Input Share", "Elasticity of Substitution")
  
  # Print Parameters table in LATEX format
  print(table_parameters, include.rownames=FALSE)
}