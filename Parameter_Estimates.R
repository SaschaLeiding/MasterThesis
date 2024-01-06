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
  dta_decomp <- readRDS("./Data/dta_analysis.rds")
}

# Define variables for flexibility in Code
ghg <- 'GHGinclBiomass' # Greenhouse gas for the analysis to allow flexibility in choice
costs <- 'realexpend' # Define column used as Costs for calculations
alpha <- 0.011 # mean Pollution elasticity

# Create Data with parameters
{
  dta_parameter <- dta_decomp %>%
    filter(classif != 'Total') %>%
    group_by(year) %>%
    mutate(tonsPollCost = !!sym(ghg)/!!sym(costs), # Calculating tons pollution per dollar costs
           meantonsPollCost = sum(tonsPollCost)/117,
           pollutionelasticity = alpha * (tonsPollCost/meantonsPollCost), # Calculating the Pollution elasticity with the mean alpha defined prior
           inputshare = !!sym(costs)/realoutput, # Calculating the input share with costs defined prior
           elasticitysubstitution = 1/(1-inputshare)) %>% # Calculating the Elasticity of Substitution by taking the ratio of the value of shipments to production costs, hence relying on assumption that firms engage in monopolistic competition
    ungroup()
  
  # Test whether pollution Elasticity has been correctly calculated
  mean((dta_parameter %>% filter(year == 2000))$pollutionelasticity)
}

# Exporting Parameters for year = 2000 to LATEX
{
  sectors <- c("")
xtable(x = (dta_parameter %>%
              mutate(classif = substr(classif, 8, 1000000L)) %>%
              filter(year == 2000) %>%
              select(classif, tonsPollCost, pollutionelasticity, inputshare, elasticitysubstitution)))
}