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
group_ind <- 117 # either 69 or 117
excl_117 <- c("Total", "Households", "Total industries")
excl_69 <- c("Total")


# Create Data with parameters
{
  "
  Note: Greenhouse gases are in 1,000 tonnes, whereas output and costs data is in million DKK
  "
  dta_parameter <- dta_decomp %>%
    filter(!(classif %in% excl_117) & group == group_ind) %>%
    group_by(year) %>%
    mutate(tonsPollCost = (!!sym(ghg)*1000)/!!sym(costs), # Calculating tons pollution per dollar costs
           meantonsPollCost = sum(tonsPollCost)/group_ind,
           pollutionelasticity = alpha * (tonsPollCost/meantonsPollCost), # Calculating the Pollution elasticity with the mean alpha defined prior
           inputshare = !!sym(costs)/realoutput, # Calculating the input share with costs defined prior
           elasticitysubstitution = 1/(1-inputshare)) %>% # Calculating the Elasticity of Substitution by taking the ratio of the value of shipments to production costs, hence relying on assumption that firms engage in monopolistic competition
    ungroup()
  
  # Test whether pollution Elasticity has been correctly calculated
  mean((dta_parameter %>% filter(year == 2000))$pollutionelasticity)
  
  attr(dta_parameter$tonsPollCost, 'label') <- 'Tons pollution per 1,000,000 DKK'
}

# Exporting Parameters for year = 2000 to LATEX
{
  # Create a Table for LATEX format
  table_parameters <- xtable(x = (dta_parameter %>%
                                  mutate(classif = substr(classif, 8, 1000000L)) %>%
                                  filter(year == 2000) %>%
                                  select(classif, tonsPollCost, pollutionelasticity, inputshare, elasticitysubstitution) %>%
                                  arrange(desc(tonsPollCost)) %>% # Arrange table in descending order by tons Pollution per m DKK costs
                                  slice(1:15)),
                           digits = c(2,2,2,4,2,2)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table_parameters) <- c("Industry", "Tons pollution per m DKK costs",
                             "Pollution elasticity", "Input Share", "Elasticity of Substitution")
  
  # Print Parameters table in LATEX format
  print(table_parameters, include.rownames=FALSE)
}