"
This Script is to construct the environmental regulation tax

The input for this script is the data 'dta_parameter.rds' from the script 
'Parameter_Estimates.R'.
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
  dta_parameter <- readRDS("./Data/dta_parameter.rds")
}
# Define variables for flexibility in Code
ghg <- 'GHGinclBiomass' # Greenhouse gas for the analysis to allow flexibility in choice
costs <- 'realexpend' # Define column used as Costs for calculations
alpha <- 0.011 # mean Pollution elasticity
base_year <- 2005 # Base year for parameter

# Calculate shocks
{
  dta_shocks <- dta_parameter %>%
    group_by(ISIC_Name, NACE_Name) %>%
    mutate(year = as.integer(year),
           wages = CompEmployees/Employees,
           chngfirmEntry = firmEntry/firmEntry[year==base_year],
           chngghg = !!sym(ghg)/(!!sym(ghg))[year==base_year],
           chngwages = wages/wages[year==base_year],
           envregulation =  (chngfirmEntry*chngwages)/chngghg) %>%
    select(ISIC_Name, NACE_Name, year, firmEntry, wages, !!sym(ghg),
           envregulation)
}