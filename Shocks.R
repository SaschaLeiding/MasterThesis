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
  dta_internat <- readRDS("./Data/dta_internat.rds")
}

# Define variables for flexibility in Code
ghg <- 'CO2ElectricityHeat' # Greenhouse gas for the analysis to allow flexibility in choice
#costs <- 'realexpend' # Define column used as Costs for calculations
alpha <- 0.011 # mean Pollution elasticity
base_year <- 2003 # Base year for parameter
end_year <- 2016

# Calculate shocks
{
  dta_shocks <- dta_parameter %>%
    group_by(ISIC_Name, NACE_Name) %>%
    mutate(year = as.integer(year),
           chngfirmEntry = firmEntry/firmEntry[year==base_year],
           chngghg = !!sym(ghg)/(!!sym(ghg))[year==base_year],
           chngwages = wage_manuf/wage_manuf[year==base_year],
           envregulation =  ((chngfirmEntry*chngwages)/chngghg)*100) %>%
    #select(ISIC_Name, NACE_Name, year, realoutput, firmEntry, wage_manuf, CompEmployees, Employees,!!sym(ghg),
    #       envregulation) %>%
    ungroup()
}

# Plot for Chemicals, Food, Electrical (ISIC) - 'Landscape' 8.00 x 6.00
{
  dta_env_plot <- dta_shocks %>%
    filter(#ISIC_Name == 'Total Manufacturing' & 
             ISIC_Name %in% c('Total Manufacturing',"Chemicals", "Food, beverages, tobacco", "Machinery and equipment") & 
             year >= base_year & year <= end_year) %>%
    select(ISIC_Name, year, envregulation) 
  
  year_breaks <- seq(from=base_year, to=end_year, by = 2)
  lplot_env <- ggplot(data = dta_env_plot, aes(x = year, y = envregulation, color = ISIC_Name, group = ISIC_Name)) +
    geom_line() +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = paste0("Base ", base_year, " = 100"),
      color = NULL) +
    scale_x_continuous(breaks = year_breaks) +
    theme_classic() +
    theme(legend.position = c(.15, .78))
  lplot_env
}

# Table 5: Exporting Chemical Industry values for Environmental Shocks
{
  # Create a Table for LATEX format
  table5 <- xtable(x = dta_shocks %>%
                     filter(ISIC_Name == "Total Manufacturing" & year >=base_year & year <= end_year) %>%
                     select(year, realoutput, !!sym(ghg), firmEntry,
                            CompEmployees, Employees, wage_manuf,
                            envregulation),
                   digits = c(2,2,0,0,0,0,0,4,0)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table5) <- c("Year",
                     "Real Output", "Emissions","Firm Entry",
                     "Compensation", "Employees", "Wages",
                     "Environmental Regulation")
  
  # Print Parameters table in LATEX format
  print(table5, 
        include.rownames=FALSE, 
        format.args = list(big.mark = ",", decimal.mark = "."))
}

# Merge final data
{
  dta_policy <- dta_parameter %>%
    left_join(dta_shocks %>%
                mutate(year = as.character(year)) %>%
                select(ISIC_Name, NACE_Name, year, envregulation),
              join_by(ISIC_Name == ISIC_Name, NACE_Name == NACE_Name, year == year))
}

# Save Data
{
  saveRDS(dta_shocks, file = "./Data/dta_shocks.rds")
  saveRDS(dta_policy, file = "./Data/dta_policy.rds")
}