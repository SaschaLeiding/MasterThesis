"
This Script is to construct the environmental regulation tax

The input for this script is the data 'dta_parameter.rds' from the script 
'Parameter_Estimates.R'.
"

# Install & Load Packages
{
  #install.packages("tidyverse")
  #install.packages("xtable")
  #install.packages("nleqslv")
  #install.packages("patchwork")
  
  library(tidyverse)
  library(xtable)
  library(nleqslv)
  library(patchwork)
}

# Load standard Data and MATLAB results
{
  dta_parameter <- readRDS("./Data/dta_parameter.rds")
}

# Define variables for flexibility in Code
ghg <- 'CO2ElectricityHeat' # 'CO2Total' # Greenhouse gas for the analysis to allow flexibility in choice
alpha <- 0.011 # mean Pollution elasticity
base_year <- 2003 # Base year for parameter
end_year <- 2014



# Plot with exogeneous wage & firm entry for Chemicals, Food, Electrical (ISIC) - 'Landscape' 8.00 x 6.00
{
  dta_env_plot_exg <- dta_shocks %>%
    filter(#NACE_Name == 'Total Manufacturing' &
      NACE_Name %in% c('Total Manufacturing',"Chemicals and pharmaceuticals",
                       "Food, beverages, tobacco", "Metal products, electronics, machinery") & 
        year >= base_year & year <= end_year) %>%
    select(NACE_Name, year, envregulation) 
  
  year_breaks <- seq(from=base_year, to=end_year, by = 2)
  lplot_env_exg <- ggplot(data = dta_env_plot_exg,
                          aes(x = year, y = envregulation, color = NACE_Name, group = NACE_Name)) +
    geom_line() +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = paste0("Base ", base_year, " = 100"),
      color = NULL) +
    scale_x_continuous(breaks = year_breaks) +
    theme_classic() +
    theme(legend.position = c(.15, .78))
  lplot_env_exg
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

# Plot Endogeneous Firm Entry and Wages - 'Landscape' 11.00 - 7.00
{
  dta_endog_lplot <- dta_MATLAB_l66_81 %>%
    group_by(year) %>%
    mutate(avgM_hat_DNK = mean(M_hat_DNK),
           avgM_hat_ROW = mean(M_hat_ROW)) %>%
    select(NACE_Name, year, w_hat_DNK, w_hat_ROW, avgM_hat_DNK, avgM_hat_ROW) %>%
    filter(NACE_Name == "Basic Metals") %>% # arbitrarily chosen as all values are averages across sectors
    pivot_longer(cols = 3:ncol(.), names_to = "Variable", values_to = "Value" ) %>%
    mutate(country = ifelse(str_detect(Variable, "ROW$"), "ROW", "DNK"),
           Value = Value*100) %>%
    #mutate(across(is.numeric, ~.x*100)) %>%
    ungroup()
  
  plot_Wages <- ggplot(data = (dta_endog_lplot %>%
                                 filter(str_detect(Variable, "w_hat"))),
                       aes(x = year, y = Value, color = country, group = country)) +
    geom_line() +
    #scale_x_continuous(breaks = seq(2003, max(dta_endog_lplot$year, na.rm = TRUE), by = 1), limits = c(2003, NA)) +
    labs(x = "Year",
         y = paste0("Base ", base_year, " = 100"),
         color = NULL)+
    theme_classic()
  plot_Wages
  
  plot_FirmEntry <- ggplot(data = (dta_endog_lplot %>%
                                     filter(str_detect(Variable, "M_hat"))),
                           aes(x = year, y = Value, color = country, group = country)) +
    geom_line() + 
    labs(x = "Year",
         y = paste0("Base ", base_year, " = 100"),
         color = NULL) +
    theme_classic()
  plot_FirmEntry
  
  # Plot in 2x1 - Export as Landscape as 11.00 - 7.00
  label_plots <- map(c("Wage", "Firm Entry"), 
                     ~ ggplot() +
                       theme_void() +
                       theme(plot.margin = margin(0, 0)) +
                       annotate("text", x = 0.5, y = 0.5, label = .x,
                                fontface = "bold", hjust = 0.5))
  
  
  combined_Endo_lplot <- (Reduce('+', label_plots) + plot_layout(ncol = 2)) /
    ( plot_Wages + plot_FirmEntry) +
    plot_layout(guides = 'collect', axis_titles = "collect", heights = c(1, 10)) &
    theme(legend.position = "bottom",
          legend.title = element_blank())
  combined_Endo_lplot
  
}

# Save Data
{
  #saveRDS(dta_shocks, file = "./Data/dta_shocks.rds")
  #saveRDS(dta_policy, file = "./Data/dta_policy.rds")
}