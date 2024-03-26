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

# Load Data
{
  dta_parameter <- readRDS("./Data/dta_parameter.rds")
  #dta_internat <- readRDS("./Data/dta_internat.rds")
}

# Define variables for flexibility in Code
ghg <- 'CO2ElectricityHeat' # 'CO2Total' # Greenhouse gas for the analysis to allow flexibility in choice
alpha <- 0.011 # mean Pollution elasticity
base_year <- 2003 # Base year for parameter
end_year <- 2014

# Calculate shocks
{
  dta_shocks <- dta_parameter %>%
    distinct() %>%
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

# Merge final data
{
  #dta_policy <- dta_parameter %>%
  #  left_join(dta_shocks %>%
  #              mutate(year = as.character(year)) %>%
  #              select(ISIC_Name, NACE_Name, year, envregulation),
  #            join_by(ISIC_Name == ISIC_Name, NACE_Name == NACE_Name, year == year))
}

# Data for MATLAB version
{
  dta_MATLAB_l1_35 <- dta_parameter %>%
    filter(year <= end_year) %>%
    select(NACE_Name, year, !!sym(ghg),
           EXP, DomDom, DomImp, ROWROW, vship,
           elasticitysubstitution, inputshare, pollutionelasticityNACE3, ParetoShape) %>%
    group_by(NACE_Name) %>%
    mutate(sigma= elasticitysubstitution[year==base_year], 
           alpha=pollutionelasticityNACE3[year==base_year],
           theta = ParetoShape[year==base_year]) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(vshipROW = ROWROW + DomImp,
           vshipDNK = DomDom + EXP,
           Eds_DNK = DomDom+DomImp,
           Eds_ROW = ROWROW+EXP,
           Rds_DNK = DomDom+EXP,
           Rds_ROW = ROWROW+DomImp,
           beta_DNK = Eds_DNK/Eds_DNK[NACE_Name == "Total Manufacturing"],
           beta_ROW = Eds_ROW/Eds_ROW[NACE_Name == "Total Manufacturing"],
           
           lambda_DomDom = DomDom/(Eds_DNK),
           lambda_DomImp = DomImp/(Eds_DNK),
           lambda_EXP = EXP/(Eds_ROW),
           lambda_ROWROW = ROWROW/(Eds_ROW),
           
           zeta_DomDom = DomDom/(Rds_DNK),
           zeta_DomImp = DomImp/(Rds_ROW),
           zeta_EXP = EXP/(Rds_DNK),
           zeta_ROWROW = ROWROW/(Rds_ROW),
           
           Ed_DNK = DomDom[NACE_Name == 'Total Manufacturing'] +
             DomImp[NACE_Name == 'Total Manufacturing'],
           Ed_ROW = ROWROW[NACE_Name == 'Total Manufacturing'] +
             EXP[NACE_Name == 'Total Manufacturing'],
           Rd_DNK = DomDom[NACE_Name == 'Total Manufacturing'] +
             EXP[NACE_Name == 'Total Manufacturing'],
           Rd_ROW = ROWROW[NACE_Name == 'Total Manufacturing'] +
             DomImp[NACE_Name == 'Total Manufacturing'],
           NXds_DNK = Rds_DNK - Eds_DNK,
           NXds_ROW = Rds_ROW - Eds_ROW,
           NXd_DNK = Rd_DNK - Ed_DNK,
           NXd_ROW = Rd_ROW - Ed_ROW,
           
           NXAds_DNK = sum(NXds_DNK*(sigma-1) * (theta-alpha+1) / (theta*sigma), na.rm=TRUE),
           NXAds_ROW = sum(NXds_ROW*(sigma-1) * (theta-alpha+1) / (theta*sigma), na.rm=TRUE))
  
  # retrieve base things
  dta_MATLAB_l41_48 <- dta_MATLAB_l1_35 %>%
    group_by(NACE_Name)
  
  dta_MATLAB_l54_63 <- dta_MATLAB_l1_35 %>%
    filter(NACE_Name != 'Total Manufacturing') %>%
    group_by(year) %>%
    mutate(w_base_DNK = sum(vshipDNK),
           w_base_ROW = sum(vshipROW)) %>%
    ungroup() %>%
    mutate(w_hat_DNK = w_base_DNK / w_base_DNK[year == base_year],
           w_hat_ROW = w_base_ROW / w_base_ROW[year == base_year]) %>%
    group_by(NACE_Name) %>%
    mutate(lambda_hat_DomDom = lambda_DomDom / lambda_DomDom[year == base_year],
           lambda_hat_DomImp = lambda_DomImp / lambda_DomImp[year == base_year],
           lambda_hat_EXP = lambda_EXP / lambda_EXP[year == base_year],
           lambda_hat_ROWROW = lambda_ROWROW / lambda_ROWROW[year == base_year],
           shocks.beta_hat_DNK = beta_DNK / beta_DNK[year == base_year],
           shocks.beta_hat_ROW = beta_ROW / beta_ROW[year == base_year],
           Z_hat = !!sym(ghg) / (!!sym(ghg))[year == base_year],
           Rds_hat_DNK = Rds_DNK / Rds_DNK[year == base_year],
           Rds_hat_ROW = Rds_ROW / Rds_ROW[year == base_year],
           shocks.NXd_hat_DNK = NXd_DNK / NXd_DNK[year == base_year],
           shocks.NXd_hat_ROW = NXd_ROW / NXd_ROW[year == base_year],
           shocks.NXAds_hat_DNK = NXAds_DNK / NXAds_DNK[year == base_year],
           shocks.NXAds_hat_ROW = NXAds_ROW / NXAds_ROW[year == base_year],
           
           # Matlab Line 62 & 63
           #w_oNJY_DNK = w_hat_DNK, # not included because in MATLAB only because of formatting
           #w_oNJY_ROW = w_hat_ROW, # not included because in MATLAB only because of formatting
           M_hat_DNK = Rds_hat_DNK / w_hat_DNK,
           M_hat_ROW = Rds_hat_ROW / w_hat_ROW) %>%
    ungroup()
  
  dta_MATLAB_l66_81 <- dta_MATLAB_l54_63 %>%
    mutate(pwrE = 1-theta / ((sigma-1)*(1-alpha))) %>%
    group_by(NACE_Name) %>%
    # Create MATLAB: 'shocks.Gamma_hat_star'
    mutate(shocks.Gamma_hat_star_DomDom = (lambda_hat_DomDom/(M_hat_DNK * (w_hat_DNK^(-theta)))) *
             (((shocks.beta_hat_DNK/w_hat_DNK) * (Rd_DNK-NXd_DNK) /
             (Rd_DNK[year == base_year] - NXd_DNK[year == base_year]))^pwrE),
           
           shocks.Gamma_hat_star_EXP = (lambda_hat_EXP/(M_hat_DNK * (w_hat_DNK^(-theta)))) *
             (((shocks.beta_hat_ROW/w_hat_ROW) * (Rd_ROW-NXd_ROW) /
                (Rd_ROW[year == base_year] - NXd_ROW[year == base_year]))^pwrE),
           
           shocks.Gamma_hat_star_DomImp = (lambda_hat_DomImp/(M_hat_ROW * (w_hat_ROW^(-theta)))) *
             (((shocks.beta_hat_DNK/w_hat_DNK) * (Rd_DNK-NXd_DNK) /
                 (Rd_DNK[year == base_year] - NXd_DNK[year == base_year]))^pwrE),
           
           shocks.Gamma_hat_star_ROWROW = (lambda_hat_ROWROW/(M_hat_ROW * (w_hat_ROW^(-theta)))) *
             (((shocks.beta_hat_ROW/w_hat_ROW) * (Rd_ROW-NXd_ROW) /
                 (Rd_ROW[year == base_year] - NXd_ROW[year == base_year]))^pwrE),
           
           t_hat = (M_hat_DNK * w_hat_DNK)/Z_hat,
           
           shocks.Gamma_hat_t = t_hat^((-alpha*theta)/(1-alpha)),
           shocks.Gamma_hat_foreign_DomDom = 1,
           shocks.Gamma_hat_foreign_EXP = 1,
           shocks.Gamma_hat_foreign_DomImp = shocks.Gamma_hat_star_DomImp,
           shocks.Gamma_hat_foreign_ROWROW = shocks.Gamma_hat_star_ROWROW,
           
           shocks.Gamma_hat_domestic_DomDom = shocks.Gamma_hat_star_DomDom/shocks.Gamma_hat_t,
           shocks.Gamma_hat_domestic_EXP = shocks.Gamma_hat_star_EXP/shocks.Gamma_hat_t,
           shocks.Gamma_hat_domestic_DomImp = NA,
           shocks.Gamma_hat_domestic_ROWROW = NA)
  
  #wwM_hat = w_hat_DNK # Matrix where the first row is w_hat_DNK and the rest M_hat in general
                        # second row the values for ROW industry 1
                        # third row the values for DNK industry 1
                        # fourth row the values for ROW industry 2
                        # fifth row the values for DNK industry 1
                      #...
                        # 34th row the values for ROW industry 17
                        # 35th row the values for DNK industry 17
  # takes both M_hat (ROW and DNK)
    
}

# TEST environment for "nleqslv"
{
  # Defining Multivariate function to solve for
  fn <- function(x) {
    
    rate <- x[1] * x[2] - 5
    shape <- sqrt(x[1] * x[2]^2) - 10
    
    return(c(rate, shape))
  }
  
  nleqslv(x=c(0.75, 1.25), # numeric vector with an initial guess of the root of the function # Shapiro choose 0.75 until 1.25
          fn=fn, #A function of x returning a vector of function values with the same length as the  vector x.
          global = 'pwldog') 
}

# Apply algorithm to "nleqslv"
{
  # Factors used: Trade_od,s & Emissions Z_o,s, three parameters
  # Net Exports = countrys exports - imports
  # from example in 'TEST environment' the x-values are the potential values used in shocks
  # need to return the wages and firm entry changes that make the equation hold
  
  #algorithm_data <- d
}

# Plot with Environmental regulation tax selected Industries - 'Landscape' 8.00 x 6.00
{
  dta_env_plot_end <- dta_MATLAB_l66_81 %>%
    filter(#NACE_Name == 'Total Manufacturing' &
      NACE_Name %in% c('Total Manufacturing',"Chemicals and pharmaceuticals",
                       "Food, beverages, tobacco", "Metal products, electronics, machinery") & 
        year >= base_year & year <= end_year) %>%
    select(NACE_Name, year, t_hat) %>%
    mutate(year = as.numeric(year),
           t_hat = t_hat * 100)
  
  year_breaks <- seq(from=base_year, to=end_year, by = 2)
  lplot_env_end <- ggplot(data = dta_env_plot_end,
                          aes(x = year, y = t_hat, color = NACE_Name, group = NACE_Name)) +
    geom_line() +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = paste0("Base ", base_year, " = 100"),
      color = NULL) +
    scale_x_continuous(breaks = year_breaks) +
    theme_classic() +
    theme(legend.position = c(.20, .85))
  lplot_env_end
}

# Plot Endogeneous Firm Entry and Wages
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
  combined_Endo_lplot <- plot_Wages + plot_FirmEntry + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
       legend.title = element_blank())
  combined_Endo_lplot
  
}

# Merge data
{
  dta_policy <- dta_parameter %>%
    left_join((dta_MATLAB_l66_81 %>%
                 select(NACE_Name, year, t_hat)), join_by(NACE_Name == NACE_Name, year == year))
}

# Save Data
{
  saveRDS(dta_shocks, file = "./Data/dta_shocks.rds")
  saveRDS(dta_policy, file = "./Data/dta_policy.rds")
}