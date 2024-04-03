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
  library(readxl)
}

# Load Data
{
  dta_policy <- readRDS("./Data/dta_policy.rds")
  ShocksbyIndustry <- read_xls("./Data/ShocksbyIndustry.xls", range = "A1:BH11", col_names = FALSE)
  ShocksTotal <- read_xls("./Data/ShocksTotal.xls", range = "A1:E12", col_names = FALSE)
  wwM_hat <- read_xls("./Data/wwM_hat.xls", range = "A1:L23", col_names = FALSE)
  dta_t_hat <- read_xls("./Data/t_hat.xls", range = "A1:L11", col_names = FALSE)
  w_hat <- read_xls("./Data/w_hat.xls", range = "A1:L2", col_names = FALSE)
  
}

# Define variables for flexibility in Code
{
  ghg <- 'CO2ElectricityHeat'
  base_year <- 2003 # Base year for parameter
  end_year <- 2014
  y_var <- 't_hat_MATLAB'
}

# Transform MATLAB data
{
  colnames(ShocksTotal)[1] <- 'ForeignComp'
  colnames(ShocksTotal)[2] <- 'DNKComp'
  colnames(ShocksTotal)[3] <- 'ExpenditureShares'
  colnames(ShocksTotal)[4] <- 'EnvironmentalRegulation'
  #w_hat_DNK <- wwM_hat[1,]
  M_hat <- wwM_hat[-1,]
  
  # Merge data with dta_policy
  {
    dta_MATLAB <- dta_policy %>%
      left_join((ShocksTotal %>%
                   mutate(year = as.character(2002 + row_number()),
                          NACE_Name = 'Total Manufacturing')),
                join_by(year == year, NACE_Name == NACE_Name)) %>%
      left_join((dta_t_hat %>%
                   rename_with(~ as.character(2002 + seq_along(.)), .cols = everything()) %>%
                   mutate(NACE_Code = row_number()) %>%
                   pivot_longer(cols = starts_with("20"), values_to = 't_hat_MATLAB', names_to = 'year')),
                join_by(NACE_Code == NACE_Code, year == year)) %>%
      left_join((M_hat %>%
                   rename_with(~ as.character(2002 + seq_along(.)), .cols = everything()) %>%
                   mutate(NACE_Code = row_number()) %>%
                   pivot_longer(cols = starts_with("20"), values_to = 'M_hat_MATLAB', names_to = 'year')),
                join_by(NACE_Code == NACE_Code, year == year)) %>%
      left_join((w_hat %>%
                   rename_with(~ as.character(2002 + seq_along(.)), .cols = everything()) %>%
                   mutate(country = ifelse(row_number() %% 2 == 0, "DNK", "ROW")) %>%
                   pivot_longer(cols = starts_with("20"), values_to = 'w_hat', names_to = 'year') %>%
                   pivot_wider(names_from = 'country', values_from = 'w_hat', names_prefix = 'w_hat')),
                join_by(year == year)) %>%
      
      left_join((dta_t_hat %>%
                  rename_with(~ as.character(2002 + seq_along(.)), .cols = everything()) %>%
                  mutate(NACE_Code = row_number()) %>%
                  pivot_longer(cols = starts_with("20"), values_to = 't_hat_MATLAB', names_to = 'year') %>%
                  group_by(year) %>%
                  summarise(t_hat_MATLAB = mean(t_hat_MATLAB)) %>%
                  mutate(NACE_Name =  'Total Manufacturing')),
                join_by(NACE_Name == NACE_Name, year == year)) %>%
      unite('t_hat_MATLAB', c("t_hat_MATLAB.x", "t_hat_MATLAB.y"), na.rm = TRUE)
  }
}

# Plot with Environmental regulation tax selected Industries - 'Landscape' 8.00 x 6.00
{
  dta_env_plot_end <- dta_MATLAB %>%
    filter(NACE_Name %in% c('Total Manufacturing',"Chemicals and pharmaceuticals",
                            "Food, beverages, tobacco", "Metal products, electronics, machinery") & 
             year >= base_year & year <= end_year) %>%
    select(NACE_Name, year, t_hat_MATLAB) %>%
    #full_join(dta_env_Manuf) %>%
    group_by(year) %>%
    mutate(year = as.numeric(year),
           t_hat = as.numeric(t_hat_MATLAB) * 100)
  
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

# Plot Emission with Shocks - 'Landscape' 8.00 x 6.00
{
  dta_lplot_shocks <- dta_MATLAB %>%
    filter(NACE_Name == 'Total Manufacturing') %>%
    mutate(normalized_ghg = (!!sym(ghg) / (!!sym(ghg))[year == base_year]) * 100) %>%
    select(year, normalized_ghg, EnvironmentalRegulation, ExpenditureShares ) %>%#,
          # ForeignComp, DNKComp)
      pivot_longer(cols = -1, names_to = 'Shock', values_to = 'Values')
  
  lplot_shocks <- ggplot(data = dta_lplot_shocks,
                         aes(x = year, y = Values, color = Shock, group = Shock)) +
    geom_line() +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = paste0("Base ", base_year, " = 100"),
      color = NULL) +
    scale_colour_manual(values = c("#FF0000",  "#669900", "#330099"),
                        labels = c("Environmental Regulation",
                                   "Expenditure Shares",
                                   "CO² Electricity")) +
    theme_classic() +
    theme(legend.position = c(.15, .15),
          #panel.background = element_rect(fill = "#FFFFFF"),
          panel.grid.major.y = element_line(color = "#8B8878")) +
    geom_vline(xintercept = '2009', color = "black", linetype = "dotted")
  
  lplot_shocks
}

# Plot Fit-Exposure - fit-tariff x energy use per industry
# Plot normalized GHG development & normalized Environmental regulation tax & FIT-Exposure for 'Total Manuf' <- two y-axis

# Calculate FIT-Exposure
# Sum by fuel type of change in Electricity Generation x FIT in given year
{
  dta_exposure <- dta_MATLAB %>%
    filter(year >= base_year & year <= end_year) %>%
    mutate(across(starts_with('Elect_'), ~.x/Elect_Total, .names="Share{.col}")) %>% # Calc. Electricity Share of each fuel
    select(!c("Elect_Hydro", "Elect_Geothermal", "Elect_Wind","Elect_SolarThermal",
              "Elect_Solar", "Elect_Marine", "Elect_Biomass", "Elect_Waste")) %>%
    mutate(across(starts_with('ShareElect'), ~.x*UseElectricityShare, .names="Tot{.col}"), # multiply Use Share with Fuel Share
           across(starts_with('TotShareElect'), ~.x, .names="Exp{.col}")) %>% # WITHTOUT Multiplication of Total Electricity
                  #~.x*Elect_Total, .names="Exp{.col}")) %>% # Electricity Exposure: multiply exposure in shares with Electricity Production
    
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
  mutate(TotalExposure = (TotalExposure+0.01),#/1000000,
         CostsElectBaseprice = electBaseprice * UseElectricity,
         norm_Exposure = (TotalExposure/TotalExposure[year == base_year])*100,
         norm_ElectPrice = (CostsElectBaseprice/CostsElectBaseprice[year == base_year])*100) %>%
  ungroup()

# Run Fixed_Effects Estimation
{
  # Pooled OLS
  model_OLS <- lm(t_hat_MATLAB ~ norm_Exposure  + norm_ElectPrice , data = testtt)
  summary(model_OLS)
  
  # Fixed Effects with only FIT Exposure and year FE
  model_fe_simple <- felm(t_hat_MATLAB ~ norm_Exposure | # Model Variable
                        year | # Fixed Effects
                        0 | # Instrument
                        NACE_Name, # Variables for Cluster-robust Standard errors
                      data = testtt, cmethod = 'cgm2')
  summary(model_singlefe_simple)
  
  # Fixed Effects with FIT Exposure & Electricity and year FE
  model_singlefe_full <- felm(t_hat_MATLAB ~ norm_Exposure + norm_ElectPrice | # Model Variable
                     year | # Fixed Effects
                     0 | # Instrument
                     NACE_Name, # Variables for Cluster-robust Standard errors
                   data = testtt, cmethod = 'cgm2')
  summary(model_singlefe_full)
  
  # Fixed Effects with FIT Exposure and year and sector FE
  model_singlefe_full <- felm(t_hat_MATLAB ~ norm_Exposure | # Model Variable
                                year + NACE_Name| # Fixed Effects
                                0 | # Instrument
                                NACE_Name, # Variables for Cluster-robust Standard errors
                              data = testtt, cmethod = 'cgm2')
  summary(model_singlefe_full)
  
  # Fixed Effects with FIT Exposure & Electricity and year and sector FE
  model_doublefe_full <- felm(t_hat_MATLAB ~ norm_Exposure + norm_ElectPrice | # Model Variable
                          year + NACE_Name| # Fixed Effects
                          0 | # Instrument
                          NACE_Name, # Variables for Cluster-robust Standard errors
                        data = testtt, cmethod = 'cgm2')
  summary(model_doublefe_full)
  
  # Fixed Effects with FIT Exposure & Electricity and year and sector FE, excl. Coke
  model_doublefe_excl.petrol <- felm(t_hat_MATLAB ~ norm_Exposure + norm_ElectPrice | # Model Variable
                                year + NACE_Name| # Fixed Effects
                                0 | # Instrument
                                NACE_Name, # Variables for Cluster-robust Standard errors
                              data = (testtt %>%
                                        filter(NACE_Name != 'Coke, petroleum')), cmethod = 'cgm2')
  summary(model_doublefe_excl.petrol)
}
  
# Export to LATEX
{
  stargazer(model_OLS, model_singlefe_simple, #model_singlefe_full,
            model_doublefe_full, model_doublefe_excl.petrol, 
            title="Comparison of Model Results", 
            header=FALSE, 
            type="latex", 
            model.numbers=TRUE, 
            column.labels=c("(1)", "(2)", "(3)", "(4)", "(5)"), 
            covariate.labels=c("Exposure", "Electricity Costs"),
            omit.stat=c("LL", "ser", "f"), 
            align=TRUE)
}
stargazer(model_doublefe_excl.petrol, 
          title="Comparison of Model Results", 
          header=FALSE, 
          type="latex", 
          model.numbers=TRUE, 
          column.labels=c("(1)"), 
          covariate.labels=c("Exposure", "Electricity Costs"),
          omit.stat=c("LL", "ser", "f"), 
          align=TRUE)

