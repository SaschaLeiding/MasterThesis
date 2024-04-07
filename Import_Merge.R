"
This Script is to test downloaded Datasets

Attention: Variable 'ghg' & 'base_year' must be the same in all Scripts
"

# Install & Load Packages
{
  #install.packages("tidyverse")
  #install.packages("readxl")
  
  library(tidyverse)
  library(readxl)
}

# Load Data
{
  # Loading MRU1: Air Emission Accounts by industry and type of Emissions
  emissions_01 <- read_xlsx("./Data/DK_Emissions_117grouping_01.xlsx", range = "A3:E2763")
  emissions_02 <- read_xlsx("./Data/DK_Emissions_117grouping_02.xlsx", range = "A3:E2763")
  emissions_03 <- read_xlsx("./Data/DK_Emissions_117grouping_03.xlsx", range = "A3:E2763")
  emissions_04 <- read_xlsx("./Data/DK_Emissions_117grouping_04.xlsx", range = "A3:E2763")
  emissions_05 <- read_xlsx("./Data/DK_Emissions_117grouping_05.xlsx", range = "A3:E2763")
  emissions69_01 <- read_xlsx("./Data/DK_Emissions_69grouping_01.xlsx", range = "A3:H1658")
  emissions69_02 <- read_xlsx("./Data/DK_Emissions_69grouping_02.xlsx", range = "A3:H1658")
  
  # Loading DRIVHUS: Greenhouse Gas Accounts (in CO2 equivalents) by industry, time and type of emission
  emissionsEqui_01 <- read_xlsx("./Data/DK_EmissionsEquivalents_117grouping_01.xlsx", range = "A3:D3963")
  emissionsEqui_02 <- read_xlsx("./Data/DK_EmissionsEquivalents_117grouping_02.xlsx", range = "A3:D3963")
  emissionsEqui_03 <- read_xlsx("./Data/DK_EmissionsEquivalents_117grouping_03.xlsx", range = "A3:D3963")
  emissionsEqui_04 <- read_xlsx("./Data/DK_EmissionsEquivalents_117grouping_04.xlsx", range = "A3:C3963")
  emissionsEqui69_01 <- read_xlsx("./Data/DK_EmissionsEquivalents_69grouping_01.xlsx", range = "A3:G1658")
  emissionsEqui69_02 <- read_xlsx("./Data/DK_EmissionsEquivalents_69grouping_02.xlsx", range = "A3:D1658")
  
  # Loading DRIVHUS2: CO2 in direct and indirect emissions by industry, time and type of emission
  emissionsEqui_05 <- read_xlsx("./Data/DK_CO2_117grouping.xlsx", range = "A3:E2544") # 1,000 tonnes
  
  # Loading NABO: Production and generation of income by price unity, transaction, industry and time
  output_01 <- read_xlsx("./Data/DK_Production_117grouping_01.xlsx", range = "A4:F2384")
  output_02 <- read_xlsx("./Data/DK_Production_117grouping_02.xlsx", range = "A4:E2384")
  output69_01 <- read_xlsx("./Data/DK_Production_69grouping_01.xlsx", range = "A4:F1495")
  output69_02 <- read_xlsx("./Data/DK_Production_69grouping_02.xlsx", range = "A4:E1495")
  
  # Loading Business Demographics Data
  business_01 <- read_xlsx("./Data/DEM01_BusinessDemo_EntryExit_117grouping.xlsx", range = "B3:X261")
  business_02 <- read_xlsx("./Data/GF01_EnterpriseStatistics_117grouping.xlsx", range = "A3:X390")
  
  # Loading Energy Cost data
  cost_ener_01 <- read_xlsx("./Data/DK_CostsEnergy_117grouping_01.xlsx", range = "B3:T1915") # million DKK
  cost_ener_02 <- read_xlsx("./Data/DK_CostsEnergy_117grouping_02.xlsx", range = "B3:T1915") # million DKK
  
  # Loading Energy Use Data
  # Electricity = GWh; Heat = TJ
  ener_use <- read_xlsx("./Data/DK_EnergyUse_117grouping.xlsx", range = "A3:D2363")
  
  # Loading PPI data
  ppi <- read_xlsx("./Data/DK_PPIcommodities_Manufacturing.xlsx", range = "B3:KB46")
  ppi_mapp117 <- read_xlsx("./Data/Mapping_PPI_117grouping.xlsx", range = "B3:G122")
  ppi_mapp69 <- read_xlsx("./Data/Mapping_PPI_69grouping.xlsx", range = "B2:C73")
  
  # Load OECD Exchange rate from 1,000 USD
  OECDFX <- read_xlsx("./Data/OECD_ExchangeRate.xlsx", range = "C3:Z67")
  
  # Load OECD mean Feed-in tariffs and Purchase Power Agreements in USD per kWh
  OECDREFIT <- read_xlsx("./Data/OECD_REFIT.xlsx", range = "A5:J1405") # USD per kWh
  OECDREPPA <- read_xlsx("./Data/OECD_REPPA.xlsx", range = "A5:J1405")
  
  # Load EUROSTAT Electricity Price and Production Data in NATIONAL CURRENCY
  # Prices = national Currency per kWh 
  # Production = GWh
  #EUROSTAT_ElectPrices_kWh_01 <- read_xlsx("./Data/EUROSTAT_ElectricityPrices_Industry_01.xlsx",
  #                                     range = "A11:V101", sheet = 'Sheet 1')
  #EUROSTAT_ElectPrices_kWh_02 <- read_xlsx("./Data/EUROSTAT_ElectricityPrices_Industry_02.xlsx",
  #                                     range = "A11:BP134", sheet = 'Sheet 1')
  EUROSTAT_Production_Electricity <- read_xlsx("./Data/EUROSTAT_Production_Electricity.xlsx",
                                               range = "A9:V829", sheet = 'Sheet 1')
  EUROSTAT_Production_Heat <- read_xlsx("./Data/EUROSTAT_Production_Heat.xlsx",
                                               range = "A9:V829", sheet = 'Sheet 1')
  EUROSTAT_ElectPrices_DNK_kWh_01 <- read_xlsx("./Data/EUROSTAT_ElectricityPrices_DNK_01.xlsx",
                                           range = "A11:L38", sheet = 'Sheet 1')
  EUROSTAT_ElectPrices_DNK_kWh_02 <- read_xlsx("./Data/EUROSTAT_ElectricityPrices_DNK_02.xlsx",
                                           range = "A11:AJ32", sheet = 'Sheet 1')
}

# Define variables for code flexibility
{
  ghg <-'CO2ElectricityHeat'  #'CO2Total' # Greenhouse gas for the analysis to allow flexibility in choice
  varname_ghgintensity <- paste0(ghg, "_intensity")
  base_year <- 2003
}

# Transform Air Emissions Accounts for Merging and Create unique Air Emissions Dataset
{
  # Transform Emissions Data
  {
    emissions_datalist <- list(emissions_01 = emissions_01,
                               emissions_02 = emissions_02,
                               emissions_03 = emissions_03,
                               emissions_04 = emissions_04,
                               emissions_05 = emissions_05,
                               emissions69_01 = emissions69_01,
                               emissions69_02 = emissions69_02)
    
    for (dataset_name in names(emissions_datalist)) {
      # Rename Columns 1 & 2 with to indicate classification 'classif' and 'year'
      colnames(emissions_datalist[[dataset_name]])[1] <- "classif"
      colnames(emissions_datalist[[dataset_name]])[2] <- "year"
      
      # Fill up NAs of Column 1 by respective classification
      emissions_datalist[[dataset_name]] <- emissions_datalist[[dataset_name]] %>% fill(classif) %>%
        mutate(across(3:ncol(emissions_datalist[[dataset_name]]), .fns=as.numeric))
    }
  }
  
  # Create unique Emissions Dataset
  {
    emissions <- rbind((emissions_datalist[["emissions_01"]] %>% #Base Dataset
      left_join(emissions_datalist[["emissions_02"]], join_by(classif == classif, year == year)) %>% # Join 117grouping Data
      left_join(emissions_datalist[["emissions_03"]], join_by(classif == classif, year == year)) %>% # Join 117grouping Data
      left_join(emissions_datalist[["emissions_04"]], join_by(classif == classif, year == year)) %>% # Join 117grouping Data
        mutate(grouping = 117)), # Create Grouping variable
      # Add 69-grouping Data
      (emissions_datalist[["emissions69_01"]]%>%
         filter(classif != 'Total' & classif != 'Households' & classif != 'Total industries')) %>%
        left_join((emissions_datalist[["emissions69_02"]])%>%
                    filter(classif != 'Total' & classif != 'Households' & classif != 'Total industries'),
                  join_by(classif == classif, year == year)) %>%
        mutate(grouping = 69)) %>% # Create Grouping variable
      left_join(emissions_datalist[["emissions_05"]], join_by(classif == classif, year == year))# Join 117grouping Data of fluorinated gases
  }
  
  # Remove unnecessary Data
  {
    rm(emissions_datalist)
    rm(emissions_01)
    rm(emissions_02)
    rm(emissions_03)
    rm(emissions_04)
    rm(emissions_05)
    rm(emissions69_01)
    rm(emissions69_02)
    rm(dataset_name)
  }
  
  # Rename Columns and Subheaders of Columns
  {
    # Create a list with new Variable Names
    emissions_col_names <- c("classif", "year", "CO2inclBiomass", "CO2exclBiomass", "CO2fromBiomass", "SO2", "NOx",
                             "CO", "NH3", "N2O", "CH4", "NMVOC", "PM10", "PM2.5", "group", "SF6", "PFC", "HFC")
    # Create list with attributes
    emissions_col_attr <-  colnames(emissions)
    
    # Rename them
    for(i in 1:ncol(emissions)){
      attr(emissions[[i]], 'label') <- emissions_col_attr[i]
      colnames(emissions)[i] <- emissions_col_names[i]
    }
  }
}

# Transform Greenhouse Gas Accounts for Merging and Create unique GHG Dataset
{
  # Transform Emissions Data
  {
    emissions_datalist <- list(emissionsEqui_01 = emissionsEqui_01,
                               emissionsEqui_02 = emissionsEqui_02,
                               emissionsEqui_03 = emissionsEqui_03,
                               emissionsEqui_04 = emissionsEqui_04,
                               emissionsEqui_05 = emissionsEqui_05,
                               emissionsEqui69_01 = emissionsEqui69_01,
                               emissionsEqui69_02 = emissionsEqui69_02)
    
    for (dataset_name in names(emissions_datalist)) {
      # Rename Columns 1 & 2 with to indicate classification 'classif' and 'year'
      colnames(emissions_datalist[[dataset_name]])[1] <- "classif"
      colnames(emissions_datalist[[dataset_name]])[2] <- "year"
      
      # Fill and Transform Data
      emissions_datalist[[dataset_name]] <- emissions_datalist[[dataset_name]] %>% 
        fill(classif) %>% # Fill Up NAs by respective classification
        mutate(across(-(1:2), .fns=as.numeric)) # Transform Columns except 1 and 2 into numeric
    }
  }

  # Create unique Emissions Dataset
  {
    emissionsEqui <- rbind((emissions_datalist[["emissionsEqui_01"]] %>% 
      left_join(emissions_datalist[["emissionsEqui_02"]], join_by(classif == classif, year == year)) %>%
      left_join(emissions_datalist[["emissionsEqui_03"]], join_by(classif == classif, year == year)) %>%
      left_join(emissions_datalist[["emissionsEqui_04"]], join_by(classif == classif, year == year))),
      (emissions_datalist[["emissionsEqui69_01"]] %>%
      left_join(emissions_datalist[["emissionsEqui69_02"]], join_by(classif == classif, year == year))))%>%
      left_join(emissions_datalist[["emissionsEqui_05"]], join_by(classif == classif, year == year))
  }
  
  # Remove unnecessary Data
  {
    rm(emissionsEqui_01)
    rm(emissionsEqui_02)
    rm(emissionsEqui_03)
    rm(emissionsEqui_04)
    rm(emissionsEqui_05)
    rm(emissionsEqui69_01)
    rm(emissionsEqui69_02)
    rm(dataset_name)
    rm(emissions_datalist)
  }
  
  # Rename Columns and Subheaders of Columns
  {
    # Create a list with new Variable Names
    emissions_col_names <- c("classif", "year", "GHGexclBiomass", "GHGinclBiomass", 
                             "CO2exclBiomass", "CO2fromBiomass", "N2O_Equi", "CH4_Equi", "Fgases_Equi",
                             "C02Direct", "CO2ElectricityHeat", "CO2Total")
    # Create list with attributes
    emissions_col_attr <-  colnames(emissionsEqui)
    
    # Rename them
    for(i in 1:ncol(emissionsEqui)){
      attr(emissionsEqui[[i]], 'label') <- emissions_col_attr[i]
      colnames(emissionsEqui)[i] <- emissions_col_names[i]
    }
  }
  
  # Drop Columns 'CO2exclBiomass' and 'CO2fromBiomass' as already included in Air Emissions Accounts to Data
  {
    emissionsEqui <- emissionsEqui %>% select(-c('CO2exclBiomass', 'CO2fromBiomass'))
  }
}

# Transform Output Data to be able to merge with emissions Data in longitudinal-format
{
  output <- rbind((output_01 %>% fill(...1) %>% # Fill NAs in column transaction with above classification
    full_join((output_02 %>% fill(...1)))),# Join Data
    (output69_01 %>% fill(...1) %>%
       left_join(output69_02 %>% fill(...1), join_by(...1 == ...1, ...2 == ...2))))
  
  # Rename Columns for Output
  {
    colnames(output)[1] <- 'classif'
    colnames(output)[2] <- 'year'
    colnames(output)[3] <- 'voutput'
    colnames(output)[4] <- 'interConsumption'
    colnames(output)[5] <- 'grossVA'
    colnames(output)[6] <- 'TaxSubsidies'
    colnames(output)[7] <- 'GDPatFactorCosts'
    colnames(output)[8] <- 'CompEmployees'
    colnames(output)[9] <- 'GrossOpSurplus'
    }
  
  # Attribute label to columns
  {
    for(i in 3:ncol(output)){
      attr(output[[i]], 'label') <- "m DKK"
    }
  }
  
  # Remove unneccessary data
  {
    rm(output_01)
    rm(output_02)
    rm(output69_01)
    rm(output69_02)
  }
}

# Transform Business Data
{
  business <- business_01 %>% fill(...1) %>%
    full_join((business_02 %>% fill(...1) %>% mutate(across(3:24, .fns=as.numeric)))) %>%
    pivot_longer(cols = starts_with("20"),
                 names_to = 'year',
                 values_to = '.value') %>%
    pivot_wider(names_from = ...1,
                values_from = .value)
  
  colnames(business)[1] <- 'classif'
  colnames(business)[3] <- 'firmExit'
  colnames(business)[4] <- 'firmEntry'
  colnames(business)[5] <- 'firms'
  colnames(business)[6] <- 'Employees'
  colnames(business)[7] <- 'Turnover'
  
  rm(business_01)
  rm(business_02)
}

# Transform PPI Data to be able to merge with emissions Data in longitudinal-format
{
  colnames(ppi)[1] <- 'classif' # Add column name for first column
  
  ppi_trans <- ppi %>% select(classif, ends_with("12")) %>% # Drop all columns that contain the values of the month January-November
    mutate(across(2:24, .fns=as.numeric)) %>% # Transform columns into numerics
    pivot_longer(cols = 2:24, names_to = "year", values_to = "PPI") %>% # Transform data into longitudinal format
    mutate(year = substring(year, 1, 4)) %>% # Drop month ('December') indicator
    group_by(classif) %>%
    mutate(PPI = (PPI/PPI[year == base_year])*100) %>% # Rescale PPI with Base year 2000
    ungroup()
  
  ppi_mapp <- ppi_mapp117 %>%
    full_join(ppi_mapp69) %>%
    distinct(Class_output, .keep_all=TRUE)
  
  # Remove unnecessary data
  rm(ppi_mapp117)
  rm(ppi_mapp69)
  rm(ppi)
}

# Transform Energy Cost and Use Data
{
  colnames(cost_ener_01)[1] <- 'costtype'
  colnames(cost_ener_01)[2] <- 'classif'
  colnames(cost_ener_02)[1] <- 'costtype'
  colnames(cost_ener_02)[2] <- 'classif'
  colnames(ener_use)[1] <- 'classif'
  colnames(ener_use)[2] <- 'year'
  
  cost_ener_trans <- cost_ener_01 %>%
    fill(costtype) %>%
    pivot_longer(cols = starts_with("20"),
                 names_to = 'year',
                 values_to = '.value') %>%
    filter(costtype == 'Energy expense at purchacers prices (7=1+ ... +6)') %>%
    pivot_wider(names_from = costtype,
                values_from = .value) %>%
    rename(costEnergy = 'Energy expense at purchacers prices (7=1+ ... +6)') %>%
    
    left_join((cost_ener_02 %>%
                 fill(costtype) %>%
                 pivot_longer(cols = starts_with("20"),
                              names_to = 'year',
                              values_to = '.value') %>%
                 filter(costtype == 'Energy expense at purchacers prices (7=1+ ... +6)') %>%
                 pivot_wider(names_from = costtype,
                             values_from = .value) %>%
                 rename(costEnergyElectricityHeat = 'Energy expense at purchacers prices (7=1+ ... +6)')),
              join_by(classif == classif, year == year)) %>%
    
    left_join((ener_use %>%
                 fill(classif) %>%
                 rename(UseElectricityGWh = 'Electricity (GWh)',
                        UseHeatTJ = 'District heat (TJ)') %>%
                 mutate(UseElectricity = UseElectricityGWh*1000000,
                        UseHeat = UseHeatTJ * 277777.778) %>%
                 select(!(c('UseElectricityGWh', 'UseHeatTJ')))),
              join_by(classif == classif, year == year)) %>%
    
    left_join((ppi_trans %>%
                 ungroup() %>%
                 filter(classif == "Energy (MIG)") %>%
                 select(year, PPI)),
              join_by(year == year)) %>%
    mutate(realcostEnergy = round((costEnergy / (PPI/100)), digits = 2),
           realcostEnergyElectricityHeat = round((costEnergyElectricityHeat / (PPI/100)), digits = 2))
  
  rm(cost_ener_01)
  rm(cost_ener_02)
}

# Transform Renewable Energy FIT and PPA length data (OECD)
{
  dta_FITPPA <- cbind((OECDREFIT %>%
    fill(Country) %>%
    select(!'...2') %>%
    rename(country = Country,
           year = Year,
           Solar = 'Solar PV',
           Hydro = 'Small Hydro') %>%
    mutate(across(Solar:Marine, as.numeric, .names="FIT{.col}")) %>%
    select(country, year, starts_with("FIT"))),
    (OECDREPPA %>%
                 fill(Country) %>%
                 select(!'...2') %>%
                 rename(country = Country,
                        year = Year,
                        Solar = 'Solar PV',
                        Hydro = 'Small Hydro') %>%
                 mutate(across(Solar:Marine, as.numeric, .names="PPA{.col}")) %>%
                 select(starts_with("PPA"))))
  
  rm(OECDREFIT)
  rm(OECDREPPA)
}

# Transform Electricity Price and Production Data (EUROSTAT)
{
  # Create Electricity Price Data and Plot for Justification
  {
    # Create List with Consumption groups
    {
      Elect_consGroup_until_prior <- c("Industry - Ia (Annual consumption: 30 MWh; maximum demand: 30 kW; annual load: 1 000 hours) (for Luxembourg: 50% power reduction during hours of heavy loading)",
                                       "Industry - Ib (Annual consumption: 50 MWh; maximum demand: 50 kW; annual load: 1 000 hours) (for Luxembourg: 50% power reduction during hours of heavy loading)",
                                       "Industry - Ic (Annual consumption: 160 MWh; maximum demand: 100 kW; annual load: 1 600 hours) (for Luxembourg: 50% power reduction during hours of heavy loading)",
                                       "Industry - Id (Annual consumption: 1 250 MWh; maximum demand: 500 kW; annual load: 2 500 hours) (for Luxembourg: 50% power reduction during hours of heavy loading)",
                                       "Industry - Ie (Annual consumption: 2 000 MWh; maximum demand: 500 kW; annual load: 4 000 hours) (for Luxembourg: 50% power reduction during hours of heavy loading)",
                                       "Industry - If (Annual consumption: 10 000 MWh; maximum demand: 2 500 kW; annual load: 4 000 hours) (for Luxembourg: 50% power reduction during hours of heavy loading)",
                                       "Industry - Ig (Annual consumption: 24 000 MWh; maximum demand: 4 000 kW; annual load: 6 000 hours) (for Luxembourg: 50% power reduction during hours of heavy loading)",
                                       "Industry - Ih (Annual consumption: 50 000 MWh; maximum demand: 10 000 kW; annual load: 5 000 hours) (for Luxembourg: 50% power reduction during hours of heavy loading)",
                                       "Industry - Ii (Annual consumption: 70 000 MWh; maximum demand: 10 000 kW; annual load: 7 000 hours) (for Luxembourg: 50% power reduction during hours of heavy loading)"
      )
      
      Elect_consGroup_until_post <- c("20MWh",
                                      "499MWh",
                                      "499MWh",
                                      "1999MWh",
                                      "1999MWh",
                                      "19999MWh",
                                      "69999MWh",
                                      "69999MWh",
                                      ">70000MWh")
      Elect_consGroup_until <- cbind(Elect_consGroup_until_prior, Elect_consGroup_until_post)
      rm(Elect_consGroup_until_prior)
      rm(Elect_consGroup_until_post)
      
      Elect_consGroup_since_prior <- c("Consumption 150 000 MWh or over - band IG",
                                       "Consumption from 2 000 MWh to 19 999 MWh - band ID",
                                       "Consumption from 20 000 MWh to 69 999 MWh - band IE",
                                       "Consumption from 20 MWh to 499 MWh - band IB",
                                       "Consumption from 500 MWh to 1 999 MWh - band IC",
                                       "Consumption from 70 000 MWh to 149 999 MWh - band IF",
                                       "Consumption less than 20 MWh - band IA")
      Elect_consGroup_since_post <- c(">70000MWh",
                                      "19999MWh",
                                      "69999MWh",
                                      "499MWh",
                                      "1999MWh",
                                      ">70000MWh",
                                      "20MWh")
      Elect_consGroup_since <- cbind(Elect_consGroup_since_prior, Elect_consGroup_since_post)
      rm(Elect_consGroup_since_prior)
      rm(Elect_consGroup_since_post)
    }
    
    # Transform Electricity Price dataset tot be able to merge with each other
    {
      Electricity_Price_01 <- EUROSTAT_ElectPrices_DNK_kWh_01 %>%
        mutate(across(3:ncol(.), as.numeric)) %>%
        rename(price_type = `TAX (Labels)`) %>%
        left_join(as.data.frame(Elect_consGroup_until), join_by(`CONSOM (Labels)`==Elect_consGroup_until_prior)) %>%
        # Fill missing values for groups with assignment large than 1,999MWh
        group_by(price_type) %>%
        mutate(across(.cols = where(is.numeric), ~ ifelse(is.na(.x), .x[Elect_consGroup_until_post == '1999MWh'][1], .x))) %>%
        # Summarise to fit second dataset classification
        group_by(price_type, Elect_consGroup_until_post) %>%
        summarise(across(.cols = where(is.numeric), .fns = mean, na.rm = TRUE), .groups = 'drop') %>% 
        # Transform to Longitudinal data and average price per year
        group_by(price_type) %>%
        pivot_longer(cols = 3:ncol(.), names_to = 'year', values_to = '.value') %>%
        mutate(half = str_sub(year, -2),
               year = str_sub(year, 1, 4)) %>%
        filter(year != '2007') %>%
        group_by(price_type, Elect_consGroup_until_post, year) %>%
        summarise(avgValue = mean(.value)) %>%
        # Transform to longitudinal Data but 3 variables as columns
        pivot_wider(names_from = price_type, values_from =avgValue) %>%
        # Rename Columns
        rename(consGroups = Elect_consGroup_until_post,
               electBaseprice = 'Excluding taxes and levies',
               electVATfreeprice = 'Excluding VAT and other recoverable taxes and levies',
               electFinalprice = 'All taxes and levies included') %>%
        ungroup()
      
      Electricity_Price_02 <- EUROSTAT_ElectPrices_DNK_kWh_02 %>%
        mutate(across(3:ncol(.), as.numeric)) %>%
        rename(price_type = `TAX (Labels)`) %>%
        left_join(as.data.frame(Elect_consGroup_since), join_by(`NRG_CONS (Labels)`==Elect_consGroup_since_prior)) %>%
        select(-'2007-S1') %>%
        # Summarise to fit second dataset classification
        group_by(price_type, Elect_consGroup_since_post) %>%
        summarise(across(.cols = where(is.numeric), .fns = mean, na.rm = TRUE), .groups = 'drop') %>% 
        # Transform to Longitudinal data and average price per year
        group_by(price_type) %>%
        pivot_longer(cols = 3:ncol(.), names_to = 'year', values_to = '.value') %>%
        mutate(half = str_sub(year, -2),
               year = str_sub(year, 1, 4)) %>%
        group_by(price_type, Elect_consGroup_since_post, year) %>%
        summarise(avgValue = mean(.value)) %>%
        # Transform to longitudinal Data but 3 variables as columns
        pivot_wider(names_from = price_type, values_from =avgValue) %>%
        # Rename Columns
        rename(consGroups = Elect_consGroup_since_post,
               electBaseprice = 'Excluding taxes and levies',
               electVATfreeprice = 'Excluding VAT and other recoverable taxes and levies',
               electFinalprice = 'All taxes and levies included') %>%
        ungroup()
    }
    
    Electricity_Price <- rbind(Electricity_Price_01, Electricity_Price_02) %>%
      pivot_longer(cols = is.numeric,names_to = 'price_type', values_to = '.value')
    
    #Plot Electricity Base Prices by group - Landscape 8,00x6,00
    {
      # Create Dataframe for plot
      dta_lplot_ElectPrice <- Electricity_Price %>%
        filter(price_type == 'electBaseprice') %>%
        filter( year <= '2014') %>%
        group_by(consGroups) %>%
        mutate(normPrice = (.value/.value[year == base_year])*100)
      
      lplot_ElectPrice <- ggplot(data = dta_lplot_ElectPrice,
                                 aes(x = year, y = normPrice, color = consGroups, group = consGroups)) +
        geom_line(aes(size = consGroups)) +  # Add size mapping here
        geom_vline(xintercept = 2009, color = "black", linetype = "dotted") +
        scale_colour_manual(values = c("black", "green", "blue", "red", "orange", "violet"),
                            labels = c("> 70,000 MWh", "<19,999 MWh", "<1,999 MWh",
                                       "<20 MWh", "499 MWh", "<69,999 MWh")) +
        scale_size_manual(values = c('>70000MWh' = 1, '19999MWh' = 0.5, '1999MWh' = 0.5,
                                     '20MWh' = 0.5, '499MWh' = 0.5, '69999MWh' = 0.5),
                          guide = FALSE) +
        labs(x = "Year",
             y = paste0("Base ", base_year, " = 100"),
             color = NULL) +  # Hide the size legend
        theme_classic() +
        theme(legend.position = c(.15, .78),
              panel.grid.major.y = element_line(color = "#8B8878"))
      lplot_ElectPrice
      }
  }
  
  # Create Electricity and Heat Production Data
  {
    ElectricityHeat_Production <- EUROSTAT_Production_Electricity %>%
      select(!starts_with("...")) %>%
      select(!('Ambient heat (heat pumps)')) %>%
      rename(country = 'GEO (Labels)',
             year = TIME,
             SolarThermal = 'Solar thermal',
             Solar = 'Solar photovoltaic',
             Marine = 'Tide, wave, ocean',
             Biomass = Biogases,
             Waste = 'Renewable municipal waste') %>%
      mutate(across(3:ncol(.), as.numeric, .names="Elect_{.col}")) %>%
      left_join((EUROSTAT_Production_Heat %>%
                   select(!starts_with("...")) %>%
                   select(!('Ambient heat (heat pumps)')) %>%
                   rename(country = 'GEO (Labels)',
                          year = TIME,
                          SolarThermal = 'Solar thermal',
                          Solar = 'Solar photovoltaic',
                          Marine = 'Tide, wave, ocean',
                          Biomass = Biogases,
                          Waste = 'Renewable municipal waste') %>%
                   mutate(across(3:ncol(.), as.numeric, .names="Heat_{.col}"))),
                join_by(country == country, year == year)) %>%
      select(country, year, starts_with("Elect_"), starts_with("Heat_")) %>%
      mutate(across(is.numeric, ~.x*1000000),
             RE_prod = Elect_Hydro + Elect_Geothermal + Elect_Wind + Elect_SolarThermal +
               Elect_Solar + Elect_Marine + Elect_Biomass + Elect_Waste,
             RE_share = RE_prod/Elect_Total)
  }
  
  # Merge two Dataset
  dta_electricityheat <- ElectricityHeat_Production %>%
    left_join(((Electricity_Price %>%
                  filter(consGroups == ">70000MWh") %>%
                  pivot_wider(names_from=price_type, values_from = '.value') %>%
                  select(-consGroups) %>%
                  left_join(Electricity_Price %>%
                              filter(consGroups == "20MWh") %>%
                              pivot_wider(names_from=price_type, names_prefix = '20MWh',
                                          values_from = '.value') %>%
                              select(-consGroups),
                            join_by(year == year))) %>%
                 mutate(country = "Denmark")),
              join_by(country == country, year == year))
  
  # Remove unnecessary Data
  {
    rm(EUROSTAT_ElectPrices_DNK_kWh_01)
    rm(EUROSTAT_ElectPrices_DNK_kWh_02)
    rm(Elect_consGroup_since)
    rm(Elect_consGroup_until)
    rm(Electricity_Price_01)
    rm(Electricity_Price_02)
    rm(EUROSTAT_Production_Electricity)
    rm(EUROSTAT_Production_Heat)
    rm(Electricity_Price)
    rm(ElectricityHeat_Production)
  }
}

# Calculate Total of Manufacturing #ENERGY USE appended through 'cost_ener_use', thus Statbank data
{
  dta_total <- emissions %>% # Take 'Air Emission Accounts' as Base data
    left_join((emissionsEqui) %>% distinct(), # Note: 'emissionsEqui' contains duplicates
              join_by(classif == classif, year == year)) %>% # Join 'GHG in CO2-equivalents Data
    left_join(output, join_by(classif == classif, year == year)) %>%
    left_join(business, join_by(classif == classif, year == year)) %>%
    left_join((cost_ener_trans %>% select(!PPI)), join_by(classif == classif, year == year)) %>%
    left_join(ppi_mapp, join_by(classif == Class_output)) %>%
    
    filter(!is.na(NACE_Code)) %>%
    group_by(year) %>%
    summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE), .groups = 'drop') %>%
    mutate(classif = 'Total Manufacturing',
           ISIC_Name = 'Total Manufacturing',
           Class_PPI = 'C Manufacturing',
           NACE_Name = 'Total Manufacturing') %>%
    
    left_join(ppi_trans %>% 
                filter(classif == 'C Manufacturing') %>%
                select(!classif), 
              join_by(year == year))
}

# Merge Emissions, Output and PPI data
{
  dta_decomp <- emissions %>% # Take 'Air Emission Accounts' as Base data
    left_join((emissionsEqui) %>% distinct(), # Note: 'emissionsEqui' contains duplicates
              join_by(classif == classif, year == year)) %>% # Join 'GHG in CO2-equivalents Data
    left_join(output, join_by(classif == classif, year == year)) %>% # Join Output data
    left_join(business, join_by(classif == classif, year == year)) %>% # Join Business data
    left_join(ppi_mapp, join_by(classif == Class_output)) %>% # Join Mapping for PPI to 117 grouping
    left_join(ppi_trans, join_by(Class_PPI == classif, year == year)) %>% # Join PPI values by previously inserted Mapping
    left_join((cost_ener_trans %>% select(!PPI)), 
              join_by(classif == classif, year == year)) %>%
    full_join(dta_total) %>%
    filter(!(year %in% c(2020, 2021,2022))) %>% # Disselect the entries of the years 2020-2022
    distinct()
  # Note: Not dropping NA because all 69 grouping does not have values for individual Fluorinated gases but its sum in CO2-equivalents
  
  rm(emissions)
  rm(emissionsEqui)
  rm(output)
  rm(business)
  rm(ppi_trans)
  rm(cost_ener_trans)
}

# Calculate Variables: Emission Intensity, real Output and real Output Intensity
{
  dta_decomp <- dta_decomp %>% 
    group_by(classif) %>% 
    arrange(year, .by_group = TRUE) %>%
    mutate(!!varname_ghgintensity := !!sym(ghg)/(voutput/1000),
           realoutput = voutput / (PPI/100),
           expend = interConsumption + CompEmployees,
           realexpend = expend / (PPI/100),
           realouput_intensity = (realoutput * (!!sym(varname_ghgintensity))),
           wage = CompEmployees/Employees,
           ElectbyFirm = UseElectricity/firms)
  
  attr(dta_decomp$realoutput, 'label') <- 'm DKK'
  attr(dta_decomp$expend, 'label') <- 'm DKK'
  attr(dta_decomp$realexpend, 'label') <- 'm DKK'
  attr(dta_decomp$realouput_intensity, 'label') <- '1,000 DKK per ton'
}

# Create NACE-classification data
{
  dta_NACEsum <- dta_decomp %>%
    filter(!is.na(NACE_Code)) %>%
    group_by(year, NACE_Code) %>%
    filter(n()>1) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop') %>%
    ungroup() %>%
    mutate(!!varname_ghgintensity := !!sym(ghg)/(voutput/1000),
           realouput_intensity = (realoutput * (!!sym(varname_ghgintensity)))) %>%
    left_join(unique(ppi_mapp %>% select(NACE_Code, NACE_Name)), join_by(NACE_Code == NACE_Code))
  
  NACEcode_sum <- unique(dta_NACEsum$NACE_Code)
  
  dta_NACE <- dta_NACEsum %>%
    full_join(dta_decomp %>%
                filter(!is.na(NACE_Code) & !(NACE_Code %in% NACEcode_sum))) %>%
    mutate(ISIC_Code = NA,
           ISIC_Name = NA,
           classsystem = 'NACE') %>%
    group_by(year) %>%
    mutate(output_share = realoutput/(sum(realoutput)/2), # divide by 2 because the sum includes each indiv. and the total together
           wage_manuf = CompEmployees[NACE_Name == 'Total Manufacturing'] / Employees[NACE_Name ==  'Total Manufacturing'],
           UseElectricityShare = UseElectricity/(sum(UseElectricity)/2))
}

# EUROSTAT Country Name & Code
{
  country_EUROSTAT <- c("Albania", "Argentina", "Australia", "Austria", "Belgium",
                        "Brazil", "Bulgaria", "Cameroon", "Canada", "Chile", 
                        "China (People's Republic of)", "Colombia", "Costa Rica", 
                        "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", 
                        "Euro area (19 countries)", "European Union (27 countries, 2020)", 
                        "Finland", "France", "Georgia", "Germany", "Greece", 
                        "Hong Kong, China", "Hungary", "Iceland", "India", "Indonesia", 
                        "Ireland", "Israel", "Italy", "Japan", "Korea", "Latvia", 
                        "Lithuania", "Luxembourg", "Madagascar", "Malta", "Mexico", 
                        "Morocco", "Netherlands", "New Zealand", "North Macedonia", 
                        "Norway", "Poland", "Portugal", "Romania", "Russia", 
                        "Saudi Arabia", "Senegal", "Serbia", "Singapore", "Slovak Republic", 
                        "Slovenia", "South Africa", "Spain", "Sweden", "Switzerland", 
                        "Türkiye", "United Kingdom", "United States", "Zambia")
    countrycode_EUROSTAT <- c(08, 032, 036, 040, 056, 076, 100, 120, 124, 152, 156, 170, 
                              188, 191, 196, 203, 208, 233, NA, NA, 246, 250, 268, 
                              276, 300, 344, 348, 352, 356, 360, 372, 376, 380, 392, 
                              410, 428, 440, 442, 450, 470, 484, 504, 528, 554, 807, 
                              578, 616, 620, 642, 643, 682, 686, 688, 702, 703, 705, 
                              710, 724, 752, 756, 792, 826, 840, 894)
}

# WIODs
{
  # Denmark Input Output table
  {
    WIOD_DNK <- read_xlsx("./Data/WIOD_DNK.xlsx", range = "A1:BO1802", sheet = "National IO-tables")
    
    WIOD_DNK_inter <- WIOD_DNK[-1,] %>%
      mutate(across(5:ncol(WIOD_DNK), as.numeric))%>%
      filter(str_detect(Code, "^C")) %>%
      mutate(DomDom = GO-EXP,
             DomImp = ifelse(Origin == 'Imports', rowSums(select(., 5:ncol(WIOD_DNK))), NA)) %>%
      select(Code, Description, Origin, Year, GO, EXP, DomDom, DomImp)
    
    WIOD_DNK_final <- WIOD_DNK_inter %>%
      filter(Origin == "Domestic") %>%
      select(Code, Description, Year, GO, EXP, DomDom) %>%
      
      left_join((WIOD_DNK_inter %>%
                   filter(Origin == "Imports") %>%
                   select(Code, Description, Year, DomImp)),
                join_by(Code == Code, Description == Description, Year == Year))
  }
  
  # World Input-Output table
  {
    load("./Data/WIOT2003_October16_ROW.RData")
    WIOT_2003 <- wiot
    load("./Data/WIOT2004_October16_ROW.RData")
    WIOT_2004 <- wiot
    load("./Data/WIOT2005_October16_ROW.RData")
    WIOT_2005 <- wiot
    load("./Data/WIOT2006_October16_ROW.RData")
    WIOT_2006 <- wiot
    load("./Data/WIOT2007_October16_ROW.RData")
    WIOT_2007 <- wiot
    load("./Data/WIOT2008_October16_ROW.RData")
    WIOT_2008 <- wiot
    load("./Data/WIOT2009_October16_ROW.RData")
    WIOT_2009 <- wiot
    load("./Data/WIOT2010_October16_ROW.RData")
    WIOT_2010 <- wiot
    load("./Data/WIOT2011_October16_ROW.RData")
    WIOT_2011 <- wiot
    load("./Data/WIOT2012_October16_ROW.RData")
    WIOT_2012 <- wiot
    load("./Data/WIOT2013_October16_ROW.RData")
    WIOT_2013 <- wiot
    load("./Data/WIOT2014_October16_ROW.RData")
    WIOT_2014 <- wiot
    
    WIOD_World <- rbind(WIOT_2003, WIOT_2004, WIOT_2005, WIOT_2006, WIOT_2007, WIOT_2008,
                        WIOT_2009, WIOT_2010, WIOT_2011, WIOT_2012, WIOT_2013, WIOT_2014)
    
    WIOD_World_final <- WIOD_World %>%
      filter(Country != "DNK") %>%
      select(!starts_with("DNK"), -TOT) %>%
      mutate(total = rowSums(select(., 6:ncol(.)))) %>%
      select(IndustryCode, IndustryDescription, Country, RNr, Year, total) %>%
      group_by(IndustryCode, IndustryDescription, Year) %>%
      summarise(ROWROW = sum(total, na.rm = TRUE), .groups = "drop") %>%
      filter(str_detect(IndustryCode, "^C")) %>%
      ungroup()
    
    # Remove unnecessary data
    {
      rm(WIOT_2003)
      rm(WIOT_2004)
      rm(WIOT_2005)
      rm(WIOT_2006)
      rm(WIOT_2007)
      rm(WIOT_2008)
      rm(WIOT_2009)
      rm(WIOT_2010)
      rm(WIOT_2011)
      rm(WIOT_2012)
      rm(WIOT_2013)
      rm(WIOT_2014)
      rm(WIOD_World)
      }
  }
  
  # Merge Denmark and World input-output table and create total manufacturing
  {
    WIOD <- WIOD_DNK_final %>%
      left_join((WIOD_World_final %>% select(!IndustryDescription)),
                join_by(Code == IndustryCode, Year == Year)) %>%
      filter(Year >= "2003") %>%
      rename(year = Year) %>%
      mutate(OutputWorld = NA)
    
    WIOD_total <- WIOD %>%
      group_by(year) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop') %>%
      mutate(Description = "Total Manufacturing",
             Code = "C",
             OutputWorld = EXP + DomDom + DomImp + ROWROW)
    
    # Create Frame for NACE identification
    {
      WIOD_codes <- sort(c("C10-C12", "C13-C15", "C16", "C17", "C18", "C19", "C20",
                           "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", 
                           "C29", "C30", "C31_C32", "C33", "C"))
      WIOD_NACE <- c("Total Manufacturing", "Food, beverages, tobacco",
                     "Textiles, wearing apparel, leather", "Wood products",
                     "Pulp, paper, publishing", "Pulp, paper, publishing",
                     "Coke, petroleum", "Chemicals and pharmaceuticals",
                     "Chemicals and pharmaceuticals", "Rubber and plastics",
                     "Non-metallic minerals", "Basic Metals", "Metal products, electronics, machinery",
                     "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                     "Metal products, electronics, machinery", "Vehicles, other transport, n.e.c.",
                     "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.",
                     "Vehicles, other transport, n.e.c.")
      WIOD_merge <- as.data.frame(cbind(WIOD_codes, WIOD_NACE))
      }
    
    WIOD_final <- rbind(WIOD, WIOD_total) %>%
      left_join(WIOD_merge, join_by(Code == WIOD_codes)) %>%
      group_by(year, WIOD_NACE) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop') %>%
      mutate(year = as.character(year)) %>%
      
      # ADD FX rates
      left_join((OECDFX %>%
                   filter(Location == "Denmark") %>%
                   pivot_longer(cols = starts_with("20"),
                                names_to = 'year',
                                values_to = 'FXrateUSD') %>%
                   mutate(FXrateUSD = as.numeric(FXrateUSD)/1000) %>%
                   pivot_wider(names_from = Location,
                               values_from = FXrateUSD) %>%
                   rename(DKKUSD = Denmark)),
                join_by(year == year)) %>%
      mutate(across(where(is.numeric), ~.x*DKKUSD)) %>%
      group_by(year) %>%
      mutate(OutputWorld = OutputWorld[WIOD_NACE == 'Total Manufacturing'],
             across(3:7, ~.x/OutputWorld),
             vship = GO/GO[WIOD_NACE == 'Total Manufacturing']) %>%
      select(-GO, -OutputWorld,-DKKUSD)
    
    # Create WIOD without Coke
    {
      WIOD_inter_exCoke <- rbind(WIOD, WIOD_total) %>%
        left_join(WIOD_merge, join_by(Code == WIOD_codes)) %>%
        group_by(year, WIOD_NACE) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop') %>%
        mutate(year = as.character(year)) %>%
        
        # ADD FX rates
        left_join((OECDFX %>%
                     filter(Location == "Denmark") %>%
                     pivot_longer(cols = starts_with("20"),
                                  names_to = 'year',
                                  values_to = 'FXrateUSD') %>%
                     mutate(FXrateUSD = as.numeric(FXrateUSD)/1000) %>%
                     pivot_wider(names_from = Location,
                                 values_from = FXrateUSD) %>%
                     rename(DKKUSD = Denmark)),
                  join_by(year == year)) %>%
        mutate(across(where(is.numeric), ~.x*DKKUSD)) %>%
        group_by(year) %>%
        mutate(sumsector = EXP+DomDom+DomImp+ROWROW,
               OutputWorld = OutputWorld[WIOD_NACE == 'Total Manufacturing'] -
                 sumsector[WIOD_NACE == 'Coke, petroleum'],
               across(3:7, ~.x/OutputWorld),
               vship = GO/(GO[WIOD_NACE == 'Total Manufacturing']-
                             GO[WIOD_NACE == 'Coke, petroleum'])) %>%
        select(-GO, -OutputWorld,-DKKUSD, sumsector) %>%
        filter(WIOD_NACE != 'Coke, petroleum' & WIOD_NACE != 'Total Manufacturing')
      
      WIOD_Total_exCoke <- WIOD_inter_exCoke %>%
        group_by(year) %>%
        summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop') %>%
        mutate(WIOD_NACE = 'Total Manufacturing')
      
      WIOD_final_excoke <- rbind(WIOD_inter_exCoke, WIOD_Total_exCoke)
    }
    
    
    # Remove unnecessary data
    rm(WIOD_World_final)
    rm(WIOD_DNK_final)
    rm(WIOD)
    rm(WIOD_total)
    rm(WIOD_codes)
    rm(WIOD_NACE)
    rm(WIOD_merge)
    rm(WIOD_inter_exCoke)
    rm(WIOD_Total_exCoke)
    
  }
}

# Combine NACE data, FITPPA and Electricity/Heat Production by fueltype
# Transform FITPPA from kWh/USD to kWh/DKK
{
  dta_analysis <- dta_NACE %>%
    left_join(dta_FITPPA %>%
                 full_join(dta_electricityheat,
                           join_by(country == country, year == year)) %>%
                 left_join((as.data.frame(cbind(country_EUROSTAT,countrycode_EUROSTAT))),
                           join_by(country == country_EUROSTAT)) %>%
                 drop_na(countrycode_EUROSTAT) %>%
                 filter(country == 'Denmark') %>%
                 select(!c('country', 'countrycode_EUROSTAT')),
              join_by(year == year)) %>%
    
    # ADD FX rates
    left_join((OECDFX %>%
                 filter(Location %in% c("Denmark", "Germany")) %>%
                 pivot_longer(cols = starts_with("20"),
                              names_to = 'year',
                              values_to = 'FXrateUSD') %>%
                 mutate(FXrateUSD = as.numeric(FXrateUSD)/1000) %>%
                 pivot_wider(names_from = Location,
                             values_from = FXrateUSD)%>%
                 rename(DKKUSD = Denmark,
                        EURUSD = Germany)),
              join_by(year == year)) %>%
    mutate(across(starts_with('FIT'), ~.x*DKKUSD)) %>%
    
    # Add WIOD data
    left_join(WIOD_final, join_by(year == year, NACE_Name == WIOD_NACE))
  
  # Create dta_analysis_exCoke
  {
    dta_analysis_exCoke <- dta_analysis %>%
      filter(NACE_Name != 'Coke, petroleum') %>%
      select(-EXP, -DomDom, -DomImp, -ROWROW, -vship) %>%
      mutate(UseElectricityShare = UseElectricity/(sum(UseElectricity)/2),
             output_share = realoutput/(sum(realoutput)/2)) %>%
      # Add WIOD data
      left_join(WIOD_final_excoke, join_by(year == year, NACE_Name == WIOD_NACE))
  }
}

# Create Data for Matlab
{
  #dta_analysis_exCoke <- readRDS("./Data/dta_analysis_exCoke.rds")
  #options(scipen = 999)
  #dta_MAT <- dta_analysis_exCoke %>%
  #select(year, NACE_Name, NACE_Code, ROWROW, DomImp, EXP, DomDom, vship,
  #       CO2Total, CO2ElectricityHeat) %>%
  #filter(!is.na(ROWROW) & NACE_Name != 'Total Manufacturing') %>%
  #group_by(year) %>%
  #mutate(year_id = cur_group_id()) %>%
  #  group_by(year_id, NACE_Code) %>%
  #  arrange(year_id, NACE_Code) %>%
  #  ungroup() %>%
  #  mutate(vshipDNK  = DomDom+EXP,
  #         vshipROW = ROWROW + DomImp) %>%
    #select(year_id, NACE_Code, vshipROW, vshipDNK) 
    #select(year_id, NACE_Code, ROWROW, DomImp, EXP, DomDom)
    #select(year_id, NACE_Code, CO2Total)
    #select(year_id, NACE_Code, CO2ElectricityHeat)
  
  
}

# Save the Data
{
  saveRDS(dta_decomp, file = "./Data/dta_full.rds")
  saveRDS(dta_analysis, file = "./Data/dta_analysis_elect.rds")
  saveRDS(dta_analysis_exCoke, file = "./Data/dta_analysis_exCoke.rds")
}