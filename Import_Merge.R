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
  
  # Loading International data
  # Load IEA-Emission Data in million tonnes CO2
  IEA_emissions <- read_xlsx("./Data/IEA_Emissions.xlsx", range = "A4:AQ933")
  IEA_emissions <- IEA_emissions[-1,]
  
  # Load INDSTAT Data in USD
  INDSTAT_1 <- read_xlsx("./Data/INDSTAT_WorldManuf_ISIC.xlsx", sheet = "EmplEstabl", range = "A1:M97980")
  INDSTAT_2 <- read_xlsx("./Data/INDSTAT_WorldManuf_ISIC.xlsx", sheet = "OutpGFI", range = "A1:M83882")
  INDSTAT_3 <- read_xlsx("./Data/INDSTAT_WorldManuf_ISIC.xlsx", sheet = "WagesVA", range = "A1:M99504")
  # Load more detailed data
  INDSTAT_detail <- read_xlsx("./Data/INDSTAT_Output_detail.xlsx", range = "A1:M88932")
  
  # Load Eurostat Trade data in EUR
  EUROSTAT_Import <- read_xlsx("./Data/EUROSTAT_Trade_Import.xlsx", sheet = "Sheet 1", range = "A10:E730")
  EUROSTAT_Export <- read_xlsx("./Data/EUROSTAT_Trade_Export.xlsx", sheet = "Sheet 1", range = "A10:E730")
  
  # Load OECD Exchange rate from 1,000 USD
  OECDFX <- read_xlsx("./Data/OECD_ExchangeRate.xlsx", range = "C3:Z67")
  
  # Load OECD mean Feed-in tariffs and Purchase Power Agreements in USD per kWh
  OECDREFIT <- read_xlsx("./Data/OECD_REFIT.xlsx", range = "A5:J1405") # USD per kWh
  OECDREPPA <- read_xlsx("./Data/OECD_REPPA.xlsx", range = "A5:J1405")
  
  # Load EUROSTAT Electricity Price and Production Data in NATIONAL CURRENCY
  # Prices = national Currency per kWh 
  # Production = GWh
  EUROSTAT_ElectPrices_kWh_01 <- read_xlsx("./Data/EUROSTAT_ElectricityPrices_Industry_01.xlsx",
                                       range = "A11:V101", sheet = 'Sheet 1')
  EUROSTAT_ElectPrices_kWh_02 <- read_xlsx("./Data/EUROSTAT_ElectricityPrices_Industry_02.xlsx",
                                       range = "A11:BP134", sheet = 'Sheet 1')
  EUROSTAT_Production_Electricity <- read_xlsx("./Data/EUROSTAT_Production_Electricity.xlsx",
                                               range = "A9:V829", sheet = 'Sheet 1')
  EUROSTAT_Production_Heat <- read_xlsx("./Data/EUROSTAT_Production_Heat.xlsx",
                                               range = "A9:V829", sheet = 'Sheet 1')

  ember <- read.csv("./Data/EMBER_ElectricityData.csv")
}

# Define variables for code flexibility
{
  ghg <-'CO2Total'  #'CO2ElectricityHeat' # Greenhouse gas for the analysis to allow flexibility in choice
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
  Electricity_Price <- EUROSTAT_ElectPrices_kWh_02 %>%
    full_join(EUROSTAT_ElectPrices_kWh_01, join_by('GEO (Labels)' == 'GEO (Labels)',
                                                   'TAX (Labels)' == 'TAX (Labels)')) %>%
    select(!starts_with("...")) %>%
    mutate(across(3:ncol(.), as.numeric)) %>%
    pivot_longer(cols = 3:ncol(.), names_to = 'year', values_to = '.value') %>%
    pivot_wider(id_cols = c('GEO (Labels)','year'),
                names_from = "TAX (Labels)",
                values_from = ".value") %>%
    mutate(half = str_sub(year, -2),
           year = str_sub(year, 1, 4)) %>%
    rename(country = 'GEO (Labels)',
           electBaseprice = 'Excluding taxes and levies',
           electVATfreeprice = 'Excluding VAT and other recoverable taxes and levies',
           electFinalprice = 'All taxes and levies included') %>%
    group_by(country, year) %>%
    summarise(across(.cols = where(is.numeric), .fns = mean, na.rm = TRUE), .groups = 'drop')
  
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
  
  dta_electricityheat <- ElectricityHeat_Production %>%
    left_join(Electricity_Price,
              join_by(country == country, year == year))
  
  rm(EUROSTAT_ElectPrices_kWh_01)
  rm(EUROSTAT_ElectPrices_kWh_02)
  rm(EUROSTAT_Production_Electricity)
  rm(EUROSTAT_Production_Heat)
  rm(Electricity_Price)
  rm(ElectricityHeat_Production)
}

# Trade Data - from EUR to DKK
{
  # Classification Names
  {
    trade_classif <- c("Food products", "Beverages", "Tobacco products",
                       "Textiles", "Wearing apparel", "Leather and related products",
                       "Wood and of products of wood and cork, except furniture; articles of straw and plaiting materials",
                       "Paper and paper products", "Printing and recording services",
                       "Coke and refined petroleum products",
                       "Chemicals and chemical products","Basic pharmaceutical products and pharmaceutical preparations",                                           
                       "Rubber and plastic products",                                                                             
                       "Other non-metallic mineral products",
                       "Basic metals",
                       "Fabricated metal products, except machinery and equipment",
                       "Computer, electronic and optical products", "Electrical equipment",
                       "Machinery and equipment n.e.c.",
                       "Motor vehicles, trailers and semi-trailers",
                       "Other transport equipment",
                       "Furniture",
                       "Other manufactured goods")
    ISIC_Name <- c("Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                        "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather",
                        "Wood products",
                        "Paper and publishing", "Paper and publishing",
                        "Coke, refined petroleum, fuels",
                        "Chemicals", "Chemicals",
                        "Rubber and plastics",
                        "Other non-metallic minerals",    
                        "Basic metals",
                        "Fabricated metals",
                        "Office, computing, electrical", "Office, computing, electrical",
                        "Machinery and equipment",
                        "Motor vehicles, trailers",
                        "Other transport equipment",
                        "Furniture, other, recycling",
                        "Medical, precision, and optical")
    NACE_Name <- c("Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                        "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather",
                        "Wood products",                         
                        "Pulp, paper, publishing", "Pulp, paper, publishing",
                        "Coke, petroleum",
                        "Chemicals and pharmaceuticals", "Chemicals and pharmaceuticals",
                        "Rubber and plastics",
                        "Non-metallic minerals",
                        "Basic Metals",
                        "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                        "Metal products, electronics, machinery",
                        "Metal products, electronics, machinery",
                        "Vehicles, other transport, n.e.c.",
                        "Vehicles, other transport, n.e.c.",
                        "Vehicles, other transport, n.e.c.",
                        "Metal products, electronics, machinery")
    dta_tradename <- as.data.frame(cbind(trade_classif, ISIC_Name, NACE_Name))
  }
  
  # Colnames of EUROSTAT Export & Import
  {
    colnames(EUROSTAT_Export)[1] <- 'classif'
    colnames(EUROSTAT_Import)[1] <- 'classif'
    colnames(EUROSTAT_Export)[2] <- 'year'
    colnames(EUROSTAT_Import)[2] <- 'year'
    colnames(EUROSTAT_Export)[3] <- 'ExportROW'
    colnames(EUROSTAT_Import)[3] <- 'ImportROW'
    colnames(EUROSTAT_Export)[4] <- 'ExportEU'
    colnames(EUROSTAT_Import)[4] <- 'ImportEU'
    colnames(EUROSTAT_Export)[5] <- 'ExportWorld'
    colnames(EUROSTAT_Import)[5] <- 'ImportWorld'
  }
  
  # Create Trade Data of Denmark
  {
    dta_trade_interim <- EUROSTAT_Export %>%
      left_join(EUROSTAT_Import, join_by(classif == classif, year == year)) %>%
      mutate(across(3:8, as.numeric),
             NetExportsROW = ExportROW - ImportROW,
             NetExportsEU = ExportEU - ImportEU,
             NetExportsWorld = ExportWorld - ImportWorld) %>%
      #Transform from EUR to DKK
      left_join((OECDFX %>%
                   filter(Location %in% c("Euro area (19 countries)", "Denmark")) %>%
                   pivot_longer(cols = starts_with("20"),
                                names_to = 'year',
                                values_to = 'FXrateUSD') %>%
                   mutate(FXrateUSD = ifelse(year > 2002 & Location == "Euro area (19 countries)",
                                             as.numeric(FXrateUSD),
                                             as.numeric(FXrateUSD)/1000)) %>%
                   pivot_wider(names_from = Location,
                               values_from = FXrateUSD) %>%
                   rename(DKKUSD = Denmark,
                          EURUSD = "Euro area (19 countries)") %>%
                   mutate(USDEUR = 1/EURUSD) %>%
                   select(year,DKKUSD, USDEUR)),
                join_by(year == year)) %>%
      mutate(across(3:11, ~(.x*USDEUR)*DKKUSD)) %>%
      select(-c("USDEUR", "DKKUSD")) %>%
      left_join(dta_tradename, join_by(classif == trade_classif))
  }
  
  # Create Trade NACE & ISIC Data
  {
    # Create NACE Data
    dta_trade_NACE <- dta_trade_interim %>%
      #select(!ISIC_Name) %>%
      filter(!is.na(NACE_Name)) %>%
      group_by(year, NACE_Name) %>%
      summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE),.groups = 'drop') %>%
      mutate(ISIC_Name = NA)
      
    
    # Create ISIC Data
    dta_trade_ISIC <- dta_trade_interim %>%
      filter(!is.na(ISIC_Name)) %>%
      group_by(year, ISIC_Name) %>%
      summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE),.groups = 'drop') %>%
      mutate(NACE_Name = NA)
  }
  
  # Create 'Total Manufacturing'
  {
    dta_tradeTotal <- dta_trade_interim %>%
      filter(!is.na(ISIC_Name)) %>%
      group_by(year) %>%
      summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE), .groups = 'drop') %>%
      mutate(#classif = 'Total Manufacturing',
             ISIC_Name = 'Total Manufacturing',
             #       Class_PPI = 'C Manufacturing',
             NACE_Name = 'Total Manufacturing')
  }
  
  # Merge Trade data
  dta_trade <- rbind(dta_tradeTotal, dta_trade_ISIC, dta_trade_NACE)
  
  rm(dta_trade_interim)
  rm(dta_trade_ISIC)
  rm(dta_trade_NACE)
  rm(dta_tradeTotal)
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
           wage = CompEmployees/Employees)
  
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
    left_join(dta_trade, join_by(NACE_Name == NACE_Name, year == year)) %>%
    mutate(ISIC_Code = NA,
           ISIC_Name = NA,
           classsystem = 'NACE') %>%
    group_by(year) %>%
    mutate(output_share = realoutput/(sum(realoutput)/2), # divide by 2 because the sum includes each indiv. and the total together
           wage_manuf = CompEmployees[NACE_Name == 'Total Manufacturing'] / Employees[NACE_Name ==  'Total Manufacturing'],
           UseElectricityShare = UseElectricity/(sum(UseElectricity)/2))
}

# Create Shapiro&Walker(ISIC)-classification data
{
  dta_ISICsum <- dta_decomp %>%
    filter(!is.na(ISIC_Code)) %>%
    group_by(year, ISIC_Code) %>%
    filter(n()>1) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = 'drop') %>%
    ungroup() %>%
    mutate(!!varname_ghgintensity := !!sym(ghg)/(voutput/1000),
           realouput_intensity = (realoutput * (!!sym(varname_ghgintensity)))) %>%
    left_join(unique(ppi_mapp %>% select(ISIC_Code, ISIC_Name)), join_by(ISIC_Code == ISIC_Code))
  
  ISICcode_sum <- unique(dta_ISICsum$ISIC_Code)
  
  dta_ISIC <- dta_ISICsum %>%
    full_join(dta_decomp %>%
                filter(!is.na(ISIC_Code) & !(ISIC_Code %in% ISICcode_sum))) %>%
    left_join(dta_trade, join_by(ISIC_Name == ISIC_Name, year == year)) %>%
    mutate(NACE_Code = NA,
           NACE_Name = NA,
           classsystem = 'ISIC') %>%
    group_by(year) %>%
    mutate(output_share = realoutput/(sum(realoutput)/2),# divide by 2 because the sum includes each idniv. and the total together
           wage_manuf = CompEmployees[ISIC_Name == 'Total Manufacturing'] / Employees[ISIC_Name ==  'Total Manufacturing'],
           UseElectricityShare = UseElectricity/(sum(UseElectricity)/2))
}

# Merge International Data
{
  # Classification Match
  {
    classif_ISIC <- c("Total Manufacturing", 
                      "Food, beverages, tobacco", "Food, beverages, tobacco",
                      "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather",
                      "Paper and publishing", "Paper and publishing",
                      "Chemicals", 
                      "Other non-metallic minerals",
                      "Machinery and equipment", "Machinery and equipment", "Machinery and equipment",
                      "Office, computing, electrical",
                      "Furniture, other, recycling", "Furniture, other, recycling",
                      "Wood products",
                      "Coke, refined petroleum, fuels",
                      "Rubber and plastics",
                      "Basic metals",                  
                      "Fabricated metals",
                      "Motor vehicles, trailers",
                      "Other transport equipment",
                      "Medical, precision, and optical")
    
    classif_NACE <- c("Total Manufacturing", 
                      "Food, beverages, tobacco", "Food, beverages, tobacco",
                      "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather",
                      "Pulp, paper, publishing", "Pulp, paper, publishing",
                      "Chemicals and pharmaceuticals", 
                      "Non-metallic minerals",
                      "Metal products, electronics, machinery", "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                      "Metal products, electronics, machinery",
                      "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.",
                      "Wood products",
                      "Coke, petroleum",
                      "Rubber and plastics",
                      "Basic Metals",                  
                      "Metal products, electronics, machinery",
                      "Vehicles, other transport, n.e.c.",
                      "Vehicles, other transport, n.e.c.",
                      "Vehicles, other transport, n.e.c.")
    
    classif_IEA <- c("Manufacturing",
                     "Food and tobacco", "Food and tobacco",
                     "Textiles and leather", "Textiles and leather", "Textiles and leather",
                     "Paper, pulp and printing", "Paper, pulp and printing",
                     "Chemicals and chemical products",
                     "Non-metallic minerals",
                     "Machinery", "Machinery", "Machinery",
                     "Machinery",
                     "Non-specified manufacturing", "Other manufacturing",
                     "Wood and wood products",
                     "Memo: Coke and refined petroleum products",
                     "Rubber and plastic",
                     "Basic metals",
                     "Machinery",
                     "Transport equipment",
                     "Transport equipment",
                     "Other manufacturing")
    
    classif_INDSTAT <- c("Total manufacturing",
                         "Food and beverages", "Tobacco products",
                         "Textiles", "Wearing apparel, fur", "Leather, leather products and footwear",
                         "Printing and publishing", "Paper and paper products",
                         "Chemicals and chemical products",
                         "Non-metallic mineral products",
                         "Machinery and equipment n.e.c.", "Electrical machinery and apparatus", "Radio,television and communication equipment",
                         "Office, accounting and computing machinery",
                         "Furniture; manufacturing n.e.c.", "Recycling",
                         "Wood products (excl. furniture)",
                         "Coke,refined petroleum products,nuclear fuel",
                         "Rubber and plastics products",
                         "Basic metals",
                         "Fabricated metal products",
                         "Motor vehicles, trailers, semi-trailers",
                         "Other transport equipment",
                         "Medical, precision and optical instruments")
  }
  
  # Country codes for IEA
  {
    country_IEA <- c("Australia", "Austria", "Belgium", "Canada", "Czech Republic", 
                     "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", 
                     "Hungary", "Ireland", "Italy", "Japan", "Korea", "Lithuania", 
                     "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", 
                     "Poland", "Portugal", "Slovak Republic", "Spain", "Sweden", 
                     "Switzerland", "Republic of Türkiye", "United Kingdom", 
                     "United States", "Argentina", "Brazil", "Morocco", "South Africa", 
                     "Ukraine", "Chile", "Latvia", "Albania", "Armenia", "Azerbaijan", 
                     "Belarus", "Bosnia and Herzegovina", "Bulgaria", "Croatia", 
                     "Cyprus", "Georgia", "Kazakhstan", "Kosovo", "Kyrgyzstan", 
                     "Republic of North Macedonia", "Malta", "Republic of Moldova", 
                     "Romania", "Serbia", "Slovenia", "Uruguay", "Uzbekistan")
    countrycode_IEAINDSTAT <- c(036, 040, 056, 124, 203, 208, 233, 246, 250, 276, 300,
                                348, 372, 380, 392, 410, 440, 442, 484, 528, 554, 
                                578, 616, 620, 703, 724, 752, 756, 792, 826, 840,
                                032, 076, 504, 710, 804, 152, 428, 008, 051, 031, 112, 070,
                                100, 191, 196, 268, 398, 412, 417, 807, 470, 498, 
                                642, 688, 705, 858, 860)
    
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
  
  # Calculate emissions for Total Manufacturing because in Base ISIC 19 (manuf of Fuel) is excluded
  {
    dta_IEAmanuf <- IEA_emissions%>%
      rename('country' = 'Time',
             classif = ...2) %>%
      select(-starts_with("..")) %>%
      fill(country) %>%
      mutate(classif = str_remove(classif, " \\[.*")) %>%
      pivot_longer(cols = starts_with("20"),
                   names_to = 'year',
                   values_to = 'Emissions') %>% # in Million tonnes CO2
      mutate(Emissions = Emissions* 1000) %>%
      filter(!(classif %in% c('Manufacturing', 'Ferrous metals', 'Non-ferrous metals'))) %>%
      group_by(country, year) %>%
      summarise(emission = sum(Emissions, na.rm = TRUE), .groups = "keep")
  }
  
  # Merge aggregated INDTSTAT Manufacturing Data
  {
    dta_aggrINDTSTAT <- rbind(INDSTAT_1, INDSTAT_2, INDSTAT_3) %>% # Create Baseline DF with INDSTAT, as most comprehensive
      select('Country Description', 'Country Code', Year, 'ISIC Description', 'Table Description...2', Value) %>%
      rename(country = 'Country Description',
             countrycode_INDSTAT = 'Country Code',
             year = Year,
             INDSTAT_Name = 'ISIC Description') %>%
      pivot_wider(names_from = 'Table Description...2', values_from = Value) %>%
      rename(Firms = Establishments,
             grossFixCap = 'Gross fixed capital formation',
             totalwages = 'Wages and salaries',
             VA = 'Value added') %>%
      mutate(Firms = as.numeric(Firms),
             Employees = as.numeric(Employees),
             Output = as.numeric(Output),
             grossFixCap = as.numeric(grossFixCap),
             totalwages = as.numeric(totalwages),
             VA = as.numeric(VA),
             wage = totalwages/Employees) %>%
      # ADD classifications to match with other data
      left_join((data.frame(classif_ISIC, classif_NACE,classif_IEA, classif_INDSTAT)),
                join_by(INDSTAT_Name== classif_INDSTAT))
  }
  
  # Summarise aggregate data by NACE sector
  {
    dta_internat_NACE <- dta_aggrINDTSTAT %>%
      group_by(country, year, classif_NACE) %>%
      summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE), .groups = 'drop')%>%
      rename(NACE_Name = classif_NACE)
  }
  
  # Create Dataframe of matching Variables for detailed Data
  {
    NACE_Name <- c("Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco", 
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco", 
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco", 
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco", 
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                   "Food, beverages, tobacco", 
                   "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather", 
                   "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather",
                   "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather", 
                   "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather", 
                   "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather", 
                   "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather", 
                   "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather", 
                   "Textiles, wearing apparel, leather", "Textiles, wearing apparel, leather",
                   "Textiles, wearing apparel, leather",
                   "Wood products", "Wood products", "Wood products",
                   "Wood products", "Wood products", "Wood products",
                   "Pulp, paper, publishing", "Pulp, paper, publishing", 
                   "Pulp, paper, publishing", "Pulp, paper, publishing", 
                   "Pulp, paper, publishing", "Pulp, paper, publishing", 
                   "Pulp, paper, publishing", "Pulp, paper, publishing",
                   "Coke, petroleum", "Coke, petroleum", 
                   "Chemicals and pharmaceuticals", "Chemicals and pharmaceuticals", 
                   "Chemicals and pharmaceuticals", "Chemicals and pharmaceuticals",
                   "Chemicals and pharmaceuticals", "Chemicals and pharmaceuticals", 
                   "Chemicals and pharmaceuticals", "Chemicals and pharmaceuticals",
                   "Chemicals and pharmaceuticals", "Chemicals and pharmaceuticals",
                   "Chemicals and pharmaceuticals", 
                   "Rubber and plastics", "Rubber and plastics", 
                   "Rubber and plastics", "Rubber and plastics", 
                   "Non-metallic minerals", "Non-metallic minerals", "Non-metallic minerals", 
                   "Non-metallic minerals", "Non-metallic minerals", "Non-metallic minerals", 
                   "Non-metallic minerals", "Non-metallic minerals", "Non-metallic minerals", 
                   "Basic Metals", "Basic Metals", "Basic Metals", 
                   "Basic Metals", "Basic Metals", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery", 
                   "Metal products, electronics, machinery", "Metal products, electronics, machinery",
                   "Metal products, electronics, machinery",
                   "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", 
                   "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.",
                   "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.",
                   "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", 
                   "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", 
                   "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", 
                   "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", 
                   "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", 
                   "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", 
                   "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", "Vehicles, other transport, n.e.c.", 
                   "Total Manufacturing")
    
    ISIC_Name <- c("Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                   "Food, beverages, tobacco", "Food, beverages, tobacco", "Food, beverages, tobacco",
                   "Food, beverages, tobacco",
                   "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather",
                   "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather",
                   "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather",
                   "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather",
                   "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather",
                   "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather",
                   "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather",
                   "Textiles, apparel, fur, leather", "Textiles, apparel, fur, leather",
                   "Textiles, apparel, fur, leather",
                   "Wood products", "Wood products", "Wood products",
                   "Wood products", "Wood products", "Wood products", 
                   "Paper and publishing", "Paper and publishing", "Paper and publishing",
                   "Paper and publishing", "Paper and publishing", "Paper and publishing",
                   "Paper and publishing", "Paper and publishing",
                   "Coke, refined petroleum, fuels", "Coke, refined petroleum, fuels",
                   "Chemicals", "Chemicals", "Chemicals", "Chemicals",
                   "Chemicals", "Chemicals", "Chemicals", "Chemicals", 
                   "Chemicals", "Chemicals", "Chemicals",
                   "Rubber and plastics", "Rubber and plastics",
                   "Rubber and plastics", "Rubber and plastics",
                   "Other non-metallic minerals", "Other non-metallic minerals", 
                   "Other non-metallic minerals", "Other non-metallic minerals", 
                   "Other non-metallic minerals", "Other non-metallic minerals", 
                   "Other non-metallic minerals", "Other non-metallic minerals",
                   "Other non-metallic minerals",
                   "Basic metals", "Basic metals", "Basic metals", "Basic metals",
                   "Basic metals",
                   "Fabricated metals", "Fabricated metals", "Fabricated metals",
                   "Fabricated metals", "Fabricated metals", "Fabricated metals",
                   "Fabricated metals", "Fabricated metals", "Fabricated metals",
                   "Fabricated metals", 
                   "Office, computing, electrical", "Office, computing, electrical",
                   "Office, computing, electrical", "Office, computing, electrical",
                   "Medical, precision, and optical", "Medical, precision, and optical",
                   "Medical, precision, and optical", "Medical, precision, and optical",
                   "Medical, precision, and optical", "Medical, precision, and optical",
                   "Office, computing, electrical", "Office, computing, electrical",
                   "Office, computing, electrical", "Office, computing, electrical",
                   "Office, computing, electrical", "Office, computing, electrical",
                   "Office, computing, electrical", "Office, computing, electrical",
                   "Office, computing, electrical",
                   "Machinery and equipment", "Machinery and equipment", "Machinery and equipment",
                   "Machinery and equipment", "Machinery and equipment", "Machinery and equipment", 
                   "Machinery and equipment", "Machinery and equipment", "Machinery and equipment", 
                   "Machinery and equipment", "Machinery and equipment", "Machinery and equipment", 
                   "Machinery and equipment", "Machinery and equipment", "Machinery and equipment", 
                   "Machinery and equipment", "Machinery and equipment", "Machinery and equipment", 
                   "Motor vehicles, trailers", "Motor vehicles, trailers", "Motor vehicles, trailers",
                   "Other transport equipment", "Other transport equipment", "Other transport equipment", 
                   "Other transport equipment", "Other transport equipment", "Other transport equipment", 
                   "Other transport equipment", "Other transport equipment", "Other transport equipment", 
                   "Other transport equipment",
                   "Furniture, other, recycling", "Furniture, other, recycling", 
                   "Furniture, other, recycling", "Furniture, other, recycling", 
                   "Furniture, other, recycling", "Furniture, other, recycling", 
                   "Furniture, other, recycling", 
                   "Medical, precision, and optical", 
                   "Furniture, other, recycling", "Furniture, other, recycling",
                   "Furniture, other, recycling", "Furniture, other, recycling",
                   "Furniture, other, recycling", "Furniture, other, recycling",
                   "Furniture, other, recycling", "Furniture, other, recycling",
                   "Furniture, other, recycling", 
                   "Total Manufacturing")
    
    dINDSTAT_name <- c("1010 Processing/preserving of meat", "1020 Processing/preserving of fish, etc.", 
                       "1030 Processing/preserving of fruit,vegetables", "1040 Vegetable and animal oils and fats", 
                       "1050 Dairy products",
                       "106 Grain mill products,starches and starch products",
                       "1061 Grain mill products", "1062 Starches and starch products", 
                       "107 Other food products", "1071 Bakery products", "1072 Sugar", 
                       "1073 Cocoa, chocolate and sugar confectionery", "1074 Macaroni, noodles, couscous, etc.", 
                       "1075 Prepared meals and dishes", "1079 Other food products n.e.c.", 
                       "1080 Prepared animal feeds", "110 Beverages",
                       "1101 Distilling, rectifying and blending of spirits",
                       "1102 Wines", "1103 Malt liquors and malt", 
                       "1104 Soft drinks,mineral waters,other bottled waters",
                       "1200 Tobacco products",
                       "131 Spinning, weaving and finishing of textiles",
                       "1311 Preparation and spinning of textile fibres",
                       "1312 Weaving of textiles", 
                       "1313 Finishing of textiles",
                       "139 Other textiles", 
                       "1391 Knitted and crocheted fabrics",
                       "1392 Made-up textile articles, except apparel",
                       "1393 Carpets and rugs", "1394 Cordage, rope, twine and netting", 
                       "1399 Other textiles n.e.c.", "1410 Wearing apparel, except fur apparel",
                       "1420 Articles of fur",
                       "1430 Knitted and crocheted apparel",
                       "151 Leather;luggage,handbags,saddlery,harness;fur",
                       "1511 Tanning/dressing of leather; dressing of fur", 
                       "1512 Luggage,handbags,etc.;saddlery/harness",
                       "1520 Footwear",
                       "1610 Sawmilling and planing of wood",
                       "162 Wood products, cork, straw, plaiting materials", 
                       "1621 Veneer sheets and wood-based panels", "1622 Builders' carpentry and joinery",
                       "1623 Wooden containers",
                       "1629 Other wood products;articles of cork,straw",
                       "170 Paper and paper products",
                       "1701 Pulp, paper and paperboard", 
                       "1702 Corrugated paper and paperboard", "1709 Other articles of paper and paperboard",
                       "181 Printing and service activities related to printing",
                       "1811 Printing",
                       "1812 Service activities related to printing", "1820 Reproduction of recorded media",
                       "1910 Coke oven products", "1920 Refined petroleum products",
                       "201 Basic chemicals,fertilizers, etc.",
                       "2011 Basic chemicals",
                       "2012 Fertilizers and nitrogen compounds", "2013 Plastics and synthetic rubber in primary forms",
                       "202 Other chemical products", 
                       "2021 Pesticides and other agrochemical products",
                       "2022 Paints,varnishes;printing ink and mastics", 
                       "2023 Soap,cleaning and cosmetic preparations",
                       "2029 Other chemical products n.e.c.", "2030 Man-made fibres",
                       "2100 Pharmaceuticals,medicinal chemicals, etc.",
                       "221 Rubber products",
                       "2211 Rubber tyres and tubes", "2219 Other rubber products",
                       "2220 Plastics products", "2310 Glass and glass products",
                       "239 Non-metallic mineral products n.e.c.",
                       "2391 Refractory products",
                       "2392 Clay building materials", "2393 Other porcelain and ceramic products",
                       "2394 Cement, lime and plaster", "2395 Articles of concrete, cement and plaster",
                       "2396 Cutting, shaping and finishing of stone",
                       "2399 Other non-metallic mineral products n.e.c.", 
                       "2410 Basic iron and steel", "2420 Basic precious and other non-ferrous metals",
                       "243 Casting of metals", "2431 Casting of iron and steel",
                       "2432 Casting of non-ferrous metals",
                       "251 Struct.metal products, tanks, reservoirs", 
                       "2511 Structural metal products", "2512 Tanks, reservoirs and containers of metal",
                       "2513 Steam generators, excl. hot water boilers", "2520 Weapons and ammunition",
                       "259 Other metal products;metal working services",
                       "2591 Forging,pressing,stamping,roll-forming of metal", 
                       "2592 Treatment and coating of metals; machining", 
                       "2593 Cutlery, hand tools and general hardware",
                       "2599 Other fabricated metal products n.e.c.",
                       "2610 Electronic components and boards", 
                       "2620 Computers and peripheral equipment", "2630 Communication equipment",
                       "2640 Consumer electronics",
                       "265 Measuring,testing equipment; watches, etc.", 
                       "2651 Measuring/testing/navigating equipment,etc.",
                       "2652 Watches and clocks",
                       "2660 Irradiation/electromedical equipment,etc.",
                       "2670 Optical instruments and photographic equipment", 
                       "2680 Magnetic and optical media", "2710 Electric motors,generators,transformers,etc.", "2720 Batteries and accumulators", 
                       "273 Wiring and wiring devices", 
                       "2731 Fibre optic cables", 
                       "2732 Other electronic and electric wires and cables", "2733 Wiring devices", 
                       "2740 Electric lighting equipment", "2750 Domestic appliances", 
                       "2790 Other electrical equipment", 
                       "281 General-purpose machinery", 
                       "2811 Engines/turbines,excl.aircraft,vehicle engines", "2812 Fluid power equipment", "2813 Other pumps, compressors, taps and valves",
                       "2814 Bearings, gears, gearing and driving elements", "2815 Ovens, furnaces and furnace burners", "2816 Lifting and handling equipment",
                       "2817 Office machinery, excl.computers,etc.", "2818 Power-driven hand tools", "2819 Other general-purpose machinery", 
                       "282 Special-purpose machinery",
                       "2821 Agricultural and forestry machinery", "2822 Metal-forming machinery and machine tools", "2823 Machinery for metallurgy",
                       "2824 Mining, quarrying and construction machinery", "2825 Food/beverage/tobacco processing machinery",
                       "2826 Textile/apparel/leather production machinery", "2829 Other special-purpose machinery", "2910 Motor vehicles", 
                       "2920 Automobile bodies, trailers and semi-trailers", "2930 Parts and accessories for motor vehicles", 
                       "301 Building of ships and boats",
                       "3011 Building of ships and floating structures", "3012 Building of pleasure and sporting boats",
                       "3020 Railway locomotives and rolling stock", "3030 Air and spacecraft and related machinery", "3040 Military fighting vehicles",
                       "309 Transport equipment n.e.c.", 
                       "3091 Motorcycles", "3092 Bicycles and invalid carriages", "3099 Other transport equipment n.e.c.",
                       "3100 Furniture", 
                       "321 Jewellery, bijouterie and related articles", 
                       "3211 Jewellery and related articles", "3212 Imitation jewellery and related articles",
                       "3220 Musical instruments", "3230 Sports goods", "3240 Games and toys", 
                       "3250 Medical and dental instruments and supplies", "3290 Other manufacturing n.e.c.", 
                       "331 Repair of fabricated metal products/machinery",
                       "3311 Repair of fabricated metal products", "3312 Repair of machinery",
                       "3313 Repair of electronic and optical equipment", "3314 Repair of electrical equipment", 
                       "3315 Repair of transport equip., excl. motor vehicles", "3319 Repair of other equipment", 
                       "3320 Installation of industrial machinery/equipment", "C Total manufacturing")
    
    # Concatenate data
    detailINDSTAT_names <- as.data.frame(cbind(dINDSTAT_name, ISIC_Name, NACE_Name))
    
    rm(dINDSTAT_name)
    rm(ISIC_Name)
    rm(NACE_Name)
  }
  
  # Retrieve missing data of NACE: dta_miss
  {
    dta_miss <- dta_internat_NACE %>%
      filter(Output==0 & year >= base_year & year <= '2016') %>%
      select(country, year, NACE_Name)
  }
 
  # Transform detailed INDSTAT data
  {
    INDSTAT_test <- INDSTAT_detail %>%
      mutate(concISIC = paste(ISIC,`ISIC Description`),
             Value = as.numeric(Value)) %>%
      left_join(detailINDSTAT_names, join_by(concISIC==dINDSTAT_name)) %>%
      rename(country = 'Country Description',
             countrycode_INDSTAT = 'Country Code',
             year = Year,
             INDSTAT_Name = 'ISIC Description') %>%
      select(country, countrycode_INDSTAT, year, INDSTAT_Name, Value, NACE_Name)
  }
  
  # Summarise detailed data by NACE sector
  {
    INDSTAT_Nace <- INDSTAT_test %>%
      group_by(country, year, NACE_Name) %>%
      summarise(Output = sum(Value, na.rm = TRUE), .groups = "keep") %>%
      ungroup()
  }
  
  # Fill missing data with more detailed INDSTAT data
  {
    dta_fillmiss <- dta_miss %>%
      left_join(INDSTAT_Nace,
                join_by(country == country, year == year,
                        NACE_Name == NACE_Name))
  }
  
  # OLD: IEA emission data not needed and dont need to summarise by ISIC
  {
      # Summarise data by ISIC sector
      #dta_internat_ISIC <- dta_aggrINDTSTAT %>%
      #  group_by(country, year, classif_ISIC) %>%
      #  summarise(across(.cols = where(is.numeric), .fns = sum, na.rm = TRUE), .groups = 'drop') %>%
      #  rename(ISIC_Name = classif_ISIC) %>%
      #  mutate(NACE_Name = NA)
      
      # Combine International data
      #dta_aggrWorld <- rbind(dta_internat_NACE, dta_internat_ISIC)
      
      # Summarise detailed INDSTAT data by ISIC classification
      #INDSTAT_ISIC <- INDSTAT_test %>%
      #  group_by(country, year, ISIC_Name) %>%
      #  summarise(Output = sum(Value, na.rm = TRUE), .groups = "keep") %>%
      #  ungroup() %>%
      #  mutate(NACE_Name = NA)
      
      # Combine detailed International data
      #dta_detailWorld <- rbind(INDSTAT_Nace, INDSTAT_ISIC)
      
      # Add Emission data
    #  left_join((IEA_emissions %>%
     #              rename('country' = 'Time',
      #                    classif = ...2) %>%
      #             select(-starts_with("..")) %>%
      #             fill(country) %>%
      #             mutate(classif = str_remove(classif, " \\[.*")) %>%
      #             pivot_longer(cols = starts_with("20"),
      #                          names_to = 'year',
      #                          values_to = 'Emissions') %>% # in Million tonnes CO2
      #             mutate(Emissions = Emissions* 1000) %>%
      #             left_join(as.data.frame(cbind(country_IEA, countrycode_IEAINDSTAT)),
      #                       join_by(country == country_IEA)) %>%
      #             select(!country)),
      #          join_by(classif_IEA == classif,
      #                  year == year,
      #                  countrycode_INDSTAT == countrycode_IEAINDSTAT)) %>%
    
    # Add Wage from Total Manufacturing
    #group_by(country,year) %>%
    #mutate(wage_manuf = if (any(INDSTAT_Name == 'Total manufacturing')) {
    #  wage[INDSTAT_Name == 'Total manufacturing']
    #} else {
    #  NA_real_ # This ensures that the NA has the same type as wage
    #}) %>%
    #ungroup() %>%
    }
  
  dta_internatROW <- rbind((dta_internat_NACE %>% select(country, year, NACE_Name, Output)), dta_fillmiss) %>%
    filter(year >= base_year & year <= 2016 & country != "Denmark") %>%
    group_by(year, NACE_Name) %>%
    summarise(OutputWorld = sum(Output, na.rm = TRUE), .groups = "keep")
  
  dta_internatDNK <- rbind((dta_internat_NACE %>% select(country, year, NACE_Name, Output)), dta_fillmiss) %>%
    filter(year >= base_year & year <= 2016 & country == "Denmark") %>%
    rename(OutputDNK = Output)
  
  dta_internat <- dta_internatROW %>%
    left_join(dta_internatDNK, join_by(year == year, NACE_Name == NACE_Name)) %>%

    # Add OECD Exchange rate to USD
    left_join((OECDFX %>%
                 filter(Location == "Denmark") %>%
                 pivot_longer(cols = starts_with("20"),
                              names_to = 'year',
                              values_to = 'FXrateUSD') %>%
                 mutate(FXrateUSD = as.numeric(FXrateUSD)/1000) %>%
                 pivot_wider(names_from = Location,
                             values_from = FXrateUSD)%>%
                 rename(DKKUSD = Denmark)), join_by(year == year)) %>%
    mutate(OutputWorld = (OutputWorld*DKKUSD)/1000000,
           OutputDNK = (OutputDNK*DKKUSD)/1000000) %>%
    select(NACE_Name, year, OutputWorld, OutputDNK) %>%
    filter(!is.na(OutputDNK))
  
  # Comparison of INDSTAT versus Statbank DK by sector
  dta_manufcomp <- dta_internat %>%
    select(!OutputWorld) %>%
    left_join((dta_NACE %>%
                 select(NACE_Name, year, voutput)),
              join_by(NACE_Name == NACE_Name, year == year)) %>%
    mutate(deviation = (OutputDNK/voutput)-1)
}

# Combine NACE, ISIC data, FITPPA and Electricity/Heat Production by fueltype
# Transform FITPPA from kWh/USD to kWh/DKK
{
  dta_analysis <- dta_NACE %>%
    #full_join(dta_ISIC) %>%
    left_join(dta_FITPPA %>%
                 full_join(dta_electricityheat,
                           join_by(country == country, year == year)) %>%
                 left_join((as.data.frame(cbind(country_EUROSTAT,countrycode_EUROSTAT))),
                           join_by(country == country_EUROSTAT)) %>%
                 drop_na(countrycode_EUROSTAT) %>%
                 filter(country == 'Denmark') %>%
                 select(!c('country', 'countrycode_EUROSTAT')),
              join_by(year == year)) %>%
    left_join(dta_internat, join_by(NACE_Name == NACE_Name, year == year)) %>%
    
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
    mutate(across(starts_with('FIT'), ~.x*DKKUSD))
}

# Save the Data
{
  saveRDS(dta_decomp, file = "./Data/dta_full.rds")
  saveRDS(dta_analysis, file = "./Data/dta_analysis.rds")
  #saveRDS(dta_internat, file = "./Data/dta_internat.rds")
}