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
  
  # Loading NABO: Production and generation of income by price unity, transaction, industry and time
  output_01 <- read_xlsx("./Data/DK_Production_117grouping_01.xlsx", range = "A4:F2384")
  output_02 <- read_xlsx("./Data/DK_Production_117grouping_02.xlsx", range = "A4:E2384")
  output69_01 <- read_xlsx("./Data/DK_Production_69grouping_01.xlsx", range = "A4:F1495")
  output69_02 <- read_xlsx("./Data/DK_Production_69grouping_02.xlsx", range = "A4:E1495")
  
  # Loading Business Demographics Data
  business_01 <- read_xlsx("./Data/DEM01_BusinessDemo_EntryExit_117grouping.xlsx", range = "B3:X261")
  business_02 <- read_xlsx("./Data/GF01_EnterpriseStatistics_117grouping.xlsx", range = "A3:X390")
  
  # Loading Energy Cost data
  cost_ener <- read_xlsx("./Data/DK_CostsEnergy.xlsx", range = "B3:T1915")
  
  # Loading PPI data
  ppi <- read_xlsx("./Data/DK_PPIcommodities_Manufacturing.xlsx", range = "B3:KB46")
  ppi_mapp117 <- read_xlsx("./Data/Mapping_PPI_117grouping.xlsx", range = "B3:G122")
  ppi_mapp69 <- read_xlsx("./Data/Mapping_PPI_69grouping.xlsx", range = "B2:C73")
  
  ember <- read.csv("./Data/EMBER_ElectricityData.csv")
}

# Define variables for code flexibility
{
  ghg <- 'GHGinclBiomass' # Greenhouse gas for the analysis to allow flexibility in choice
  varname_ghgintensity <- paste0(ghg, "_intensity")
  base_year <- 2005
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
      left_join(emissions_datalist[["emissionsEqui69_02"]], join_by(classif == classif, year == year))))
  }
  
  # Remove unnecessary Data
  {
    rm(emissionsEqui_01)
    rm(emissionsEqui_02)
    rm(emissionsEqui_03)
    rm(emissionsEqui_04)
    rm(emissionsEqui69_01)
    rm(emissionsEqui69_02)
    rm(dataset_name)
  }
  
  # Rename Columns and Subheaders of Columns
  {
    # Create a list with new Variable Names
    emissions_col_names <- c("classif", "year", "GHGexclBiomass", "GHGinclBiomass", 
                             "CO2exclBiomass", "CO2fromBiomass", "N2O_Equi", "CH4_Equi", "Fgases_Equi")
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

# Transform Energy Cost Data
{
  colnames(cost_ener)[1] <- 'costtype'
  colnames(cost_ener)[2] <- 'classif'
  
  cost_ener_trans <- cost_ener %>%
    fill(costtype) %>%
    pivot_longer(cols = starts_with("20"),
                 names_to = 'year',
                 values_to = '.value') %>%
    pivot_wider(names_from = costtype,
                values_from = .value) %>%
    select(1, 2, 9) %>%
    rename(costEnergy = 'Energy expense at purchacers prices (7=1+ ... +6)') %>%
    left_join((ppi_trans %>%
                 ungroup() %>%
                 filter(classif == "Energy (MIG)") %>%
                 select(year, PPI)),
              join_by(year == year)) %>%
    mutate(realcostEnergy = round((costEnergy / (PPI/100)), digits = 2))
}

# Calculate Total of Manufacturing
{
  dta_total <- emissions %>% # Take 'Air Emission Accounts' as Base data
    left_join((emissionsEqui) %>% distinct(), # Note: 'emissionsEqui' contains duplicates
              join_by(classif == classif, year == year)) %>% # Join 'GHG in CO2-equivalents Data
    left_join(output, join_by(classif == classif, year == year)) %>%
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
    left_join(ppi_mapp, join_by(classif == Class_output)) %>% # Join Mapping for PPI to 117 grouping
    left_join(ppi_trans, join_by(Class_PPI == classif, year == year)) %>% # Join PPI values by previously inserted Mapping
    left_join((cost_ener_trans %>% select(!PPI)), 
              join_by(classif == classif, year == year)) %>%
    full_join(dta_total) %>%
    filter(!(year %in% c(2020, 2021,2022))) %>% # Disselect the entries of the years 2020-2022
    distinct()
  #dta_decomp <- na.omit(dta_decomp) # Drop all NA
  # Note: Not dropping NA because all 69 grouping does not have values for individual Fluorinated gases but its sum in CO2-equivalents
}

# Calculate Emission Intensity, real Output and real Output Intensity
{
  dta_decomp <- dta_decomp %>% 
    group_by(classif) %>% 
    arrange(year, .by_group = TRUE) %>%
    mutate(!!varname_ghgintensity := !!sym(ghg)/(voutput/1000),
           realoutput = voutput / (PPI/100),
           expend = interConsumption + CompEmployees,
           realexpend = expend / (PPI/100),
           realouput_intensity = (realoutput * (!!sym(varname_ghgintensity))))
  
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
    mutate(output_share = realoutput/(sum(realoutput)/2)) # divide by 2 because the sum includes each idniv. and the total together
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
    mutate(NACE_Code = NA,
           NACE_Name = NA,
           classsystem = 'ISIC') %>%
    group_by(year) %>%
    mutate(output_share = realoutput/(sum(realoutput)/2)) # divide by 2 because the sum includes each idniv. and the total together
  
}

# Combine NACE and ISIC data
{
  dta_analysis <- dta_NACE %>% full_join(dta_ISIC)
}

# Save the Data
{
  saveRDS(dta_decomp, file = "./Data/dta_full.rds")
  saveRDS(dta_analysis, file = "./Data/dta_analysis.rds")
}