"
This Script is to test downloaded Datasets
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
  emissions_01 <- read_xlsx("./Data/DK_Emissions_117grouping_01.xlsx", range = "A3:E2763")
  emissions_02 <- read_xlsx("./Data/DK_Emissions_117grouping_02.xlsx", range = "A3:E2763")
  emissions_03 <- read_xlsx("./Data/DK_Emissions_117grouping_03.xlsx", range = "A3:E2763")
  emissions_04 <- read_xlsx("./Data/DK_Emissions_117grouping_04.xlsx", range = "A3:E2763")
  emissions_05 <- read_xlsx("./Data/DK_Emissions_117grouping_05.xlsx", range = "A3:E2763")
  
  output <- read_xlsx("./Data/DK_OutputNominal_117grouping.xlsx", range = "C3:W122")
  ppi <- read_xlsx("./Data/DK_PPIcommodities_Manufacturing.xlsx", range = "B3:KB46")
  ppi_mapp <- read("./Data/Mapping_PPI_117grouping.xslx", range = "B3:C120")
  
  ember <- read.csv("./Data/EMBER_ElectricityData.csv")
}

# Transform Emissions Data for Merging and Create unique Emissions Dataset
{
  # Transform Emissions Data
  {
    emissions_datalist <- list(emissions_01 = emissions_01,
                               emissions_02 = emissions_02,
                               emissions_03 = emissions_03,
                               emissions_04 = emissions_04,
                               emissions_05 = emissions_05)
    
    for (dataset_name in names(emissions_datalist)) {
      # Rename Columns 1 & 2 with to indicate classification 'classif' and 'year'
      colnames(emissions_datalist[[dataset_name]])[1] <- "classif"
      colnames(emissions_datalist[[dataset_name]])[2] <- "year"
      
      # Fill up NAs of Column 1 by respective classification
      emissions_datalist[[dataset_name]] <- emissions_datalist[[dataset_name]] %>% fill(classif) %>%
        mutate(across(3:5, .fns=as.numeric))
    }
  }
  
  # Create unique Emissions Dataset
  {
    emissions <- emissions_datalist[["emissions_01"]] %>% 
      left_join(emissions_datalist[["emissions_02"]], join_by(classif == classif, year == year)) %>%
      left_join(emissions_datalist[["emissions_03"]], join_by(classif == classif, year == year)) %>%
      left_join(emissions_datalist[["emissions_04"]], join_by(classif == classif, year == year)) %>%
      left_join(emissions_datalist[["emissions_05"]], join_by(classif == classif, year == year))
  }
  
  # Remove unnecessary Data
  {
    rm(emissions_datalist)
    rm(emissions_01)
    rm(emissions_02)
    rm(emissions_03)
    rm(emissions_04)
    rm(emissions_05)
    rm(dataset_name)
  }
  
  # Rename Columns and Subheaders of Columns
  {
    # Create a list with new Variable Names
    emissions_col_names <- c("classif", "year", "CO2inclBiomass", "CO2exclBiomass", "CO2fromBiomass", "SO2", "NOx",
                             "CO", "NH3", "N2O", "CH4", "NMVOC", "PM10", "PM2.5", "SF6", "PFC", "HFC")
    # Create list with attributes
    emissions_col_attr <-  colnames(emissions)
    
    # Rename them
    for(i in 1:ncol(emissions)){
      attr(emissions[[i]], 'label') <- emissions_col_attr[i]
      colnames(emissions)[i] <- emissions_col_names[i]
    }
  }
}

# Transform Output Data to be able to merge with emissions Data in longitudinal-format
{
  colnames(output)[1] <- 'classif'
  output <- output %>% pivot_longer(cols = 2:21, names_to = "year", values_to = "voutput")
  attr(output[["voutput"]], 'label') <- "m DKK"
}

# Transform PPI Data to be able to merge with emissions Data in longitudinal-format
{
  colnames(ppi)[1] <- 'classif'
  
  ppi_t <- ppi %>% select(classif, ends_with("12"))
  
  ppi_tv04 <- ppi_t %>% filter(classif == 'BCD Mining and quarrying and manufacturing and energy supply') %>%
    mutate(across(2:24, .fns=as.numeric)) %>%
    pivot_longer(cols = 2:24, names_to = "year", values_to = "PPI") %>%
    mutate(year = substring(year, 1, 4),
           PPI= (PPI/first(PPI))*100)
}

# Merge Emissions and Output data
{
  dta_decomp <- emissions %>% left_join(output, join_by(classif == classif, year == year)) %>%
    left_join((ppi_tv04 %>% select(year, PPI)), join_by(year == year)) %>%
    filter(!(year %in% c(2020, 2021,2022)))
  dta_decomp <- na.omit(dta_decomp)
}

# Calculate Emission Intensity, real Output and real Output Intensity
{
  dta_decomp <- dta_decomp %>% group_by(classif) %>% arrange(year, .by_group = TRUE) %>%
    mutate(CO2exclBiomass_intensity = CO2exclBiomass/(voutput/1000),
           realoutput = voutput * (PPI/100),
           realouput_intensity = (realoutput*CO2exclBiomass_intensity))
}

# Save the Data
{
  saveRDS(dta_decomp, file = "./Data/dta_analysis.rds")
}
