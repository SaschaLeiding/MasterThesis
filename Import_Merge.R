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
  
  ember <- read.csv("./Data/EMBER_ElectricityData.csv")
  ppi <- read_xlsx("./Data/DK_PPIcommodities_Manufacturing.xlsx", range = "B3:KB46")
  
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

# Decomposition
# SCALE IS NOT ADAPTED WITH PPI
{
  #
  {
    dta_decomp <- dta_decomp %>% group_by(classif) %>% arrange(year, .by_group = TRUE) %>%
      mutate(CO2exclBiomass_intensity = CO2exclBiomass/(voutput/1000),
             realoutput = voutput * (PPI/100),
             realouput_intensity = (realoutput*CO2exclBiomass_intensity), # realouput HAS TO BE CORRECTED by correct PPI
             
             scale = (realoutput/first(realoutput))*100, # voutput HAS TO BE CORRECTED by PPI
             scale_comp_techn = (CO2exclBiomass/first(CO2exclBiomass))*100,
             scale_comp = (realouput_intensity/first(realouput_intensity))*100, # currently same as scale_comp_tech as voutput NOT corrected by PPI
             techn = scale_comp_techn - scale_comp+100,
             comp = scale_comp - scale+100)
    
    attr(dta_decomp[["CO2exclBiomass_intensity"]], 'label') <- "tons Emissions per 1,000 DKK"
    
  }
}


# Trends in Manufacturing Pollution Emissions
{
  dta_emissions_plot <- dta_decomp %>% filter(classif == 'Total') %>%
    select(year, realoutput, CO2exclBiomass, CO, SO2, NOx, PM10, PM2.5, NMVOC) %>%
    pivot_longer(cols = realoutput:NMVOC, values_to = 'Value', names_to = 'Category') %>%
    group_by(Category) %>%
    mutate(normalized_Value = Value/ first(Value) * 100)
  
  lplot_emissions <- ggplot(data = dta_emissions_plot, aes(x = year, y = normalized_Value, color = Category, group = Category)) +
    geom_line() +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = "Base 2000 = 100",
      color = NULL) +
    scale_color_discrete(name = "Greenhouse Gases") +
    #scale_size(range = c(0.5, 1.2), guide = "none") +
    scale_colour_manual(values = c("#FF0000", "#CC6633", "#669900", "#CC3399", "#330099",
                                   "#339999", "#00FFFF", "orange")) +
    theme(legend.position = c(.1, .75))
  lplot_emissions
}

# Line Plot of the Decomposition
{
  dta_decomp_plot <- dta_decomp %>% filter(classif == 'Total') %>%
    select(year, scale, techn, comp) %>%
    pivot_longer(cols = scale:comp, names_to = 'Effect', values_to = 'Values')
  
  lplot_decom <- ggplot(data = dta_decomp_plot, aes(x = year, y = Values, color = Effect, group = Effect)) +
    geom_line() +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = "Base 2000 = 100",
      color = NULL) +
    #scale_color_discrete(name = "Greenhouse Gases", labels = c("A", "B", "C")) +
    #scale_size(range = c(0.5, 1.2), guide = "none") +
    scale_colour_manual(values = c("blue", "green", "black"),
                        labels = c("Composition", "Scale", "Technique")) +
    theme(legend.position = c(.083, .9))
  lplot_decom
}

#Hallo


