"
This Script is to conduct the Decomposition of Emissions into the Scale, 
Composition and Technique effect.

The input for this script is the data 'dta_analysis.rds' from the script 
'Import_Merge.R'.

The output for this script are Figure X and Figure X

Attention: Variable 'ghg' must be the same in all Scripts
Print Plots as PDF in 'Landscape' 8.00 x 6.00
"

# Install & Load Packages
{
  #install.packages("tidyverse")
  
  library(tidyverse)
}

# Load Data
{
  dta_decomp <- readRDS("./Data/dta_analysis.rds")
}

# Define variables for flexibility in Code
ghg <- 'GHGinclBiomass' # Greenhouse gas for the analysis to allow flexibility in choice
base_year <- 2005 # Base year for the normalizing the 3 effects

# Decomposition
{
  dta_decomp <- dta_decomp %>%
    filter(classsystem == 'NACE') %>% # NACE=ZEW, ISIC=Shapiro
    group_by(ZEW_Name) %>% 
    arrange(year, .by_group = TRUE) %>%
    mutate(scale = (realoutput / realoutput[year == base_year]) * 100, 
           scale_comp_techn = (!!sym(ghg) / (!!sym(ghg))[year == base_year]) * 100,
           scale_comp = (realouput_intensity / realouput_intensity[year == base_year]) * 100,
           techn = scale_comp_techn - scale_comp + 100,
           comp = scale_comp - scale + 100,
           normalized_ghg = (!!sym(ghg) / (!!sym(ghg))[year == base_year]) * 100) %>%
  ungroup()
    
  attr(dta_decomp[[ghg]], 'label') <- "tons Emissions per 1,000 DKK"
}

# Trends in Manufacturing Pollution Emissions
{
  dta_emissions_plot <- dta_decomp %>% 
    filter(ZEW_Name == 'Total Manufacturing') %>%
    select(year, realoutput, GHGinclBiomass, CO2inclBiomass, SO2, NOx, PM10, PM2.5, NMVOC) %>%
    pivot_longer(cols = realoutput:NMVOC, values_to = 'Value', names_to = 'Category') %>%
    group_by(Category) %>%
    mutate(normalized_Value = Value/ Value[year == base_year] * 100) %>%
    ungroup()
  
  lplot_emissions <- ggplot(data = dta_emissions_plot, aes(x = year, y = normalized_Value, color = Category, group = Category)) +
    geom_line() +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = paste0("Base ", base_year, " = 100"),
      color = NULL) +
    scale_colour_manual(values = c("#FF0000", "#CC6633", "#669900", "#CC3399", "#330099",
                                   "#339999", "#00FFFF", "orange"),
                        labels = c("CO² incl. Biomass", "GHG incl. Biomass",
                               "NVMOC", "Nitrogen Oxides", "Particular Matter < 10",
                               "Particular Matter < 2.5", "Real Output", "Sulphur Dioxide")) +
    theme(legend.position = c(.15, .75))
  lplot_emissions
}

# Line Plot of the decomposition - standard depiction
{
  dta_decomp_plot_stand <- dta_decomp %>% filter(ZEW_Name == 'Total Manufacturing') %>%
    select(year, scale, scale_comp_techn, scale_comp) %>%
    pivot_longer(cols = scale:scale_comp, names_to = 'Effect', values_to = 'Values')
  
  lplot_decomstand <- ggplot(data = dta_decomp_plot_stand, 
                             aes(x = year, y = Values, color = Effect, group = Effect)) +
    geom_line(size = 1) +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = paste0("Base ", base_year, " = 100"),
      color = NULL) +
    scale_colour_manual(values = c("blue", "green", "red"),
                        labels = c("Scale", "Scale and Composition", 
                                   "Scale, Composition and Technique")) +
    theme(legend.position = c(.083, .9))
  lplot_decomstand
}

# Line Plot of the Decomposition - individual effects
{
  dta_decomp_plot <- dta_decomp %>% filter(ZEW_Name == 'Total Manufacturing') %>%
    select(year, scale, techn, comp, normalized_ghg) %>%
    pivot_longer(cols = scale:normalized_ghg, names_to = 'Effect', values_to = 'Values')
  
  lplot_decom <- ggplot(data = dta_decomp_plot, aes(x = year, y = Values, color = Effect, group = Effect)) +
    geom_line(size = 1) +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = paste0("Base ", base_year, " = 100"),
      color = NULL) +
    scale_colour_manual(values = c("blue", "black", "green", "red"),
                        labels = c("Composition", "GHG", "Scale", "Technique")) +
    theme(legend.position = c(.083, .9))
  lplot_decom
}