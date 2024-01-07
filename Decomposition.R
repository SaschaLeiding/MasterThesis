"
This Script is to conduct the Decomposition of Emissions into the Scale, 
Composition and Technique effect.

The input for this script is the data 'dta_analysis.rds' from the script 
'Import_Merge.R'.

The output for this script are Figure X and Figure X

Attention: Variable 'ghg' must be the same in all Scripts
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

# Decomposition
{
  #
  {
    dta_decomp <- dta_decomp %>% group_by(classif) %>% arrange(year, .by_group = TRUE) %>%
      mutate(scale = (realoutput/first(realoutput))*100, 
             scale_comp_techn = (!!sym(ghg)/first(!!sym(ghg)))*100,
             scale_comp = (realouput_intensity/first(realouput_intensity))*100,
             techn = scale_comp_techn - scale_comp+100,
             comp = scale_comp - scale+100,
             normalized_ghg = (!!sym(ghg)/first(!!sym(ghg)))*100)
    
    attr(dta_decomp[[ghg]], 'label') <- "tons Emissions per 1,000 DKK"
    
  }
}

# Trends in Manufacturing Pollution Emissions
{
  dta_emissions_plot <- dta_decomp %>% filter(classif == 'Total') %>%
    select(year, realoutput, GHGinclBiomass, CO2inclBiomass, SO2, NOx, PM10, PM2.5, NMVOC) %>%
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
    select(year, scale, techn, comp, normalized_ghg) %>%
    pivot_longer(cols = scale:normalized_ghg, names_to = 'Effect', values_to = 'Values')
  
  lplot_decom <- ggplot(data = dta_decomp_plot, aes(x = year, y = Values, color = Effect, group = Effect)) +
    geom_line() +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = "Base 2000 = 100",
      color = NULL) +
    #scale_color_discrete(name = "Greenhouse Gases", labels = c("A", "B", "C")) +
    #scale_size(range = c(0.5, 1.2), guide = "none") +
    scale_colour_manual(values = c("blue", "black", "green", "red"),
                        labels = c("Composition", "GHG", "Scale", "Technique")) +
    theme(legend.position = c(.083, .9))
  lplot_decom
}