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
  #install.packages("xtable")
  #install.packages("patchwork")
  
  library(tidyverse)
  library(xtable)
  library(patchwork)
}

# Load Data
{
  dta_analysis <- readRDS("./Data/dta_analysis.rds")
}

# Define variables for flexibility in Code
ghg <- 'GHGinclBiomass' # Greenhouse gas for the analysis to allow flexibility in choice
base_year <- 2000 # Base year for the normalizing the 3 effects
end_year <- 2019 # End year to define time sequence under observation

# Decomposition
{
  dta_decomp <- dta_analysis %>%
    filter(classsystem == 'ISIC') %>% # NACE=ZEW, ISIC=Shapiro
    group_by(ISIC_Name) %>% 
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
    filter(ISIC_Name == 'Total Manufacturing') %>%
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
    theme(legend.position = c(.15, .22))
  lplot_emissions
}

# Line Plot of the decomposition - standard depiction
{
  dta_decomp_plot_stand <- dta_decomp %>% filter(ISIC_Name == 'Total Manufacturing') %>%
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
  dta_decomp_plot <- dta_decomp %>% filter(ISIC_Name == 'Total Manufacturing') %>%
    select(year, scale, techn, comp, normalized_ghg) %>%
    pivot_longer(cols = scale:normalized_ghg, names_to = 'Effect', values_to = 'Values')

  lplot_decom <- ggplot(data = dta_decomp_plot, aes(x = year, y = Values, color = Effect, group = Effect)) +
    geom_line() +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = paste0("Base ", base_year, " = 100"),
      color = NULL) +
    #scale_colour_manual(values = c("GHG" = "blue", "Scale" = "black", "Composition" = "green", "Technique" = "red"),
    #                    labels = c("GHG", "Scale", "Composition", "Technique")) +
    scale_colour_manual(values = c("blue", "black", "green", "red"),
                        labels = c("Composition", "GHG", "Scale", "Technique")) +
    theme(legend.position = c(.15, .22))
  lplot_decom
}

# Line Plot of Individual Effects and Emissions by Industry
{
  dta_technique <- dta_decomp %>% 
    filter(classsystem == 'ISIC') %>% 
    select(ISIC_Name, year, techn, scale, comp, scale_comp_techn) %>%
    mutate(line_size = ifelse(ISIC_Name == "Total Manufacturing", 1, 0.5))
  
  # Legend Titles
  {
    legend_titles <- c("Basic metals", "Chemicals", "Coke, refined petroleum, fuels",
                       "Fabricated metals", "Food, beverages, tobacco", "Furniture, other, recycling",
                       "Machinery and equipment", "Medical, precision, and optical", "Motor vehicles, trailers",
                       "Office, computing, electrical", "Other non-metallic minerals", "Other transport equipment",      
                       "Paper and publishing", "Rubber and plastics", "Textiles, apparel, fur, leather",
                       "Wood products", "Total Manufacturing")
  }
  
  # Plot Emissions by Industry
  lplot_emiss <- ggplot(data = dta_technique, aes(x=year, y=scale_comp_techn, color=ISIC_Name, group=ISIC_Name)) +
    geom_line()
  
  # Define Variables
  effects_plots <- list()
  year_breaks <- seq(from=base_year, to=end_year, by = 2) # X-Axis
  decomp <- c("techn", "scale", "comp")
  
  # Loop through 'decomp' (individual effects) to create plots by industry
  for (i in decomp) {
    effects_plots[[paste0("lplot_", i)]] <- ggplot(data = dta_technique, aes(x=year, y=!!sym(i), color=ISIC_Name, group=ISIC_Name)) +
      geom_line(size=dta_technique$line_size) +
      scale_x_discrete(breaks = year_breaks) +
      scale_colour_discrete(breaks = legend_titles) +
      labs(x=NULL, y = "Base 2000 = 100") +
      guides(size = FALSE)
  }
  
  # Combine Plots
  {
    # Create Labels for Plots
    label_plots <- map(c("Technique", "Scale", "Composition"), 
                       ~ ggplot() +
                         theme_void() +
                         theme(plot.margin = margin(0, 0, 0)) +
                         annotate("text", x = 0.5, y = 0.5, label = .x, fontface = "bold", hjust = 0.5))
    
    # Combine everything
    final_plot <- (Reduce('+', label_plots) + plot_layout(ncol = 3)) / 
      (Reduce('+', effects_plots) + plot_layout(ncol = 3, axes = "collect")) +
      plot_layout(guides = 'collect', axis_titles = "collect", heights = c(1, 10)) &
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 7), # Adjust this value as needed
            legend.key.size = unit(0.5, "lines"))
    }
  print(final_plot) # Print 11.00-6.00
}
"
Technique Effect is positive for all industries,
Scale Effect differs substantially by industry, see change in Output(real output)
-> Chemicals, Food and Machinery are large drivers of Scale
Composition for all negative

=> Chemicals, Machinery are large drivers for all effects (adjusted for size)
"

# Stylized Facts Table
### NEED TO FIX
{
  dta_style <- dta_decomp %>%
    filter(classsystem == "ISIC" & year == 2000) %>%
    select(ISIC_Name, !!sym(ghg), output_share, realoutput, realexpend, realcostEnergy, ISIC_Code) %>%
    mutate(across(realoutput:realcostEnergy, ~round(.x, digits = 0)),
           output_share = output_share*100) %>%
    rename("Emissions" = "GHGinclBiomass",
           "Output Share" = "output_share",
           "Real Output" = "realoutput",
           "Real Costs" = "realexpend",
           "Real Energy Costs" = "realcostEnergy") %>%
    arrange(ISIC_Code) %>%
    
    left_join(dta_decomp %>%
                filter(classsystem == "ISIC" & (year %in% c(2000,2016))) %>%
                select(ISIC_Name, year, !!sym(ghg), output_share, realoutput, realexpend, realcostEnergy) %>%
                #mutate_if(is.numeric, ~round(.x, digits = 0)) %>%
                group_by(ISIC_Name) %>%
                mutate_if(is.numeric, ~ round(((lead(.x)-.x) / .x)*100, digits = 1)) %>%
                rename("Emissions" = "GHGinclBiomass",
                       "Output Share" = "output_share",
                       "Real Output" = "realoutput",
                       "Real Costs" = "realexpend",
                       "Real Energy Costs" = "realcostEnergy") %>%
                rename_if(is.numeric, ~paste0("Change in ", .x)) %>%
                filter(year == 2000) %>% select(!year),
              join_by(ISIC_Name == ISIC_Name)) %>%
    select(ISIC_Name, Emissions, 'Change in Emissions',
           'Output Share', 'Change in Output Share', 'Real Output', 'Change in Real Output',
           "Real Costs", 'Change in Real Costs',
           'Real Energy Costs', 'Change in Real Energy Costs')
  
  # Print table to LATEX
  # Function to apply italic formatting to specific columns
  italicize_columns <- function(df, cols) {
    for (col in cols) {
      df[[col]] <- sprintf("\\textit{%s}", df[[col]])
    }
    return(df)
  }
  
  # Apply the italic formatting to specified columns by index
  #cols_to_italicize <- c(3, 5, 7, 9 , 11) # Specify columns to italicize by their index
  cols_to_italicize <- grep("^Change", names(dta_style))
  dta_styleitalic <- italicize_columns(dta_style, cols_to_italicize)
  
  # Create a Table for LATEX format
  table_stylized <- xtable(x = dta_styleitalic,
                           digits = c(0,0,0,1,2,1,0,1,0,1,0,1)) # Set number of decimals per column
  
  # Print Parameters table in LATEX format
  print(table_stylized , include.rownames=FALSE, format.args = list(big.mark = ",", decimal.mark = "."))
}