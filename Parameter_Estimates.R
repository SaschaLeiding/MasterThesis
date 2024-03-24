"
This Script is to do the Parameter Estimates per industry and time.
It calculates the parameters 'tons pollution per m DKK', 'Input Share',
'Elasticity of Substitution' and various 'Pollution Elasticity'
Pollution elasticities compromise:
(a) 'pollutionelasticityISIC' = U.S. mean scaled by pollution per sector
(b) 'pollutionelasticityNACE1' = ZEW Method by construction and not cross-time estimation
(c) 'pollutionelasticityNACE2' = ZEW Method by estimation where for Output Energy Elasticity dependent ln Real Output
(d) 'pollutionelasticityNACE3' = ZEW Method by estimation where for Output Energy Elasticity dependent ln Energy Share
(e) 'pollutionelasticityISIC2' = ZEW Method by estimation where for Output Energy Elasticity dependent ln Real Output
(f) 'pollutionelasticityISIC3' = ZEW Method by estimation where for Output Energy Elasticity dependent ln Energy Share

The input for this script is the data 'dta_analysis.rds' from the script 
'Import_Merge.R'.

The output for this script are Table X and X.

Attention: Variable 'ghg' must be the same in all Scripts
"

# Install & Load Packages
{
  #install.packages("tidyverse")
  #install.packages("xtable")
  
  library(tidyverse)
  library(xtable)
}

# Load Data
{
  dta_analysis <- readRDS("./Data/dta_analysis.rds")
}

# Define variables for flexibility in Code
ghg <- 'CO2Total' # Greenhouse gas for the analysis to allow flexibility in choice
costs <- 'realexpend' # Define column used as Costs for calculations
alpha <- 0.011 # mean Pollution elasticity
base_year <- 2003 # Base year for parameter
end_year <- 2016 # End year to define time sequence under observation

# Elasticity Estimation for by each sector
{
  # Create Data with parameters, where Pollution Elasticity by Scaling and ZEW-method non-estimation
  {
    "
  Note: Greenhouse gases are in 1,000 tonnes, whereas output and costs data is in million DKK
  "
    dta_inter <- dta_analysis %>%
      filter(NACE_Name != 'Total Manufacturing' | ISIC_Name != 'Total Manufacturing') %>%
      filter(year >= base_year & year <= end_year) %>%
      
      # ISIC: Calculate Parameters with Shapiro and Walker(2018) Estimation method
      group_by(year, classsystem) %>%
      mutate(tonsPollCost = (!!sym(ghg)*1000)/!!sym(costs), # Calculating tons pollution per dollar costs
             meantonsPollCost = sum(tonsPollCost, na.rm=TRUE)/ifelse(classsystem=='NACE', 11, 16),
             pollutionelasticityISIC = alpha * (tonsPollCost/meantonsPollCost), # Calculating the Pollution elasticity with the mean alpha defined prior
             inputshare = !!sym(costs)/realoutput, # Calculating the input share with costs defined prior
             elasticitysubstitution = 1/(1-inputshare)) %>% # Calculating the Elasticity of Substitution by taking the ratio of the value of shipments to production costs, hence relying on assumption that firms engage in monopolistic competition
      ungroup() %>%
      
      # NACE_1: Calculate Pollution Elasticity with ZEW (2023) Estimation Method
      group_by(NACE_Name, ISIC_Name) %>%
      arrange(year, .by_group = TRUE) %>%
      mutate(lnrealoutput = log(realoutput),
             lnrealcostEnergy = log(realcostEnergy),
             lnghg = log(!!sym(ghg)),
             energyshare = realcostEnergy/realoutput,
             lnenergyshare = log(energyshare),
             
             chngOutputEnergyNACE = (lead(lnrealcostEnergy) - lnrealcostEnergy)/(lead(energyshare) - energyshare),
             elasticityOutputEnergyNACE1 = (1/chngOutputEnergyNACE) * (lnrealcostEnergy/energyshare),
             
             chngEmissionEnergyNACE = (lead(lnrealcostEnergy) - lnrealcostEnergy)/(lead(lnghg) - !lnghg),
             elasticityEmissionEnergyNACE1 = (1/chngEmissionEnergyNACE) * (lnrealcostEnergy/lnghg),
             pollutionelasticityNACE1 = elasticityOutputEnergyNACE1/elasticityEmissionEnergyNACE1) %>%
      ungroup()
    
    # Test whether pollution Elasticity has been correctly calculated
    mean((dta_inter %>% filter(year == base_year & classsystem=='NACE'))$pollutionelasticityISIC)
    mean((dta_inter %>% filter(year == base_year & classsystem=='ISIC'))$pollutionelasticityISIC)
    
    # Add Label to column 'tonspollcost'
    attr(dta_inter$tonsPollCost, 'label') <- 'Tons pollution per 1,000,000 DKK'
  }
  
  # Estimating Pollution Elasticity (ZEW-method)
  {
    # Create Dataframe
    {
      dta_elast <- data.frame(NACE_Name = character(0),
                              ISIC_Name = character(0),
                              
                              elasticityOutputEnergyNACE2 = numeric(0), 
                              elasticityEmissionEnergyNACE2 = numeric(0),
                              pollutionelasticityNACE2 = numeric(0),
                              elasticityOutputEnergyNACE3 = numeric(0),
                              pollutionelasticityNACE3 = numeric(0),
                              
                              elasticityOutputEnergyISIC2 = numeric(0), 
                              elasticityEmissionEnergyISIC2 = numeric(0),
                              pollutionelasticityISIC2 = numeric(0),
                              elasticityOutputEnergyISIC3 = numeric(0),
                              pollutionelasticityISIC3 = numeric(0),
                              
                              # Elasticities excl. 2008
                              elasticityOutputEnergyISICFinCri = numeric(0), 
                              elasticityEmissionEnergyISICFinCri = numeric(0),
                              pollutionelasticityISICFinCri = numeric(0),
                              
                              elasticityOutputEnergyNACEFinCri = numeric(0), 
                              elasticityEmissionEnergyNACEFinCri = numeric(0),
                              pollutionelasticityNACEFinCri = numeric(0))
    }
    
    # NACE: Estimation Loop for Pollution elasticity 
    for(i in na.omit(unique(dta_inter$NACE_Name))){
      dta_model <- dta_inter %>%
        filter(NACE_Name == i & !is.na(NACE_Name) & year <= end_year)
      
      # Run Estimations
      model_test_OutputEnergyNACE2 <- lm(lnrealoutput ~ lnrealcostEnergy, data = dta_model)
      model_test_OutputEnergyNACE3 <- lm(energyshare ~ lnrealcostEnergy, data = dta_model)
      model_test_EmissionsEnergyNACE2 <- lm(lnghg ~ lnrealcostEnergy, data = dta_model)
      # excl. Financial crisis(2008)
      model_OutputEnergyNACEFinCri <- lm(energyshare ~ lnrealcostEnergy,
                                         data = (dta_model %>% filter(year != 2008)))
      model_EmissionsEnergyNACEFinCri <- lm(lnghg ~ lnrealcostEnergy,
                                            data = (dta_model %>% filter(year != 2008)))
      
      # Extract Beta 1 from Models
      elasticityOutputEnergyNACE2 <- coef(model_test_OutputEnergyNACE2)[2]
      elasticityOutputEnergyNACE3 <- coef(model_test_OutputEnergyNACE3)[2]
      elasticityEmissionEnergyNACE2 <- coef(model_test_EmissionsEnergyNACE2)[2]
      elasticityOutputEnergyNACEFinCri <- coef(model_OutputEnergyNACEFinCri)[2]
      elasticityEmissionEnergyNACEFinCri <- coef(model_EmissionsEnergyNACEFinCri)[2]
      
      # Calculate Pollution Elasticity from estimated elasticities (beta 1)
      pollutionelasticityNACE2 = elasticityOutputEnergyNACE2/elasticityEmissionEnergyNACE2
      pollutionelasticityNACE3 = elasticityOutputEnergyNACE3/elasticityEmissionEnergyNACE2
      pollutionelasticityNACEFinCri = elasticityOutputEnergyNACEFinCri/elasticityEmissionEnergyNACEFinCri
      
      # Add estimations to Model
      dta_elast <- dta_elast %>%
        add_row(NACE_Name = i,
                elasticityOutputEnergyNACE2 = elasticityOutputEnergyNACE2,
                elasticityOutputEnergyNACE3 = elasticityOutputEnergyNACE3,
                elasticityEmissionEnergyNACE2 = elasticityEmissionEnergyNACE2,
                pollutionelasticityNACE2 = pollutionelasticityNACE2,
                pollutionelasticityNACE3 = pollutionelasticityNACE3,
                elasticityOutputEnergyNACEFinCri = elasticityOutputEnergyNACEFinCri, 
                elasticityEmissionEnergyNACEFinCri = elasticityEmissionEnergyNACEFinCri,
                pollutionelasticityNACEFinCri = pollutionelasticityNACEFinCri)
    }
    
    # ISIC: Estimation Loop for Pollution elasticity 
    for(i in na.omit(unique(dta_inter$ISIC_Name))){
      dta_model <- dta_inter %>%
        filter(ISIC_Name == i & !is.na(ISIC_Name) & year <= end_year)
      
      # Run Estimations
      model_test_OutputEnergyISIC2 <- lm(lnrealoutput ~ lnrealcostEnergy, data = dta_model)
      model_test_OutputEnergyISIC3 <- lm(energyshare ~ lnrealcostEnergy, data = dta_model)
      model_test_EmissionsEnergyISIC2 <- lm(lnghg ~ lnrealcostEnergy, data = dta_model)
      # excl. Financial crisis(2008)
      model_OutputEnergyISICFinCri <- lm(energyshare ~ lnrealcostEnergy,
                                         data = (dta_model %>% filter(year != 2008)))
      model_EmissionsEnergyISICFinCri <- lm(lnghg ~ lnrealcostEnergy,
                                            data = (dta_model %>% filter(year != 2008)))
      
      # Extract Beta 1 from Models
      elasticityOutputEnergyISIC2 <- coef(model_test_OutputEnergyISIC2)[2]
      elasticityOutputEnergyISIC3 <- coef(model_test_OutputEnergyISIC3)[2]
      elasticityEmissionEnergyISIC2 <- coef(model_test_EmissionsEnergyISIC2)[2]
      elasticityOutputEnergyISICFinCri <- coef(model_OutputEnergyISICFinCri)[2]
      elasticityEmissionEnergyISICFinCri <- coef(model_EmissionsEnergyISICFinCri)[2]
      
      # Calculate Pollution Elasticity from estimated elasticities (beta 1)
      pollutionelasticityISIC2 = elasticityOutputEnergyISIC2/elasticityEmissionEnergyISIC2
      pollutionelasticityISIC3 = elasticityOutputEnergyISIC3/elasticityEmissionEnergyISIC2
      pollutionelasticityISICFinCri = elasticityOutputEnergyISICFinCri/elasticityEmissionEnergyISICFinCri
      
      # Add estimations to Model
      dta_elast <- dta_elast %>%
        add_row(ISIC_Name = i,
                elasticityOutputEnergyISIC2 = elasticityOutputEnergyISIC2,
                elasticityOutputEnergyISIC3 = elasticityOutputEnergyISIC3,
                elasticityEmissionEnergyISIC2 = elasticityEmissionEnergyISIC2,
                pollutionelasticityISIC2 = pollutionelasticityISIC2,
                pollutionelasticityISIC3 = pollutionelasticityISIC3,
                elasticityOutputEnergyISICFinCri = elasticityOutputEnergyISICFinCri,
                elasticityEmissionEnergyISICFinCri = elasticityEmissionEnergyISICFinCri,
                pollutionelasticityISICFinCri = pollutionelasticityISICFinCri)
    }
  }
}

# Elasticity Estimation for 'Total Manufacturing
{
  # Create Data with parameters, where Pollution Elasticity by Scaling and ZEW-method non-estimation
  {
    "
  Note: Greenhouse gases are in 1,000 tonnes, whereas output and costs data is in million DKK
  "
    dta_total <- dta_analysis %>%
      filter(NACE_Name == 'Total Manufacturing' & year >= base_year & year <= end_year) %>%
      
      # ISIC: Calculate Parameters with Shapiro and Walker(2018) Estimation method
      mutate(ISIC_Name = 'Total Manufacturing',
             tonsPollCost = (!!sym(ghg)*1000)/!!sym(costs), # Calculating tons pollution per dollar costs
             meantonsPollCost = tonsPollCost/ifelse(classsystem=='NACE', 11, 16),
             pollutionelasticityISIC = alpha, # Calculating the Pollution elasticity with the mean alpha defined prior
             inputshare = !!sym(costs)/realoutput, # Calculating the input share with costs defined prior
             elasticitysubstitution = 1/(1-inputshare)) %>% # Calculating the Elasticity of Substitution by taking the ratio of the value of shipments to production costs, hence relying on assumption that firms engage in monopolistic competition
      
      # NACE_1: Calculate Pollution Elasticity with ZEW (2023) Estimation Method
      arrange(year, .by_group = TRUE) %>%
      mutate(lnrealoutput = log(realoutput),
             lnrealcostEnergy = log(realcostEnergy),
             lnghg = log(!!sym(ghg)),
             energyshare = realcostEnergy/realoutput,
             lnenergyshare = log(energyshare),
             
             chngOutputEnergyNACE = (lead(lnrealcostEnergy) - lnrealcostEnergy)/(lead(energyshare) - energyshare),
             elasticityOutputEnergyNACE1 = (1/chngOutputEnergyNACE) * (lnrealcostEnergy/energyshare),
             
             chngEmissionEnergyNACE = (lead(lnrealcostEnergy) - lnrealcostEnergy)/(lead(lnghg) - !lnghg),
             elasticityEmissionEnergyNACE1 = (1/chngEmissionEnergyNACE) * (lnrealcostEnergy/lnghg),
             pollutionelasticityNACE1 = elasticityOutputEnergyNACE1/elasticityEmissionEnergyNACE1)
    
    # Test whether pollution Elasticity has been correctly calculated
    mean(dta_total$pollutionelasticityISIC)
    
    # Add Label to column 'tonspollcost'
    attr(dta_inter$tonsPollCost, 'label') <- 'Tons pollution per 1,000,000 DKK'
  }
  
  # Estimating Pollution Elasticity (ZEW-method)
  {
    # Create Dataframe
    {
      dta_elast_tot <- data.frame(NACE_Name = character(0),
                              ISIC_Name = character(0),
                              
                              elasticityOutputEnergyNACE2 = numeric(0), 
                              elasticityEmissionEnergyNACE2 = numeric(0),
                              pollutionelasticityNACE2 = numeric(0),
                              elasticityOutputEnergyNACE3 = numeric(0),
                              pollutionelasticityNACE3 = numeric(0),
                              
                              elasticityOutputEnergyISIC2 = numeric(0), 
                              elasticityEmissionEnergyISIC2 = numeric(0),
                              pollutionelasticityISIC2 = numeric(0),
                              elasticityOutputEnergyISIC3 = numeric(0),
                              pollutionelasticityISIC3 = numeric(0),
                              
                              # Elasticities excl. 2008
                              elasticityOutputEnergyISICFinCri = numeric(0), 
                              elasticityEmissionEnergyISICFinCri = numeric(0),
                              pollutionelasticityISICFinCri = numeric(0),
                              
                              elasticityOutputEnergyNACEFinCri = numeric(0), 
                              elasticityEmissionEnergyNACEFinCri = numeric(0),
                              pollutionelasticityNACEFinCri = numeric(0))
    }
    
    # NACE: Estimation
    {
      # Run Estimations
      model_test_OutputEnergyNACE2 <- lm(lnrealoutput ~ lnrealcostEnergy, data = dta_total)
      model_test_OutputEnergyNACE3 <- lm(energyshare ~ lnrealcostEnergy, data = dta_total)
      model_test_EmissionsEnergyNACE2 <- lm(lnghg ~ lnrealcostEnergy, data = dta_total)
      model_OutputEnergyNACEFinCri <- lm(energyshare ~ lnrealcostEnergy,
                                              data = (dta_total %>% filter(year != 2008)))
      model_EmissionsEnergyNACEFinCri <- lm(lnghg ~ lnrealcostEnergy,
                                                 data = (dta_total %>% filter(year != 2008)))
      
      # Extract Beta 1 from Models
      elasticityOutputEnergyNACE2 <- coef(model_test_OutputEnergyNACE2)[2]
      elasticityOutputEnergyNACE3 <- coef(model_test_OutputEnergyNACE3)[2]
      elasticityEmissionEnergyNACE2 <- coef(model_test_EmissionsEnergyNACE2)[2]
      elasticityOutputEnergyNACEFinCri <- coef(model_OutputEnergyNACEFinCri)[2]
      elasticityEmissionEnergyNACEFinCri <- coef(model_EmissionsEnergyNACEFinCri)[2]
      
      # Calculate Pollution Elasticity from estimated elasticities (beta 1)
      pollutionelasticityNACE2 = elasticityOutputEnergyNACE2/elasticityEmissionEnergyNACE2
      pollutionelasticityNACE3 = elasticityOutputEnergyNACE3/elasticityEmissionEnergyNACE2
      pollutionelasticityNACEFinCri = elasticityOutputEnergyNACEFinCri/elasticityEmissionEnergyNACEFinCri
      
      # Add estimations to Model
      dta_elast_tot <- dta_elast_tot %>%
        add_row(NACE_Name = 'Total Manufacturing',
                ISIC_Name = 'Total Manufacturing',
                elasticityOutputEnergyNACE2 = elasticityOutputEnergyNACE2,
                elasticityOutputEnergyNACE3 = elasticityOutputEnergyNACE3,
                elasticityEmissionEnergyNACE2 = elasticityEmissionEnergyNACE2,
                pollutionelasticityNACE2 = pollutionelasticityNACE2,
                pollutionelasticityNACE3 = pollutionelasticityNACE3,
                
                elasticityOutputEnergyISIC2 = elasticityOutputEnergyNACE2,
                elasticityOutputEnergyISIC3 = elasticityOutputEnergyNACE3,
                elasticityEmissionEnergyISIC2 = elasticityEmissionEnergyNACE2,
                pollutionelasticityISIC2 = pollutionelasticityNACE2,
                pollutionelasticityISIC3 = pollutionelasticityNACE3,
                
                elasticityOutputEnergyISICFinCri = elasticityOutputEnergyNACEFinCri, 
                elasticityEmissionEnergyISICFinCri = elasticityEmissionEnergyNACEFinCri,
                pollutionelasticityISICFinCri = pollutionelasticityNACEFinCri,
                
                elasticityOutputEnergyNACEFinCri = elasticityOutputEnergyNACEFinCri, 
                elasticityEmissionEnergyNACEFinCri = elasticityEmissionEnergyNACEFinCri,
                pollutionelasticityNACEFinCri = pollutionelasticityNACEFinCri)
    }
  }
}

# Add 'dta_elast' to 'dta_inter' to create 'dta_parameter and save it
{
  dta_parameter <- rbind((dta_inter %>%
                            left_join(dta_elast, join_by(NACE_Name == NACE_Name, ISIC_Name == ISIC_Name))),
                         (dta_total %>%
      left_join(dta_elast_tot, join_by(NACE_Name == NACE_Name, ISIC_Name == ISIC_Name))))
    
  
  saveRDS(dta_parameter, file = "./Data/dta_parameter.rds")
}

# Create Dataframes with U.S. (Shapiro and Walker), and Germany (ZEW) values for pollution elasticity
{
  # U.S. Dataframe
  # Does not include sector 'Radio, television, communication' as its not distinguishable in Danish Classification
  dta_US <- data.frame(ISIC_Name = c("Basic metals", "Chemicals", "Coke, refined petroleum, fuels",
                                     "Fabricated metals", "Food, beverages, tobacco", "Furniture, other, recycling",
                                     "Machinery and equipment", "Medical, precision, and optical", "Motor vehicles, trailers",
                                     "Office, computing, electrical", "Other non-metallic minerals", "Other transport equipment",
                                     "Paper and publishing", "Rubber and plastics", "Textiles, apparel, fur, leather",
                                     "Wood products"),
                       NACE_Name = c("Basic Metals", "Chemicals and pharmaceuticals", "Coke, petroleum", 
                                     NA, "Food, beverages, tobacco", NA,
                                     "Metal products, electronics, machinery", NA, NA,
                                     NA, "Non-metallic minerals", "Vehicles, other transport, n.e.c.",                
                                     "Pulp, paper, publishing","Rubber and plastics", "Textiles, wearing apparel, leather",      
                                     "Wood products"),
                       USpollutionelasticity = c(0.0557, 0.0205, 0.0212, 0.0019, 
                                        0.0040, 0.0047, 0.0015, 0.0014, 
                                        0.0016, 0.0023, 0.0303, 0.0019, 
                                        0.0223, 0.0048, 0.0022, 0.0103),
                       USParetoShape = c(10.01, 3.50, 9.91,
                                         4.80, 3.89, 3.75,
                                         4.19, 2.86, 5.6,
                                         5.32, 4.05, 3.87,
                                         5.21, 4.62, 4.8,
                                         6.2),
                       USParetoSE = c(0.50, 0.08, 1.67,
                                      0.06, 0.13, 0.03,
                                      0.14, 0.06, 0.18,
                                      0.15, 0.11, 0.13,
                                      0.10, 0.08, 0.10,
                                      0.17))
  
  # Germany Dataframe
  dta_GER <- data.frame(NACE_Name = c("Basic Metals", "Chemicals and pharmaceuticals", "Coke, petroleum", 
                                      "Food, beverages, tobacco", "Metal products, electronics, machinery", "Non-metallic minerals",                 
                                      "Pulp, paper, publishing","Rubber and plastics", "Textiles, wearing apparel, leather", 
                                      "Vehicles, other transport, n.e.c.", "Wood products"),
                        ISIC_Name = c("Basic metals", "Chemicals", "Coke, refined petroleum, fuels",
                                      "Food, beverages, tobacco", "Machinery and equipment", "Other non-metallic minerals",
                                      "Paper and publishing", "Rubber and plastics", "Textiles, apparel, fur, leather",
                                      "Other transport equipment", "Wood products"),
                        GERenergyOutputElasticity = c(0.061, 0.034, 0.007, 0.016,
                                                      0.010, 0.065, 0.059, 0.022,
                                                      0.018, 0.008, 0.031),
                        GERenergyEmissionElasticity = c(0.993, 0.993, 1.001, 0.974,
                                                        1.011, 0.946, 0.962, 1.012,
                                                        1.002, 1.001, 0.873),
                        GERpollutionelasticity = c(0.063, 0.041, 0.009, 0.020,
                                                   0.010, 0.078, 0.058, 0.024,
                                                   0.019, 0.008, 0.038),
                        GERParetoShape = c(8.187, 2.605, 0.797,
                                           2.102, 7.063, 6.841,
                                           16.871, 5.483, 7.124,
                                           5.147, 6.442))
}

# Table 2: Exporting all Parameters for base year to LATEX (NACE)
{
  # Create a Table for LATEX format
  table2 <- xtable(x = (dta_parameter %>%
                          filter(year == base_year & classsystem == 'NACE' & NACE_Name != 'Total Manufacturing') %>%
                          arrange(NACE_Code) %>%
                          left_join(dta_US %>% select(NACE_Name, USpollutionelasticity,
                                                      USParetoShape, USParetoSE),
                                    join_by(NACE_Name == NACE_Name)) %>%
                          select(NACE_Name, tonsPollCost,
                                 pollutionelasticityNACE3,
                                 inputshare, elasticitysubstitution,
                                 USParetoShape, USParetoSE)),
                   digits = c(2,2,2,4,2,2,2,2)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table2) <- c("Industry", "Tons pollution per m DKK costs",
                             "Pollution elasticity",
                             "Input Share", "Elasticity of Substitution",
                     "Pareto Shape Parameter", "Shape Parameter SE")
  
  # Print Parameters table in LATEX format
  print(table2, include.rownames=FALSE)
}

# Table 3: Exporting all Pollution Elasticities (NACE)
{
  # Create a Table for LATEX format
  table3 <- xtable(x = (dta_parameter %>%
                          filter(year == base_year & classsystem == 'NACE' & NACE_Name != 'Total Manufacturing') %>%
                          arrange(NACE_Code) %>%
                          left_join(dta_US %>% select(NACE_Name, USpollutionelasticity),
                                    join_by(NACE_Name == NACE_Name)) %>%
                          left_join(dta_GER %>% select(NACE_Name, GERpollutionelasticity),
                                    join_by(NACE_Name == NACE_Name)) %>%
                          select(NACE_Name, GERpollutionelasticity,
                                 pollutionelasticityNACE1,
                                 pollutionelasticityNACE2, pollutionelasticityNACE3,
                                 pollutionelasticityNACEFinCri)),
                   digits = c(2,2,4,4,4,4,4)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table3) <- c("Industry", "GER",
                     "ZEW calcul.",
                     "ZEW ln(Real Output)", "ZEW ln(Energy Share)",
                     "ln(Energy Share) excl FinCri")
  
  # Print Parameters table in LATEX format
  print(table3, include.rownames=FALSE)
}

# Table 4: Exporting Output Energy-/Emissions Energy/and Pollution elasticities from ZEW method to LATEX (NACE) OR Intermediate Results
{
  # Create a Table for LATEX format
  table4 <- xtable(x = (dta_parameter %>%
                          filter(year == base_year & classsystem == 'NACE' & NACE_Name != 'Total Manufacturing') %>%
                          arrange(NACE_Code) %>%
                          left_join(dta_GER, join_by(NACE_Name == NACE_Name)) %>%
                          select(NACE_Name, 
                                 GERenergyOutputElasticity, elasticityOutputEnergyNACE3,
                                 GERenergyEmissionElasticity, elasticityEmissionEnergyNACE2,
                                 GERpollutionelasticity, pollutionelasticityNACE3)),
                                digits = c(2,2,3,3,3,3,3,3)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table4) <- c("Industry", 
                     "Energy Output elasticity GER", "Energy Output elasticity DNK",
                     "Energy Emission elasticity GER", "Energy Emission elasticity DNK",
                     "Pollution elasticity GER", "Pollution elasticity DNK")
  
  # Print Parameters table in LATEX format
  print(table4 , include.rownames=FALSE)
}