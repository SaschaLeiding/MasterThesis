"
This Script is to construct the environmental regulation tax

The input for this script is the data 'dta_parameter.rds' from the script 
'Parameter_Estimates.R'.
"

# Install & Load Packages
{
  #install.packages("tidyverse")
  #install.packages("xtable")
  #install.packages("nleqslv")
  #install.packages("patchwork")
  
  library(tidyverse)
  library(xtable)
  library(nleqslv)
  library(patchwork)
}

# Load Data
{
  dta_parameter <- readRDS("./Data/dta_parameter.rds")
  #dta_internat <- readRDS("./Data/dta_internat.rds")
}

# Define variables for flexibility in Code
ghg <- 'CO2ElectricityHeat' # 'CO2Total' # Greenhouse gas for the analysis to allow flexibility in choice
alpha <- 0.011 # mean Pollution elasticity
base_year <- 2003 # Base year for parameter
end_year <- 2014

# Calculate shocks
{
  dta_shocks <- dta_parameter %>%
    distinct() %>%
    group_by(ISIC_Name, NACE_Name) %>%
    mutate(year = as.integer(year),
           chngfirmEntry = firmEntry/firmEntry[year==base_year],
           chngghg = !!sym(ghg)/(!!sym(ghg))[year==base_year],
           chngwages = wage_manuf/wage_manuf[year==base_year],
           envregulation =  ((chngfirmEntry*chngwages)/chngghg)*100) %>%
    #select(ISIC_Name, NACE_Name, year, realoutput, firmEntry, wage_manuf, CompEmployees, Employees,!!sym(ghg),
    #       envregulation) %>%
    ungroup()
}

# Plot with exogeneous wage & firm entry for Chemicals, Food, Electrical (ISIC) - 'Landscape' 8.00 x 6.00
{
  dta_env_plot_exg <- dta_shocks %>%
    filter(#NACE_Name == 'Total Manufacturing' &
          NACE_Name %in% c('Total Manufacturing',"Chemicals and pharmaceuticals",
                           "Food, beverages, tobacco", "Metal products, electronics, machinery") & 
             year >= base_year & year <= end_year) %>%
    select(NACE_Name, year, envregulation) 
  
  year_breaks <- seq(from=base_year, to=end_year, by = 2)
  lplot_env_exg <- ggplot(data = dta_env_plot_exg,
                      aes(x = year, y = envregulation, color = NACE_Name, group = NACE_Name)) +
    geom_line() +
    labs(#title = "Development of various Greenhouse Gas Emissions",
      x = "Year",
      y = paste0("Base ", base_year, " = 100"),
      color = NULL) +
    scale_x_continuous(breaks = year_breaks) +
    theme_classic() +
    theme(legend.position = c(.15, .78))
  lplot_env_exg
}

# Table 5: Exporting Chemical Industry values for Environmental Shocks
{
  # Create a Table for LATEX format
  table5 <- xtable(x = dta_shocks %>%
                     filter(ISIC_Name == "Total Manufacturing" & year >=base_year & year <= end_year) %>%
                     select(year, realoutput, !!sym(ghg), firmEntry,
                            CompEmployees, Employees, wage_manuf,
                            envregulation),
                   digits = c(2,2,0,0,0,0,0,4,0)) # Set number of decimals per column
  
  # Change Column Names in LATEX table
  names(table5) <- c("Year",
                     "Real Output", "Emissions","Firm Entry",
                     "Compensation", "Employees", "Wages",
                     "Environmental Regulation")
  
  # Print Parameters table in LATEX format
  print(table5, 
        include.rownames=FALSE, 
        format.args = list(big.mark = ",", decimal.mark = "."))
}

# Merge final data
{
  #dta_policy <- dta_parameter %>%
  #  left_join(dta_shocks %>%
  #              mutate(year = as.character(year)) %>%
  #              select(ISIC_Name, NACE_Name, year, envregulation),
  #            join_by(ISIC_Name == ISIC_Name, NACE_Name == NACE_Name, year == year))
}

# Data for MATLAB version
{
  dta_MATLAB_l1_35 <- dta_parameter %>%
    filter(year <= end_year) %>%
    select(NACE_Name, NACE_Code, year, !!sym(ghg),
           EXP, DomDom, DomImp, ROWROW, vship,
           elasticitysubstitution, inputshare, pollutionelasticityNACE3, ParetoShape) %>%
    group_by(NACE_Name) %>%
    mutate(sigma= elasticitysubstitution[year==base_year], 
           alpha=pollutionelasticityNACE3[year==base_year],
           theta = ParetoShape[year==base_year]) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(vshipROW = ROWROW + DomImp,
           vshipDNK = DomDom + EXP,
           Eds_DNK = DomDom+DomImp,
           Eds_ROW = ROWROW+EXP,
           Rds_DNK = DomDom+EXP,
           Rds_ROW = ROWROW+DomImp,
           beta_DNK = Eds_DNK/Eds_DNK[NACE_Name == "Total Manufacturing"],
           beta_ROW = Eds_ROW/Eds_ROW[NACE_Name == "Total Manufacturing"],
           
           lambda_DomDom = DomDom/(Eds_DNK),
           lambda_DomImp = DomImp/(Eds_DNK),
           lambda_EXP = EXP/(Eds_ROW),
           lambda_ROWROW = ROWROW/(Eds_ROW),
           
           zeta_DomDom = DomDom/(Rds_DNK),
           zeta_DomImp = DomImp/(Rds_ROW),
           zeta_EXP = EXP/(Rds_DNK),
           zeta_ROWROW = ROWROW/(Rds_ROW),
           
           Ed_DNK = DomDom[NACE_Name == 'Total Manufacturing'] +
             DomImp[NACE_Name == 'Total Manufacturing'],
           Ed_ROW = ROWROW[NACE_Name == 'Total Manufacturing'] +
             EXP[NACE_Name == 'Total Manufacturing'],
           Rd_DNK = DomDom[NACE_Name == 'Total Manufacturing'] +
             EXP[NACE_Name == 'Total Manufacturing'],
           Rd_ROW = ROWROW[NACE_Name == 'Total Manufacturing'] +
             DomImp[NACE_Name == 'Total Manufacturing'],
           NXds_DNK = Rds_DNK - Eds_DNK,
           NXds_ROW = Rds_ROW - Eds_ROW,
           NXd_DNK = Rd_DNK - Ed_DNK,
           NXd_ROW = Rd_ROW - Ed_ROW,
           
           NXAds_DNK = sum(NXds_DNK*(sigma-1) * (theta-alpha+1) / (theta*sigma), na.rm=TRUE),
           NXAds_ROW = sum(NXds_ROW*(sigma-1) * (theta-alpha+1) / (theta*sigma), na.rm=TRUE))
  
  # retrieve base things
  dta_MATLAB_l41_48 <- dta_MATLAB_l1_35 %>%
    group_by(NACE_Name)
  
  dta_MATLAB_l54_63 <- dta_MATLAB_l1_35 %>%
    filter(NACE_Name != 'Total Manufacturing') %>%
    group_by(year) %>%
    mutate(w_base_DNK = sum(vshipDNK),
           w_base_ROW = sum(vshipROW)) %>%
    ungroup() %>%
    mutate(w_hat_DNK = w_base_DNK / w_base_DNK[year == base_year],
           w_hat_ROW = w_base_ROW / w_base_ROW[year == base_year]) %>%
    group_by(NACE_Name) %>%
    mutate(lambda_hat_DomDom = lambda_DomDom / lambda_DomDom[year == base_year],
           lambda_hat_DomImp = lambda_DomImp / lambda_DomImp[year == base_year],
           lambda_hat_EXP = lambda_EXP / lambda_EXP[year == base_year],
           lambda_hat_ROWROW = lambda_ROWROW / lambda_ROWROW[year == base_year],
           shocks.beta_hat_DNK = beta_DNK / beta_DNK[year == base_year],
           shocks.beta_hat_ROW = beta_ROW / beta_ROW[year == base_year],
           Z_hat = !!sym(ghg) / (!!sym(ghg))[year == base_year],
           Rds_hat_DNK = Rds_DNK / Rds_DNK[year == base_year],
           Rds_hat_ROW = Rds_ROW / Rds_ROW[year == base_year],
           shocks.NXd_hat_DNK = NXd_DNK / NXd_DNK[year == base_year],
           shocks.NXd_hat_ROW = NXd_ROW / NXd_ROW[year == base_year],
           shocks.NXAds_hat_DNK = NXAds_DNK / NXAds_DNK[year == base_year],
           shocks.NXAds_hat_ROW = NXAds_ROW / NXAds_ROW[year == base_year],
           
           # Matlab Line 62 & 63
           #w_oNJY_DNK = w_hat_DNK, # not included because in MATLAB only because of formatting
           #w_oNJY_ROW = w_hat_ROW, # not included because in MATLAB only because of formatting
           M_hat_DNK = Rds_hat_DNK / w_hat_DNK,
           M_hat_ROW = Rds_hat_ROW / w_hat_ROW) %>%
    ungroup()
  
  dta_MATLAB_l66_81 <- dta_MATLAB_l54_63 %>%
    mutate(pwrE = 1-theta / ((sigma-1)*(1-alpha))) %>%
    group_by(NACE_Name) %>%
    # Create MATLAB: 'shocks.Gamma_hat_star'
    mutate(shocks.Gamma_hat_star_DomDom = (lambda_hat_DomDom/(M_hat_DNK * (w_hat_DNK^(-theta)))) *
             (((shocks.beta_hat_DNK/w_hat_DNK) * (Rd_DNK-NXd_DNK) /
             (Rd_DNK[year == base_year] - NXd_DNK[year == base_year]))^pwrE),
           
           shocks.Gamma_hat_star_EXP = (lambda_hat_EXP/(M_hat_DNK * (w_hat_DNK^(-theta)))) *
             (((shocks.beta_hat_ROW/w_hat_ROW) * (Rd_ROW-NXd_ROW) /
                (Rd_ROW[year == base_year] - NXd_ROW[year == base_year]))^pwrE),
           
           shocks.Gamma_hat_star_DomImp = (lambda_hat_DomImp/(M_hat_ROW * (w_hat_ROW^(-theta)))) *
             (((shocks.beta_hat_DNK/w_hat_DNK) * (Rd_DNK-NXd_DNK) /
                 (Rd_DNK[year == base_year] - NXd_DNK[year == base_year]))^pwrE),
           
           shocks.Gamma_hat_star_ROWROW = (lambda_hat_ROWROW/(M_hat_ROW * (w_hat_ROW^(-theta)))) *
             (((shocks.beta_hat_ROW/w_hat_ROW) * (Rd_ROW-NXd_ROW) /
                 (Rd_ROW[year == base_year] - NXd_ROW[year == base_year]))^pwrE),
           
           t_hat = (M_hat_DNK * w_hat_DNK)/Z_hat,
           
           shocks.Gamma_hat_t = t_hat^((-alpha*theta)/(1-alpha)),
           shocks.Gamma_hat_foreign_DomDom = 1,
           shocks.Gamma_hat_foreign_EXP = 1,
           shocks.Gamma_hat_foreign_DomImp = shocks.Gamma_hat_star_DomImp,
           shocks.Gamma_hat_foreign_ROWROW = shocks.Gamma_hat_star_ROWROW,
           
           shocks.Gamma_hat_domestic_DomDom = shocks.Gamma_hat_star_DomDom/shocks.Gamma_hat_t,
           shocks.Gamma_hat_domestic_EXP = shocks.Gamma_hat_star_EXP/shocks.Gamma_hat_t,
           shocks.Gamma_hat_domestic_DomImp = NA,
           shocks.Gamma_hat_domestic_ROWROW = NA)
  
  #wwM_hat = w_hat_DNK # Matrix where the first row is w_hat_DNK and the rest M_hat in general
                        # second row the values for ROW industry 1
                        # third row the values for DNK industry 1
                        # fourth row the values for ROW industry 2
                        # fifth row the values for DNK industry 1
                      #...
                        # 34th row the values for ROW industry 17
                        # 35th row the values for DNK industry 17
  # takes both M_hat (ROW and DNK)
    
}

# TEST environment for "nleqslv"
{
  # Defining Multivariate function to solve for
  fn <- function(x) {
    
    rate <- x[1] * x[2] - 5
    shape <- sqrt(x[1] * x[2]^2) - 10
    testyear <- base_year+1
    
    return(c(rate, testyear))
  }
  
  nleqslv(x=c(0.75, 1.25), # numeric vector with an initial guess of the root of the function # Shapiro choose 0.75 until 1.25
          fn=fn, #A function of x returning a vector of function values with the same length as the  vector x.
          global = 'pwldog') 
}

# create wwM_hat & base year dataframe
{
  w_hat <- dta_MATLAB_l66_81 %>% 
    filter(NACE_Name == 'Basic Metals') %>%
    select(year, w_hat_DNK) %>%
    pivot_wider(names_from = year, values_from = w_hat_DNK) %>%
    mutate(NACE_Name = 'w_hat', country = 'DNK')%>%
    ungroup()
  
  M_hat <- dta_MATLAB_l66_81 %>%
    select(NACE_Name, year, M_hat_DNK, M_hat_ROW) %>%
    pivot_longer(cols = c(M_hat_DNK, M_hat_ROW), names_to = "country", values_to = "M_hat") %>%
    mutate(country = ifelse(country == "M_hat_DNK", "DNK", "ROW")) %>% 
    arrange(NACE_Name, year, desc(country)) %>%
    pivot_wider(names_from = year, values_from = M_hat)%>%
    ungroup()
  
  wwM_hat <- rbind(w_hat, M_hat) %>%
    ungroup()
  
  dta_base <- dta_MATLAB_l66_81 %>% filter(year == base_year)
}

# Apply algorithm to "nleqslv"
{
  p4 <- function() {
    # Define datasets as 1 to calculate shocks
    NXd_hat <- dta_MATLAB_l66_81 %>%
      filter(year == n) %>% select(NACE_Name, year) %>%
      mutate(NXd_hat_DNK = 1, NXd_hat_ROW = 1)
    NXAds_hat <- dta_MATLAB_l66_81 %>%
      filter(year == n) %>% select(NACE_Name, year) %>%
      mutate(NXAds_hat_DNK = 1, NXAds_hat_ROW = 1)
    
    ## HOW TO BE ADJUSTED???
    Gamma_hat <- dta_MATLAB_l66_81 %>%
      filter(year == n) %>% select(NACE_Name, year) %>%
      mutate(ones = 1)
    
    beta_hat <- dta_MATLAB_l66_81 %>%
      filter(year == n) %>% select(NACE_Name, year) %>%
      mutate(beta_hat_DNK = 1, beta_hat_ROW = 1)
    
    # Change datasets depending on what shock to calculate
    if (loop_shock == 1){
      Gamma_hat <-  dta_MATLAB_l66_81 %>%
        filter(year == n) %>%
        select(NACE_Name, year, starts_with('shocks.Gamma_hat_foreign'))
    }
    if (loop_shock == 2){
      Gamma_hat <-  dta_MATLAB_l66_81 %>%
        filter(year == n) %>%
        select(NACE_Name, year, starts_with('shocks.Gamma_hat_domestic'))
    }
    if (loop_shock == 3){
      Gamma_hat <-  dta_MATLAB_l66_81 %>%
        filter(year == n) %>%
        select(NACE_Name, year, starts_with("shocks.Gamma_hat_t"))
    }
    if (loop_shock == 4){
      beta_hat <-  dta_MATLAB_l66_81 %>%
        filter(year == n) %>%
        select(NACE_Name, year, starts_with("shocks.beta_hat_")) %>%
        rename(beta_hat_DNK = shocks.beta_hat_DNK,
               beta_hat_ROW = shocks.beta_hat_ROW)
    }
    if (loop_shock == 5){
      NXd_hat <-  dta_MATLAB_l66_81 %>%
        filter(year == n) %>%
        select(NACE_Name, year, starts_with("shocks.NXd_hat_")) %>%
        rename(NXd_hat_DNK = shocks.NXd_hat_DNK,
               NXd_hat_ROW = shocks.NXd_hat_ROW)
      
      NXAds_hat <-  dta_MATLAB_l66_81 %>%
        filter(year == n) %>%
        select(NACE_Name, year, starts_with("shocks.NXAds_hat_")) %>%
        rename(NXAds_hat_DNK = shocks.NXAds_hat_DNK,
               NNXAds_hat_ROW = shocks.NXAds_hat_ROW)
    }
    if (loop_shock == 6){
      Gamma_hat <-  dta_MATLAB_l66_81 %>%
        filter(year == n) %>%
        select(NACE_Name, year, starts_with('shocks.Gamma_hat_star'))
      
      beta_hat <-  dta_MATLAB_l66_81 %>%
        filter(year == n) %>%
        select(NACE_Name, year, starts_with("shocks.beta_hat_")) %>%
        rename(beta_hat_DNK = shocks.beta_hat_DNK,
               beta_hat_ROW = shocks.beta_hat_ROW)
      
      NXd_hat <-  dta_MATLAB_l66_81 %>%
        filter(year == n) %>%
        select(NACE_Name, year, starts_with("shocks.NXd_hat_"))%>%
        rename(NXd_hat_DNK = shocks.NXd_hat_DNK,
               NXd_hat_ROW = shocks.NXd_hat_ROW)
      
      NXAds_hat <-  dta_MATLAB_l66_81 %>%
        filter(year == n) %>%
        select(NACE_Name, year, starts_with("shocks.NXAds_hat_"))%>%
        rename(NXAds_hat_DNK = shocks.NXAds_hat_DNK,
               NNXAds_hat_ROW = shocks.NXAds_hat_ROW)
    }
    
    ww <- (wwM_hat %>% select(-NACE_Name, -country))[1:(N-1),]# either 1 or the whole w_hat
    M_hat <- wwM_hat[2:nrow(wwM_hat),]
    wA = (1- ww*sum(dta_base$Rds_DNK))/sum(dta_base$Rds_ROW)
    w_hat <- rbind(wA, ww)
    w_hat_long <- w_hat %>%
      mutate(country = ifelse(row_number() %% 2 == 0, "_DNK", "_ROW")) %>%
      pivot_longer(cols = 1:12, names_to = 'year') %>%
      pivot_wider(names_from = country,
                  names_prefix = 'w_hat',
                  values_from = value)
    
    M_oNNJY <- M_hat %>%# and take all year two times 2003...2014,2003...2014
      pivot_longer(cols = "2003":"2014", names_to = 'year') %>%
      pivot_wider(names_from = country,
                  names_prefix = 'M_hat_',
                  values_from = value)
    
    # ham - line 27
    {
      dta_ham <- Gamma_hat %>%
        left_join((w_hat %>%
                     mutate(country = ifelse(row_number() %% 2 == 0, "_DNK", "_ROW")) %>%
                     pivot_longer(cols = 1:12, names_to = 'year') %>%
                     pivot_wider(names_from = country,
                                 names_prefix = 'w_hat',
                                 values_from = value)),
                  join_by(year == year)) %>%
        left_join((dta_MATLAB_l66_81 %>%
                     select(NACE_Name, year, theta)),
                  join_by(year == year, NACE_Name == NACE_Name)) %>%
        ungroup()
      
      if (loop_shock %in% c(1,2,6)){
        ham <- dta_ham %>%
          mutate(across(ends_with("_DomDom"), ~ .x * (w_hat_DNK^(-theta)), .names = "ham_DomDom"),
                 across(ends_with("_EXP"), ~ .x * (w_hat_DNK^(-theta)), .names = "ham_EXP"),
                 across(ends_with("_DomImp"), ~ .x * (w_hat_ROW^(-theta)), .names = "ham_DomImp"),
                 across(ends_with("_ROWROW"), ~ .x * (w_hat_ROW^(-theta)), .names = "ham_ROWROW")) %>%
          ungroup() %>%
          select(NACE_Name, year, starts_with('ham'))
      } else {
        if (loop_shock == 3){
          ham <- dta_ham %>%
            mutate(ham_DomDom = shocks.Gamma_hat_t * (w_hat_DNK^(-theta)),
                   ham_EXP = shocks.Gamma_hat_t * (w_hat_DNK^(-theta)),
                   ham_DomImp = 1,
                   ham_ROWROW = 1) %>%
            select(NACE_Name, year, starts_with('ham'))
        } else{
          ham <- dta_MATLAB_l66_81 %>% 
            filter(year == n) %>% select(NACE_Name, year) %>% 
            mutate(ham_DomDom = 1,
                   ham_EXP = 1,
                   ham_DomImp = 1,
                   ham_ROWROW = 1)
        }
      }
    }
    
    # diff 1 - line 28
    {
      diff1 <- ham %>%
        left_join(NXd_hat, join_by(NACE_Name == NACE_Name, year == year)) %>%
        left_join(beta_hat, join_by(NACE_Name == NACE_Name, year == year)) %>%
        left_join(M_oNNJY, join_by(NACE_Name == NACE_Name, year == year)) %>%
        left_join(w_hat_long, join_by(year == year)) %>%
        
        left_join(dta_base %>% 
                    select(NACE_Name,
                           lambda_DomDom, lambda_DomImp, lambda_EXP, lambda_ROWROW,
                           Rds_DNK, Rds_ROW, NXd_DNK, NXd_ROW, Rd_DNK, Rd_ROW,
                           zeta_DomDom, zeta_DomImp, zeta_EXP, zeta_ROWROW),
                  join_by(NACE_Name == NACE_Name)) %>%
        
        mutate(denom_DomDom = lambda_DomDom*ham_DomDom*M_hat_DNK,
               denom_EXP = lambda_EXP*ham_EXP*M_hat_DNK,
               denom_DomImp = lambda_DomImp*ham_DomImp*M_hat_ROW,
               denom_ROWROW = lambda_ROWROW *ham_ROWROW*M_hat_ROW,
               nom_ROW = (w_hat_ROW*sum(Rds_ROW)-NXd_hat_ROW*NXd_ROW) / (Rd_ROW-NXd_ROW),
               nom_DNK = (w_hat_DNK*sum(Rds_DNK)-NXd_hat_DNK*NXd_DNK) / (Rd_DNK-NXd_DNK),
               prod_DomDom = zeta_DomDom * ham_DomDom * beta_hat_DNK,
               prod_DomImp = zeta_DomImp * ham_DomImp * beta_hat_DNK,
               prod_EXP = zeta_EXP * ham_EXP * beta_hat_ROW,
               prod_ROWROW = zeta_ROWROW * ham_ROWROW * beta_hat_ROW,
               X_DomDom = prod_DomDom * nom_DNK /denom_DomDom,
               X_EXP = prod_EXP * nom_DNK /denom_EXP,
               X_DomImp = prod_DomImp * nom_ROW /denom_DomImp,
               X_ROWROW = prod_ROWROW * nom_ROW /denom_ROWROW,
               X_DNK = X_DomDom + X_EXP,
               X_ROW = X_ROWROW + X_DomImp,
               diff1_DNK = w_hat_DNK - X_DNK,
               diff1_ROW = w_hat_ROW - X_ROW) %>%
        select(NACE_Name, year, diff1_DNK, diff1_ROW)
    }
}

{
  Y <- length(unique(dta_MATLAB_l66_81$year))
  N <- 2
  theta <- unique(dta_MATLAB_l66_81$theta)
  for (n in 1:Y) {
    for(loop_shock in 1:5){
      nleqslv(x=c(0.75, 1.25), # numeric vector with an initial guess of the root of the function # Shapiro choose 0.75 until 1.25
              fn=p4, #A function of x returning a vector of function values with the same length as the  vector x.
              global = 'pwldog')
    }
  }
}

# create t_hat for Total Manufacturing
{
  dta_env_Manuf <- dta_MATLAB_l66_81 %>%
    select(NACE_Name, year, t_hat) %>%
    group_by(year) %>%
    summarise(t_hat = mean(t_hat)) %>%
    mutate(NACE_Name = 'Total Manufacturing')
}

# Plot with Environmental regulation tax selected Industries - 'Landscape' 8.00 x 6.00
{
  dta_env_plot_end <- dta_MATLAB_l66_81 %>%
    filter(NACE_Name %in% c('Total Manufacturing',"Chemicals and pharmaceuticals",
                       "Food, beverages, tobacco", "Metal products, electronics, machinery") & 
        year >= base_year & year <= end_year) %>%
    select(NACE_Name, year, t_hat) %>%
    full_join(dta_env_Manuf) %>%
    group_by(year) %>%
    mutate(year = as.numeric(year),
           t_hat = t_hat * 100)
  
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

# Plot Endogeneous Firm Entry and Wages - 'Landscape' 11.00 - 7.00
{
  dta_endog_lplot <- dta_MATLAB_l66_81 %>%
    group_by(year) %>%
    mutate(avgM_hat_DNK = mean(M_hat_DNK),
           avgM_hat_ROW = mean(M_hat_ROW)) %>%
    select(NACE_Name, year, w_hat_DNK, w_hat_ROW, avgM_hat_DNK, avgM_hat_ROW) %>%
    filter(NACE_Name == "Basic Metals") %>% # arbitrarily chosen as all values are averages across sectors
    pivot_longer(cols = 3:ncol(.), names_to = "Variable", values_to = "Value" ) %>%
    mutate(country = ifelse(str_detect(Variable, "ROW$"), "ROW", "DNK"),
           Value = Value*100) %>%
    #mutate(across(is.numeric, ~.x*100)) %>%
    ungroup()
  
  plot_Wages <- ggplot(data = (dta_endog_lplot %>%
                                     filter(str_detect(Variable, "w_hat"))),
                           aes(x = year, y = Value, color = country, group = country)) +
    geom_line() +
    #scale_x_continuous(breaks = seq(2003, max(dta_endog_lplot$year, na.rm = TRUE), by = 1), limits = c(2003, NA)) +
    labs(x = "Year",
         y = paste0("Base ", base_year, " = 100"),
         color = NULL)+
    theme_classic()
  plot_Wages
  
  plot_FirmEntry <- ggplot(data = (dta_endog_lplot %>%
                                filter(str_detect(Variable, "M_hat"))),
                      aes(x = year, y = Value, color = country, group = country)) +
    geom_line() + 
    labs(x = "Year",
         y = paste0("Base ", base_year, " = 100"),
         color = NULL) +
    theme_classic()
  plot_FirmEntry
  
  # Plot in 2x1 - Export as Landscape as 11.00 - 7.00
  label_plots <- map(c("Wage", "Firm Entry"), 
                     ~ ggplot() +
                       theme_void() +
                       theme(plot.margin = margin(0, 0)) +
                       annotate("text", x = 0.5, y = 0.5, label = .x,
                                fontface = "bold", hjust = 0.5))
  
  
  combined_Endo_lplot <- (Reduce('+', label_plots) + plot_layout(ncol = 2)) /
   ( plot_Wages + plot_FirmEntry) +
    plot_layout(guides = 'collect', axis_titles = "collect", heights = c(1, 10)) &
  theme(legend.position = "bottom",
       legend.title = element_blank())
  combined_Endo_lplot
  
}

# Merge data
{
  dta_policy <- dta_parameter %>%
    left_join((dta_MATLAB_l66_81 %>%
                 select(NACE_Name, year, t_hat)), join_by(NACE_Name == NACE_Name, year == year))
}

# Save Data
{
  saveRDS(dta_shocks, file = "./Data/dta_shocks.rds")
  saveRDS(dta_policy, file = "./Data/dta_policy.rds")
}