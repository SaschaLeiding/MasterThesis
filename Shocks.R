"
This Script is to construct the environmental regulation tax

The input for this script is the data 'dta_parameter.rds' from the script 
'Parameter_Estimates.R'.
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
  dta_parameter <- readRDS("./Data/dta_parameter.rds")
}