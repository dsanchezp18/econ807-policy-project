### ECON807 Policy Project
## SFU Spring 2023
# Daniel Sanchez

# This script prepares the data, by taking it in from different sources

# Preliminaries -------------------------------------------------------------------------------------------

# Load libraries

library(tidyverse)
library(readxl)

# Load the data

# Business creation data (Directorio de Compañías SCVS)

scvs_raw <-
  read_excel('data/directorio_companias.xlsx')

# Business Creation data ----------------------------------------------------------------------------------

# The data is a list of all companies registered in the SCVS at any given point in time. 
# I took out this data in February 2nd 2023.
# I will acquire business creation by grouping the number of active companies by their creation date. 
# I clean the data below

# Eliminate the rows I don't want

scvs <-
  scvs_raw %>% 
  slice(-1:-3)

# First rows as headers

names(scvs) <- 
  scvs %>% 
  slice(1) %>% 
  unlist()

scvs <-
  scvs %>% 
  slice(-1)

# Now group by datae


