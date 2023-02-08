### ECON807 Policy Project
## SFU Spring 2023
# Daniel Sanchez

# This script prepares the data, by taking it in from different sources

# Preliminaries -------------------------------------------------------------------------------------------

# Load libraries

library(tidyverse)
library(readxl)
library(lubridate)

# Load the data

# Business creation data (Directorio de Compañías SCVS)

scvs_raw <-
  read_excel('data/directorio_companias.xlsx')

# Load all area codes as per the DPA 2022 (See data sources)

area_codes <-
  read_excel('data/CODIFICACION_2022.xlsx')

# Area Identifiers ----------------------------------------------------------------------------------------

# Ecuador has 24 provinces which each normally have an id code which is used across the public sector.

# I will create a catalogue which identifies the way each province is written with the main identifier.
 
province_codes <-
  area_codes %>%
  distinct(DPA_PROVIN, .keep_all = T) %>% 
  transmute(province_code = DPA_PROVIN,
            province = DPA_DESPRO)

# Now do the same thing but without the tildes for all of the provinces

provinces_no_tilde <- c(
  'AZUAY',                         
  'BOLIVAR',                       
  'CAÑAR',                         
  'CARCHI',                      
  'COTOPAXI',                      
  'CHIMBORAZO',                    
  'EL ORO',                        
  'ESMERALDAS',                    
  'GUAYAS',                   
  'IMBABURA',                     
  'LOJA',                          
  'LOS RIOS',                      
  'MANABI',                        
  'MORONA SANTIAGO',               
  'NAPO',              
  'PASTAZA',                       
  'PICHINCHA',                    
  'TUNGURAHUA',                   
  'ZAMORA CHINCHIPE',              
  'GALAPAGOS',                     
  'SUCUMBIOS',                    
  'ORELLANA',                      
  'SANTO DOMINGO DE LOS TSACHILAS',
  'SANTA ELENA'
)

province_codes <-
  province_codes %>% 
  mutate(province_no_tilde = provinces_no_tilde)

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

# Rename province variable, then left join for the province code.

scvs <-
  scvs %>% 
  rename(province = 'PROVINCIA') %>% 
  left_join(province_codes %>% select(-province), by = c('province' = 'province_no_tilde'))

# Create the month-year combination through lubridate in the scvs data and add month-year

scvs <-
  scvs %>% 
  mutate(creation_date = dmy(FECHA_CONSTITUCION),
         month = month(creation_date),
         year = year(creation_date),
         month_year = floor_date(creation_date, 'month') %>% format('%m-%Y')) %>% 
  select(-FECHA_CONSTITUCION)

df <-
  scvs %>%
  group_by(province_code, month_year) %>% 
  summarise(buss_new = n())
  



