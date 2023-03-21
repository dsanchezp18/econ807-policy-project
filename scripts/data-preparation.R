### ECON807 Policy Project
## SFU Spring 2023
# Daniel Sanchez

# This script prepares the data, by taking it in from different sources

# Preliminaries -------------------------------------------------------------------------------------------

# Load libraries

library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)

# Load the data

# Load all area codes as per the DPA 2022 (See data sources)

area_codes <-
  read_excel('data/CODIFICACION_2022.xlsx')

# Business creation data (Directorio de Compañías SCVS)

scvs_raw <-
  read_excel('data/directorio_companias.xlsx')

# Social security jobs data

iess_raw <-
  read.csv('data/social-security-data.csv')

# Formal jobs data (job contracts)

jobs_raw <-
  read.csv('data/contracts-sut.csv',
           sep = ";")

# Province populations

prov_pop <-
  read.csv('data/province-population.csv')

# Layoffs

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

# Do an alternative ordering, some have tildes, other don't

provinces_some_tildes <- c(
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
  'GALÁPAGOS',                     
  'SUCUMBIOS',                    
  'ORELLANA',                      
  'SANTO DOMINGO DE LOS TSÁCHILAS',
  'SANTA ELENA'
)

province_all_tildes_sd <- c(
  'AZUAY',                         
  'BOLÍVAR',                       
  'CAÑAR',                         
  'CARCHI',                      
  'COTOPAXI',                      
  'CHIMBORAZO',                    
  'EL ORO',                        
  'ESMERALDAS',                    
  'GUAYAS',                   
  'IMBABURA',                     
  'LOJA',                          
  'LOS RÍOS',                      
  'MANABí',                        
  'MORONA SANTIAGO',               
  'NAPO',              
  'PASTAZA',                       
  'PICHINCHA',                    
  'TUNGURAHUA',                   
  'ZAMORA CHINCHIPE',              
  'GALÁPAGOS',                     
  'SUCUMBÍOS',                    
  'ORELLANA',                      
  'SANTO DOMINGO',
  'SANTA ELENA'
)

province_codes <-
  province_codes %>% 
  mutate(province_no_tilde = provinces_no_tilde,
         province_some_tildes = provinces_some_tildes,
         province_all_tildes_sd  = province_all_tildes_sd)

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
         year = year(creation_date) %>% as.integer(),
         month_year = floor_date(creation_date, 'month')) %>% 
  select(-FECHA_CONSTITUCION)

df <-
  scvs %>%
  group_by(province_code, month_year) %>% 
  summarise(buss_new = n()) %>% 
  mutate(month = month(month_year),
         year = year(month_year))

# Province Populations ----------------------------------------------------

# Add province populationsto compute "per capita" indicators
# Need to include the correct province codes

prov_pop <-
  prov_pop %>% 
  left_join(province_codes %>% select(-province), 
            by = c('province' = 'province_all_tildes_sd'))

df <-
  df %>% 
  left_join(prov_pop %>% select(province_code, pop),
            by = 'province_code')

# Social Security Employment Registry data --------------------------------

# Wrangle to later join

iess <-
  iess_raw %>%
  select(-Descripcion) %>% 
  rename(province_code = 'Desagregaciones..Provincia.') %>% 
  filter(!(province_code %in% c('90', 'Z0_Nocla_ubicacion'))) %>% 
  pivot_longer(
    cols = -province_code,
    names_to = 'month_year',
    values_to = 'value'
  ) %>%
  mutate(year = paste('20', str_sub(month_year, -2, -1), sep = ''),
         month_str = str_sub(month_year,1,3),
         month_str = case_when(month_str == 'abr' ~ 'apr', 
                               month_str == 'ene' ~ 'jan',
                               month_str == 'ago' ~ 'aug',
                               month_str == 'dic' ~ 'dec',
                               TRUE ~ month_str),
         month_year = parse_date_time(paste0(month_str,year), orders = 'b Y') %>% ymd()) %>% 
  select(province_code, month_year, value)

df <-
  df %>% 
  left_join(iess %>% rename(jobs = 'value'), by = c('province_code', 'month_year'))

# Formal jobs data (SUT) -----------------------------------------------------------------------------------------------

# Clean the dataframe as is at the moment and add the province code

jobs <-
  jobs_raw %>%
  mutate(
    date = dmy(Fecha.Inicio),
    year = year(date),
    month = month(date),
    month_year = floor_date(date, 'month')
  ) %>% 
  rename(province = 'Provincia.Contrato..grupo.',
         contracts = 'Contratos') %>% 
  left_join(province_codes %>% select(-province, -province_no_tilde), 
            by = c('province' = 'province_some_tildes'))

# Group at the province level

jobs_province <-
  jobs %>% 
  group_by(province_code, month_year) %>% 
  summarise(contracts = sum(contracts))

# Join it to the main dataframe

df <-
  df %>% 
  left_join(jobs_province, by = c('province_code', 'month_year'))

# Running variable --------------------------------------------------------

# Define the running variable: number of months before the month of implementation. 

df <-
  df %>% 
  mutate(time = 12 * (as.yearmon(month_year) - as.yearmon(as.Date('2020-05-01'))))

# Export --------------------------------------------------------------------------------------------------

# Export the main dataframe to an RData object

save(df, file = 'data/df-main.RData')

# Export a reduced dataframe to an RData object, between 2018 and 2022

df %>% 
  filter(year %>% between(2018, 2022)) %>% 
  save(file = 'data/df18_22.RData')

