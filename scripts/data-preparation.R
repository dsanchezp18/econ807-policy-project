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

contracts_raw <-
  read.csv('data/contracts-sut.csv',
           sep = ';')

# Province populations

prov_pop <-
  read.csv('data/province-population.csv')

# Remote workers

remote_workers_raw <-
  read.csv('data/remote-workers.csv',
           sep = ';')

# Thefts

thefts_raw <-
  read_excel('data/thefts.xls',
             col_names = c('theft_type', 'province', 'canton', 'month_str', 'year', 'thefts'),
             skip = 4)

# Homicides

homicides_raw <-
  read_excel('data/homicides.xls',
             col_names = c('homicide_type', 'province', 'canton', 'month_str', 'year', 'weapon', 'age_range', 'sex', 'homicides'),
             skip = 4)

# COVID-19 cases

covid_cases_raw <-
  read.csv('data/covid-cases.csv', sep = ';')

# Internal Revenue Service (SRI) active tax-registered contributors

active_2022 <-
  read.csv('data/sri_activos_2022.csv', sep = '|')

active_2021 <-
  read.csv('data/sri_activos_2021.csv', sep = '|')

active_2020 <-
  read.csv('data/sri_activos_2020.csv', sep = ';')

active_2019 <-
  read.csv('data/sri_activos_2019.csv', sep = ';')

active_2018 <-
  read.csv('data/sri_activos_2018.csv', sep = ';')

active_2017 <-
  read.csv('data/sri_activos_2017.csv', sep = ';')

# IRS reported sales

sales_2022 <-
  read.csv('data/sri_ventas_2022.csv', sep = '|')

sales_2021 <-
  read.csv('data/sri_ventas_2021.csv', sep = '|')

sales_2020 <-
  read.csv('data/sri_ventas_2020.csv', sep = '|')

sales_2019 <-
  read.csv('data/sri_ventas_2019.csv', sep = '|')

sales_2018 <-
  read.csv('data/sri_ventas_2018.csv', sep = '|')

sales_2017 <-
  read.csv('data/sri_ventas_2017.csv', sep = ';', fileEncoding = 'Latin1')

# Transit accidents

transit_accidents_raw <-
  read.csv('data/transit-accidents.csv')
  
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

province_all_tildes_sdt <- c(
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
  'SANTO DOMINGO DE LOS TSÁCHILAS',
  'SANTA ELENA'
)

province_codes <-
  province_codes %>% 
  mutate(province_no_tilde = provinces_no_tilde,
         province_some_tildes = provinces_some_tildes,
         province_all_tildes_sd  = province_all_tildes_sd,
         province_all_tildes_sdt = province_all_tildes_sdt,
         province_code2 = case_when(
           as.numeric(province_code) <= 9 ~ str_sub(province_code, -1, -1),
           TRUE ~ province_code
         ))

# Months -------------------------------------------------------------------

# Create lookup tables for Spanish to English month names

month_lookup <- data.frame(
  spanish_month = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                    "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
  english_month = month.abb
)

month_lookup_tax_registry <- data.frame(
  spanish_month = c("01 Enero", "02 Febrero", "03 Marzo", "04 Abril", "05 Mayo", "06 Junio",
                    "07 Julio", "08 Agosto", "09 Septiembre", "10 Octubre", "11 Noviembre", "12 Diciembre"),
  english_month = month.abb
)

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
  left_join(province_codes %>% select(-province), 
            by = c('province' = 'province_no_tilde'))

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
  ungroup() %>% 
  mutate(month = month(month_year),
         year = year(month_year))

# I'd also like a week-level dataset

scvs_weekly <-
  scvs %>%
  group_by(week_year = paste0(year(creation_date), "-", week(creation_date))) %>%
  summarise(buss_new = n()) %>% 
  ungroup()

# Province Populations ----------------------------------------------------

# Add province populations to compute "per capita" indicators
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

contracts <-
  contracts_raw %>%
  mutate(
    date = dmy(Fecha.Inicio),
    year = year(date),
    month = month(date),
    month_year = floor_date(date, 'month')
  ) %>% 
  rename(province = 'Provincia.Contrato..grupo.',
         contracts = 'Contratos') %>% 
  left_join(province_codes %>% select(-province, -province_no_tilde, province_all_tildes_sd), 
            by = c('province' = 'province_some_tildes'))

# Group at the province level

contracts_province <-
  contracts %>% 
  group_by(province_code, month_year) %>% 
  summarise(contracts = sum(contracts)) %>% 
  ungroup()

# Join it to the main dataframe

df <-
  df %>% 
  left_join(contracts_province, 
            by = c('province_code', 'month_year'))

# Remote workers ----------------------------------------------------------

# Add this variable to the main dataframe, but first wrangle it a little bit

remote_workers <-
  remote_workers_raw %>% 
  rename(year = 'Anio.Fecha.Inicio.Teletrabajo',
         month = 'Mes.Fecha.Inicio.Teletrabajo',
         province = 'Provincia.Contrato..grupo.') %>% 
  mutate(month_year = make_date(year = year, month = month, day = 01)) %>% 
  filter(!is.na(month_year)) %>% 
  group_by(province, month_year) %>% 
  summarise(remote_workers = sum(Contratos)) %>% 
  ungroup() %>% 
  mutate(province = case_when(
    province == 'SANTO DOMINGO DE LOS TSÁCHILAS' ~ 'SANTO DOMINGO DE LOS TSACHILAS',
    province == 'GALÁPAGOS' ~ 'GALAPAGOS',
    TRUE ~ province
  )) %>%
  mutate(year = year(month_year),
         month = month(month_year)) %>% 
  left_join(province_codes %>% select(-province, province_some_tildes, province_all_tildes_sd), 
            by = c('province' = 'province_no_tilde'))

# Now left join to the actual dataset

df <-
  df %>% 
  left_join(remote_workers %>% select(province_code, month_year, remote_workers),
            by = c('province_code', 'month_year'))

# Thefts ------------------------------------------------------------------

# Prepare the thefts dataset for joining

thefts <-
  thefts_raw %>% 
  mutate(month_str = case_when(month_str == 'Abr' ~ 'apr', 
                               month_str == 'Ene' ~ 'jan',
                               month_str == 'Ago' ~ 'aug',
                               month_str == 'Dic' ~ 'dec',
                               TRUE ~ month_str)) %>% 
  filter(!is.na(month_str), !is.na(year)) %>% 
  mutate(month_year = parse_date_time(paste0(month_str,year), orders = 'b Y') %>% ymd()) %>%
  group_by(province, month_year) %>% 
  summarise(thefts = sum(thefts)) %>%
  ungroup() %>% 
  left_join(province_codes %>% select(province_all_tildes_sdt, province_code), 
            by = c('province' = 'province_all_tildes_sdt'))

# Join to the main df

df <-
  df %>% 
  left_join(thefts %>% select(-province), by = c('province_code', 'month_year'))


# Homicides ---------------------------------------------------------------

# Prepare the homicides dataset for joining

homicides <-
  homicides_raw %>% 
  mutate(month_str = case_when(month_str == 'Abr' ~ 'apr', 
                               month_str == 'Ene' ~ 'jan',
                               month_str == 'Ago' ~ 'aug',
                               month_str == 'Dic' ~ 'dec',
                               TRUE ~ month_str)) %>% 
  filter(!is.na(month_str), !is.na(year)) %>%
  mutate(month_year = parse_date_time(paste0(month_str,year), orders = 'b Y') %>% ymd()) %>%
  group_by(province, month_year) %>% 
  summarise(homicides = sum(homicides)) %>%
  ungroup() %>% 
  left_join(province_codes %>% select(province_all_tildes_sdt, province_code), 
            by = c('province' = 'province_all_tildes_sdt'))

# Join to the main df

df <-
  df %>% 
  left_join(homicides %>% select(-province), by = c('province_code', 'month_year'))

# Confirmed COVID-19 Cases

# Prepare the dataset and separate between all the types of cases

covid_confirmed  <-
  covid_cases_raw %>%
  rename(date = 'fecha_notificacion',
         year = 'anio_notificacion',
         month = 'mes_notificacion',
         day = 'dia_notificacion',
         province_code2 = 'cod_provincia',
         final_condition = 'condicion_final',
         death_date = 'fecha_defuncion',
         death_year = 'anio_defuncion',
         death_month = 'mes_defuncion',
         death_day = 'dia_defuncion',
         final_classification = 'clasificacion_final') %>% 
  mutate(date = ymd(date),
         month_year = floor_date(date, 'month'),
         province_code2 = as.character(province_code2)) %>% 
  filter(final_classification == 'CONFIRMADO') %>% 
  group_by(province_code2, month_year) %>% 
  summarise(covid_confirmed = n()) %>%
  ungroup() %>% 
  left_join(province_codes %>% select(province_code, province_code2), by = 'province_code2') %>% 
  select(-province_code2)

# Left join to the actual dataframe

df <-
  df %>% 
  left_join(covid_confirmed, by = c('province_code', 'month_year'))

# Suspected + Likely COVID-19 Cases

covid_susp <-
  covid_cases_raw %>%
  rename(date = 'fecha_notificacion',
         year = 'anio_notificacion',
         month = 'mes_notificacion',
         day = 'dia_notificacion',
         province_code2 = 'cod_provincia',
         final_condition = 'condicion_final',
         death_date = 'fecha_defuncion',
         death_year = 'anio_defuncion',
         death_month = 'mes_defuncion',
         death_day = 'dia_defuncion',
         final_classification = 'clasificacion_final') %>% 
  mutate(date = ymd(date),
         month_year = floor_date(date, 'month'),
         province_code2 = as.character(province_code2)) %>% 
  filter(!(final_classification %in% c('CONFIRMADO', 'DESCARTADO'))) %>% 
  group_by(province_code2, month_year) %>% 
  summarise(covid_susp = n()) %>%
  ungroup() %>% 
  left_join(province_codes %>% select(province_code, province_code2), by = 'province_code2') %>% 
  select(-province_code2)

# Left join to the actual dataframe and compute total cases

df <-
  df %>% 
  left_join(covid_susp, by = c('province_code', 'month_year')) %>% 
  mutate(total_covid_cases = covid_confirmed + covid_susp)

# Dead by COVID-19

# Prepare a dataset with the same source as before but with the monthly dead, using the variables I defined before.

covid_dead <-
  covid_cases_raw %>%
  rename(date = 'fecha_notificacion',
         year = 'anio_notificacion',
         month = 'mes_notificacion',
         day = 'dia_notificacion',
         province_code2 = 'cod_provincia',
         final_condition = 'condicion_final',
         death_date = 'fecha_defuncion',
         death_year = 'anio_defuncion',
         death_month = 'mes_defuncion',
         death_day = 'dia_defuncion',
         final_classification = 'clasificacion_final') %>% 
  mutate(death_date = ymd(death_date),
         month_year = floor_date(death_date, 'month'),
         province_code2 = as.character(province_code2)) %>% 
  filter(final_classification != 'DESCARTADO') %>%  
  group_by(province_code2, month_year) %>% 
  summarise(total_covid_dead = n()) %>%
  ungroup() %>% 
  left_join(province_codes %>% select(province_code, province_code2), by = 'province_code2') %>% 
  select(-province_code2)

# Left join to the actual dataframe and compute total cases

df <-
  df %>% 
  left_join(covid_dead, by = c('province_code', 'month_year'))

# Tax Registry

# Prepare the dataset

tax_registry <-
  active_2017 %>% 
  bind_rows(active_2018) %>% 
  bind_rows(active_2019) %>% 
  bind_rows(active_2020) %>% 
  bind_rows(active_2021) %>% 
  bind_rows(active_2022) %>%
  left_join(month_lookup_tax_registry, 
            by = c("DFC_DESCRIPCION_MES" = "spanish_month")) %>%
  rename(year = 'DFC_ANIO_PK',
         month_str = 'english_month',
         registered = 'TOTAL',
         province = 'DESCRIPCION_PROVINCIA') %>% 
  select(-DFC_DESCRIPCION_MES) %>% 
  mutate(month_year = parse_date_time(paste0(month_str, year), orders = 'b Y') %>% ymd(),
         province = case_when(
           province == 'CA\xd1AR' ~ 'CAÑAR',
           province == '' ~ NA,
           TRUE ~ province
         )) %>% 
  left_join(province_codes %>% select(province_code, province_no_tilde), by = c('province' = 'province_no_tilde')) %>% 
  group_by(province_code, month_year) %>% 
  summarise(registered = sum(registered)) %>% 
  ungroup()

# Left join to the full dataframe

df <- 
  df %>% 
  left_join(tax_registry, by = c('province_code', 'month_year'))

# IRS Reported Sales ------------------------------------------------------

# Prepare the dataset

sales <-
  sales_2017 %>%
  rename(VENTAS_NETAS_TARIFA_12 = 'Ventas.netas.tarifa.12.',
         VENTAS_NETAS_TARIFA_0 = 'Ventas.netas.tarifa.0.',
         COMPRAS_NETAS_TARIFA_12 = 'Compras.netas.tarifa.12.',
         COMPRAS_NETAS_TARIFA_0 = 'Compras.netas.tarifa.0.',
         ANIO = 'AÑO') %>% 
  bind_rows(sales_2018) %>%
  bind_rows(sales_2019) %>% 
  bind_rows(sales_2020) %>%
  bind_rows(sales_2021) %>%
  bind_rows(sales_2022) %>%
  rename(year = 'ANIO',
         month = 'MES',
         province = 'PROVINCIA') %>%
  mutate(month_year = make_date(year = year, month = month, day = 01),
         province = if_else(province == 'CA\xd1AR','CAÑAR',province),
         total_sales = gsub(',','.', TOTAL_VENTAS) %>% as.numeric()) %>%
  select(-TOTAL_VENTAS) %>% 
  filter(province != 'ND') %>%
  left_join(province_codes %>% select(province_code, province_no_tilde), by = c('province' = 'province_no_tilde')) %>% 
  group_by(province_code, month_year) %>% 
  summarise(total_sales = sum(total_sales)) %>% 
  ungroup()

# Left join to the dataframe

df <-
  df %>% 
  left_join(sales, by = c('month_year', 'province_code'))

# Transit Accidents -------------------------------------------------------

# Prepare dataset

transit_accidents <-
  transit_accidents_raw %>%
  rename(year = 'ANIO',
         province = 'PROVINCIA',
         province_code2 = 'DPA_1',
         month = 'MES_2',
         date = 'FECHA') %>%
  mutate(date = dmy(date),
         month_year = floor_date(date),
         province_code2 = as.character(province_code2)) %>% 
  group_by(province_code2, month_year) %>% 
  summarise(transit_accidents = n()) %>% 
  ungroup() %>% 
  left_join(province_codes %>% select(province_code2, province_code), by = 'province_code2') %>% 
  select(-province_code2)

# Left join to the main one

df <-
  df %>% 
  left_join(transit_accidents, by = c('month_year', 'province_code'))

# Running variable/centered monthly --------------------------------------------------------

# Define the running variable: number of months before the month of implementation. 
# It means "centering" the variable around the date that we care for. 

df <-
  df %>% 
  mutate(time = 12 * (as.yearmon(month_year) - as.yearmon(as.Date('2020-05-01'))))

# Create a treatment variable, factorize for regressions and add some extras

df <-
  df %>%
  mutate(treat = if_else(month_year > as.Date('2020-05-01'),'Treatment Period','Control Period') %>% as.factor(),
         province_code = as.factor(province_code),
         my_event = as.factor(month_year %>% format( '%B%Y')),
         my_event = relevel(my_event, ref = 'May2020'),
         lag_jobs = lag(jobs))

# Final preparations ------------------------------------------------------

# Get the province name, without any kind of tilde

df <-
  df %>%
  left_join(province_codes %>% select(province_code, province_no_tilde), by = 'province_code')

# Export --------------------------------------------------------------------------------------------------

# Export the main dataframe to an RData object

save(df, 
     file = 'data/df-main.RData')

# Export a reduced dataframe to an RData object, between 2018 and 2022

df18_22 <-
  df %>% 
  filter(year %>% between(2018, 2022))

save(df18_22,
     file = 'data/df18_22.RData')
