### ECON807 Policy Project
## SFU Spring 2023
# Daniel Sanchez
# Script for data visualization

# Preliminaries -------------------------------------------------------------------------------------------

# Load libraries

library(tidyverse)
library(lubridate)

# Load data

load('data/df-main.RData')


# Business Creation ---------------------------------------------------------------------------------------

# Against time, each point represents one province

scatter_buss_time <-
  df %>% 
  filter(year == 2020 ) %>% 
  ggplot(aes(month_year, buss_new))+
  geom_point()+
  scale_x_date(date_breaks = '1 month', date_labels = '%m-%y')

# Against time, but group at the country level

scatter_buss_time_ecu <-
  df %>% 
  filter(year == 2020 ) %>% 
  group_by(month_year) %>% 
  summarise(buss_new = sum(buss_new)) %>% 
  ggplot(aes(month_year, buss_new))+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = '1 month', date_labels = '%m-%y')

  



