### ECON807 Policy Project
## SFU Spring 2023
# Daniel Sanchez
# Script for data visualization

# Preliminaries -------------------------------------------------------------------------------------------

# Load libraries

library(tidyverse)
library(lubridate)
library(patchwork)
library(rdrobust)

# Load data

load('data/df-main.RData')

# Predefine a theme

theme_ds <-
  theme_bw() +
  theme(panel.grid = element_blank())

# Business Creation ---------------------------------------------------------------------------------------

# Against time, each point represents one province

df %>% 
  filter(year == 2020 ) %>% 
  ggplot(aes(month_year, buss_new))+
  geom_point()+
  scale_x_date(date_breaks = '1 month', 
               date_labels = '%m-%y')

# Against time, but group at the country level

df %>% 
  filter(year == 2020 ) %>% 
  group_by(month_year) %>% 
  summarise(buss_new = sum(buss_new)) %>% 
  ggplot(aes(month_year, buss_new))+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = '1 month', 
               date_labels = '%m-%y')

# Jobs --------------------------------------------------------------------

# At the province level in level jobs

df %>%
  filter(year %>% between(2020, 2022)) %>%
  ggplot(aes(month_year, jobs)) +
  geom_point() +
  scale_x_date(date_breaks = '1 month', 
               date_labels = '%m-%y') +
  theme_ds +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  geom_vline(xintercept = as.numeric(as.Date('2020-05-01')),
             colour = 'blue', 
             linetype = 'dashed')

# At the province level in log jobs

# RDiT plots --------------------------------------------------------------

# Run some RDiT plots to see the initial relationship

df_rdit <-
  df %>% 
  filter(month_year %>% between(as.Date('2020-01-01'), as.Date('2020-10-01')))

rdplot(df_rdit$jobs, df_rdit$time, p = 2)

rdplot(df_rdit$jobs, df_rdit$time, p = 1)

rdplot(df_rdit$jobs %>% log(), df_rdit$time, p = 2)