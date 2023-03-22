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
load('data/df18_22.RData')

# Predefine a theme

theme_ds <-
  theme_bw() +
  theme(panel.grid = element_blank())

# Jobs --------------------------------------------------------------------

# At the province level in level jobs IESS

df %>%
  filter(year %>% between(2019, 2022)) %>%
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

# At the province level, log jobs

df %>%
  filter(year %>% between(2019, 2022)) %>%
  ggplot(aes(month_year, log(jobs))) +
  geom_point() +
  scale_x_date(date_breaks = '1 month', 
               date_labels = '%m-%y') +
  theme_ds +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +
  geom_vline(xintercept = as.numeric(as.Date('2020-05-01')),
             colour = 'blue', 
             linetype = 'dashed')

# At the country level, log jobs

log_jobs_country <-
  df18_22 %>% 
  group_by(month_year) %>% 
  summarise(jobs = sum(jobs)) %>% 
  ggplot(aes(month_year, log(jobs)))+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = '6 months', 
               date_labels = '%b-%y') +
  geom_vline(xintercept = as.numeric(as.Date('2020-05-01')),
             colour = 'blue', 
             linetype = 'dashed') +
  theme_ds +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = 'Period',
       y = 'Log of social security-registered job contracts')

log_jobs_country

# At the country level, level jobs

df18_22 %>% 
  group_by(month_year) %>% 
  summarise(jobs = sum(jobs)) %>% 
  ggplot(aes(month_year, jobs))+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = '3 months', 
               date_labels = '%b-%y') +
  theme_ds +
  geom_vline(xintercept = as.numeric(as.Date('2020-05-01')),
             colour = 'blue', 
             linetype = 'dashed') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Job contracts SUT -------------------------------------------------------

# At the province level in level contracts SUT

df %>%
  filter(year %>% between(2020, 2022)) %>%
  ggplot(aes(month_year, contracts)) +
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

df %>% 
  filter(year == 2020 ) %>% 
  group_by(month_year) %>% 
  summarise(jobs = sum(jobs)) %>% 
  ggplot(aes(month_year, log(jobs)))+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = '1 month', 
               date_labels = '%m-%y') +
  geom_vline(xintercept = as.numeric(as.Date('2020-05-01')),
             colour = 'blue', 
             linetype = 'dashed') +
  theme_ds

# Business Creation ---------------------------------------------------------------------------------------

# Against time, each point represents one province

df %>% 
  filter(year == 2020 ) %>% 
  ggplot(aes(month_year, buss_new))+
  geom_point() +
  scale_x_date(date_breaks = '1 month', 
               date_labels = '%m-%y') +
  theme_ds

# Against time, but group at the country level

log_buss_country <-
  df %>% 
  filter(year %>% between(2018,2022)) %>% 
  group_by(month_year) %>% 
  summarise(buss_new = sum(buss_new)) %>% 
  ggplot(aes(month_year, log(buss_new)))+
  geom_point()+
  geom_line()+
  scale_x_date(date_breaks = '6 months', 
               date_labels = '%b-%y') +
  geom_vline(xintercept = as.numeric(as.Date('2020-05-01')),
             colour = 'blue', 
             linetype = 'dashed') +
  theme_ds +
  labs(x = 'Period',
       y = 'Log of active businesses') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

log_buss_country

# Jobs & Businesses -------------------------------------------------------

# Do a graph with jobs and businesses active, log of both, at the country level

df %>% 
  filter(year %>% between(2017, 2022)) %>% 
  select(month_year, jobs, buss_new) %>% 
  gather(series, value, jobs, buss_new) %>%
  group_by(month_year, series) %>% 
  summarise(value = sum(value)) %>% 
  ggplot(aes(month_year, log(value), colour = series)) +
  geom_point() +
  geom_line() +
  theme_ds

# Do them separately and join them with patchwork, export

graph_joint <-
  log_buss_country + 
  log_jobs_country +
  plot_layout(ncol = 2)

png(filename = 'figures/joint_graph_buss_jobs.png',
    width = 900,
    height = 600)

graph_joint

dev.off()

# RDiT plots --------------------------------------------------------------

# Run some RDiT plots to see the initial relationship

df_rdit <-
  df %>% 
  filter(month_year %>% between(as.Date('2020-01-01'), as.Date('2020-10-01')))

rdplot(df_rdit$contracts, df_rdit$time, p = 2)

rdplot(df_rdit$contracts, df_rdit$time, p = 1)

rdplot(df_rdit$contracts %>% log(), df_rdit$time, p = 2)
