### ECON807 Policy Project
## SFU Spring 2023
# Daniel Sanchez

# This script runs models with the data, prepared in the data-preparation.R script.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(modelsummary)
library(fixest)
library(tidyverse)
library(lmtest)
library(sandwich)

# Load the data

load('data/df18_22.RData')

df <- df18_22

# Baseline ----------------------------------------------------------------

# I run some baseline models using interaction terms or "segmented regression".

# Simple, naive

naive <- lm(log(contracts) ~ time*treat, data = df)
summary(naive)

coeftest(naive, vcov = vcovHC(naive))

coeftest(naive, vcov = vcovHAC(naive))

# Quadratic, naive

lm(log(contracts) ~ I(time^2) + time*treat + I(time^2) * treat, 
   data = df) %>% summary()

# Cubic

lm(log(contracts) ~ I(time^3) + time*treat, 
   data = df) %>% summary()

# Province fixed effects

province_fe <- feols(log(contracts) ~ time*treat | province_code, 
                     cluster = ~ province_code,
                     data = df)
summary(province_fe)

# Province + year fixed effects

province_year_fe <- feols(log(contracts) ~ time*treat | province_code + year, 
                          cluster = ~ province_code + year,
                          data = df)
summary(province_year_fe)

# Province + month fixed effects (is a month jan? is a month feb? control for that effect)

province_month_name_fe <- feols(log(contracts) ~ time*treat | province_code + month,
                                cluster = ~ province_code + month,
                                data = df)
summary(province_month_name_fe)
coeftest(province_month_name_fe, vcovHAC(province_month_name_fe))

# Do quadratic term with and without interactions for these models. Consider it as the key regression from above. 

quadratic_fe <- feols(log(contracts) ~ I(time^2) + time*treat + I(time^2)*treat | province_code + month, 
                      cluster = ~ province_code + month, 
                      data = df)

summary(quadratic_fe)
coeftest(quadratic_fe, vcovHAC(quadratic_fe))

# (Apparently quadratic is a bit better)

# Event study regression

event_study <- feols(log(contracts) ~ my_event | province_code, 
                     cluster = ~ province_code + my_event,
                     data = df)

summary(event_study) 
coeftest(event_study, vcovHAC(event_study))

# Not very good results supporting pre-event zero effects, but probably a trend that we can incorporate in the model.

# Adding the lag of the previous period to the key regression from above

quadratic_fe_lag <- feols(log(contracts) ~ log(lag_contracts) + I(time^2) + time*treat + I(time^2)*treat | province_code + month, 
                          cluster = ~ province_code + month, 
                          data = df)

summary(quadratic_fe_lag)
coeftest(quadratic_fe_lag, vcovHAC(quadratic_fe_lag))

# That messes me up big time. Probably should think about the time series models

# Adding controls ---------------------------------------------------------

# Adding several control variables to correct OVB

# Without COVID-related stuff (more observations)

modified_fe <- feols(log(contracts) ~ time*treat + remote_workers + thefts + homicides + registered + total_sales + transit_accidents | province_code + month,
                     cluster = ~ province_code + month,
                     data = df)

summary(modified_fe)

modified_fe_quad <- feols(log(contracts) ~  I(time^2) + time*treat + I(time^2)*treat + remote_workers + thefts + homicides + registered + total_sales + transit_accidents| province_code + month,
                          cluster = ~ province_code + month,
                          data = df)

summary(modified_fe_quad)

# Event Study without COVID-19

modified_event_study <- feols(log(contracts) ~ my_event + remote_workers + thefts + homicides + registered + total_sales + transit_accidents | province_code, 
                              cluster = ~ province_code + my_event,
                              data = df)

summary(modified_event_study)

# With a lag

modified_lag <- feols(log(contracts) ~ time*treat+ log(lag_contracts) + remote_workers + thefts + homicides + registered + total_sales + transit_accidents | province_code, 
                      cluster = ~ province_code + my_event,
                      data = df)

summary(modified_lag)

# With COVID

modified_fe1 <- feols(log(contracts) ~ time*treat + remote_workers + thefts + homicides + total_covid_cases + registered + total_sales + total_covid_dead + transit_accidents | province_code + month,
                      cluster = ~ province_code + month,
                      data = df)

summary(modified_fe1)

modified_fe1_quad <- feols(log(contracts) ~ I(time^2) + time*treat + I(time^2)*treat + remote_workers + thefts + homicides + total_covid_cases + registered + total_sales + transit_accidents + total_covid_dead | province_code + month,
                           cluster = ~ province_code + month,
                           data = df)

summary(modified_fe1_quad)

# Event Study Regressions

modified_event_study1 <- feols(log(contracts) ~ my_event + remote_workers + thefts + homicides + total_covid_cases + registered + transit_accidents + total_sales + total_covid_dead | province_code, 
                               cluster = ~ province_code + my_event,
                               data = df)

summary(modified_event_study1)

# With lags 

modified_lag1 <- feols(log(contracts) ~ + log(lag_contracts) + time*treat + remote_workers + thefts + homicides + total_covid_cases + registered + total_sales + total_covid_dead +  transit_accidents| province_code + month,
                       cluster = ~ province_code + month,
                       data = df)

summary(modified_lag1)

# Some models including business creation

modified_fe_buss <- feols(log(contracts) ~ time*treat + buss_new + remote_workers + thefts + homicides + registered + total_sales + transit_accidents | province_code + month,
                          cluster = ~ province_code + month,
                          data = df)

summary(modified_fe_buss)

modified_fe_quad_buss <- feols(log(contracts) ~  buss_new + I(time^2) + time*treat + I(time^2)*treat + remote_workers + thefts + homicides + registered + total_sales + transit_accidents| province_code + month,
                               cluster = ~ province_code + month,
                               data = df)

summary(modified_fe_quad_buss)

modified_fe1_buss <- feols(log(contracts) ~ buss_new + time*treat + remote_workers + thefts + homicides + total_covid_cases + registered + total_sales + total_covid_dead + transit_accidents | province_code + month,
                           cluster = ~ province_code + month,
                           data = df)

summary(modified_fe1_buss)

modified_fe1_quad_buss<- feols(log(contracts) ~ buss_new + I(time^2) + time*treat + I(time^2)*treat + remote_workers + thefts + homicides + total_covid_cases + registered + total_sales + transit_accidents + total_covid_dead | province_code + month,
                               cluster = ~ province_code + month,
                               data = df)

summary(modified_fe1_quad_buss)



