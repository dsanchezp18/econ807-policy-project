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

naive <- lm(log(jobs) ~ time*treat, data = df)
summary(naive)

coeftest(naive, vcov = vcovHC(naive))

coeftest(naive, vcov = vcovHAC(naive))

# Quadratic, naive

lm(log(jobs) ~ I(time^2) + time*treat + I(time^2) * treat, 
   data = df) %>% summary()

# Cubic

lm(log(jobs) ~ I(time^3) + time*treat, 
   data = df) %>% summary()

# Province fixed effects

province_fe <- feols(log(jobs) ~ time*treat | province_code, 
                     cluster = ~ province_code,
                     data = df)
summary(province_fe)

# Province + year fixed effects

province_year_fe <- feols(log(jobs) ~ time*treat | province_code + year, 
                          cluster = ~ province_code + year,
                          data = df)
summary(province_year_fe)

# Province + month fixed effects (is a month jan? is a month feb? control for that effect)

province_month_name_fe <- feols(log(jobs) ~ time*treat | province_code + month,
                                cluster = ~ province_code + month,
                                data = df)
summary(province_month_name_fe)
coeftest(province_month_name_fe, vcovHAC(province_month_name_fe))

# Do quadratic term with and without interactions for these models. Consider it as the key regression from above. 

quadratic_fe <- feols(log(jobs) ~ I(time^2) + time*treat + I(time^2)*treat | province_code + month, 
                     cluster = ~ province_code + month, 
                     data = df)

summary(quadratic_fe)
coeftest(quadratic_fe, vcovHAC(quadratic_fe))

# (Apparently quadratic is a bit better)

# Event study regression

event_study <- feols(log(jobs) ~ my_event | province_code, 
                     cluster = ~ province_code + my_event,
                     data = df)

summary(event_study) 
coeftest(event_study, vcovHAC(event_study))

# Not very good results supporting pre-event zero effects, but probably a trend that we can incorporate in the model.

# Adding the lag of the previous period to the key regression from above

quadratic_fe_lag <- feols(log(jobs) ~ lag(log(jobs)) + I(time^2) + time*treat + I(time^2)*treat | province_code + month, 
                      cluster = ~ province_code + month, 
                      data = df)

summary(quadratic_fe_lag)
coeftest(quadratic_fe_lag, vcovHAC(quadratic_fe))

# That messes me up big time. Probably should think about the time series models



