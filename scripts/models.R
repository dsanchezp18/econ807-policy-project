### ECON807 Policy Project
## SFU Spring 2023
# Daniel Sanchez

# This script runs models with the data, prepared in the data-preparation.R script.

# Preliminaries -----------------------------------------------------------

# Load libraries

library(modelsummary)
library(fixest)
library(tidyverse)

# Load the data

load('data/df18_22.RData')

data <- df18_22

rm(df18_22)

# Baseline ----------------------------------------------------------------

# I run some baseline models using interaction terms or "segmented regression".

# Simple, naive

naive <- lm(log())


