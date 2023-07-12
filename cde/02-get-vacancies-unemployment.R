# Program: 02-get-vacancies-unemployment.R
# Purpose: Get data from FRED on the number of job openings and number of
# unemployed people in the US economy.
# 
# The data are saved with a date suffix. That way figures used in the paper can
# be reproduced, as this program dynamically retrieves the latest data from
# FRED.
# 
# Date Started: 2023-07-11
# Date Revised: 2023-07-11

library(tidyverse)
library(tidyquant)
library(assertr)
library(here)

file_prg <- "02-get-vacancies-unemployment"

# Date on vacancies and unemployment --------------------------------------

# Job Openings: Total Nonfarm (JTSJOL)
# Observation: May 2021: 9,209 (+ more) 
# Updated: Jul 7, 2021
# Units: Level in Thousands, Seasonally Adjusted
# Frequency: Monthly

# Unemployment Level (UNEMPLOY)
# Observation: Jun 2021: 9,484 (+ more)  
# Updated: Jul 2, 2021
# Units: Thousands of Persons, Seasonally Adjusted
# Frequency: Monthly

fred_series <- c("UNEMPLOY", "JTSJOL")
dat <- tq_get(fred_series,
              get = "economic.data",
              from = "1800-01-01")

# Get dates when both series start
dat_dates <- dat %>% 
  group_by(symbol) %>% 
  summarise(date = min(date)) %>% 
  pull(date)
dat_dates <- max(dat_dates)

# Keep the data where both series are available
dat <- dat %>% 
  filter(date >= dat_dates) %>% 
  rename(FRED_symbol = symbol,
         lvl_thousands = price) %>% 
  # Convert to millions
  mutate(lvl_millions = lvl_thousands / 1000)

# Save data as csv file ---------------------------------------------------

fout <- here("dta", "cln", paste0("dat_", file_prg, "_", Sys.Date(), ".csv"))
write_csv(dat, file = fout)
