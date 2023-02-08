#clear memory and setup
rm(list=ls())
options(scipen=999)

#packages
library(tidyverse)
library(vroom)
library(lubridate)
library(fuzzyjoin)
library(fixest)

source('functions.R')

#load matched trades
trades_matched <- vroom('../temporary/matched_paxful_trades.csv')
trades_matched$date <- as.POSIXct(trades_matched$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
country_data <- read.csv('../temporary/country_data.csv')

#US outflows
outflows_us <- trades_matched %>%
  filter(user_cc == "US" & user_cc2 != "US")

#get weekly volume
weekly_country <- outflow_volume_country(outflows_us, amount_usd, 'month')

#join crypto and foreign-born data
df <- inner_join(weekly_country, country_data, by = c("user_cc2" = "alpha.2"))

#add treatment and pre-post
treatment <- as.Date('2020-04-01')

df <- df %>%
  mutate(post = ifelse(time >= treatment, 1, 0),
         treat = ifelse(fb1_per1000 >= median(sort(unique(fb1_per1000))), 1, 0),
         time_to_treat = as.numeric(round(difftime(time, treatment, units = 'months'))))

est_did <- df %>%
  filter(time_to_treat < 10 & time_to_treat > -10) %>%
  feols(volume ~ i(time_to_treat, treat, ref = 0)|label + time_to_treat)

summary(est_did)

iplot(est_did)

############Playground

test <- tibble(time = df$time, time_to_treat = as.numeric(round(difftime(df$time, treatment, units = 'weeks'))))


