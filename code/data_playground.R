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

# create treated vs. untreated
#Migrant stock per capita as treatment
country_data <- country_data %>%
  mutate(fb_pc_above = ifelse(fb1_per1000 >= median(sort(fb1_per1000)), 1,0))

#Migrant stock itself as treatment
country_data <- country_data %>%
  mutate(fb_above = ifelse(fb1 >= median(sort(fb1)), 1,0))

# Remittance fees as treatment
country_data <- country_data %>%
  mutate(fees_above = ifelse(fees_median >= median(sort(fees_median)), 1,0))



#US outflows
outflows_us <- trades_matched %>%
  filter(user_cc == "US" & user_cc2 != "US")


#get weekly volume
weekly_country <- outflow_volume_country(outflows_us, amount_usd, 'week')


denom <- weekly_country %>%
  group_by(user_cc2) %>%
  filter(time == as.Date("2020-04-05")) %>%
  rename(denominator = volume)



weekly_country <- weekly_country %>%
  inner_join(denom[,c("denominator", 'user_cc2')], by = 'user_cc2')

weekly_country <- weekly_country %>%
  mutate(normalized = volume / denominator)

weekly_country %>%
  filter(time >= as.Date("2020-03-21") & time <= as.Date("2021-03-20")) %>%
  ggplot(aes(x = time, y = normalized, color = user_cc2)) + geom_line()










