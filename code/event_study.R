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

#create treated vs. untreated
#Migrant stock per capita as treatment
# country_data <- country_data %>%
#   mutate(treat = ifelse(fb1_per1000 >= median(sort(fb1_per1000)), 1,0))

#Migrant stock itself as treatment
country_data <- country_data %>%
  mutate(treat = ifelse(fb1 >= median(sort(fb1)), 1,0))

#Remittance fees as treatment
# country_data <- country_data %>%
#   mutate(treat = ifelse(fees_median >= median(sort(fees_median)), 1,0)) %>%
#   drop_na(treat)



#US outflows
outflows_us <- trades_matched %>%
  filter(user_cc == "US" & user_cc2 != "US")


#get weekly volume
weekly_country <- outflow_volume_country(outflows_us, amount_usd, 'week')

#join crypto and foreign-born data
df <- inner_join(weekly_country, country_data, by = c("user_cc2" = "alpha.2"))

#add treatment and pre-post
treatment <- as.Date('2020-04-09')

df <- df %>%
  mutate(post = ifelse(time >= treatment, 1, 0),
         time_to_treat = as.numeric(round(difftime(time, treatment, units = 'weeks'))))

 





############Playground

#Use other countries as control

outflows <- trades_matched %>%
  filter(user_cc != user_cc2)

#get volume
outflows_volume <- outflow_volume_country(outflows, amount_usd, 'week')

treatment <- as.Date('2020-04-09')

#create treated, post, time to treatment
outflows_volume <- outflows_volume %>%
  mutate(treat = ifelse(user_cc == 'US', 1, 0),
         post = ifelse(time >= treatment, 1, 0),
         time_to_treat = as.numeric(round(difftime(time, treatment, units = 'weeks'))))

est_did <- outflows_volume %>%
  filter(time_to_treat < 30 & time_to_treat > -30) %>%
  feols(volume ~ i(time_to_treat, treat, ref = 0)|user_cc + time_to_treat)

summary(est_did)

iplot(est_did)



#Number of trades as outcome

trades_count <- trade_count(outflows_us, 'day')

testdf <- inner_join(trades_count, country_data, by = c("user_cc2" = "alpha.2"))

treatment <- as.Date('2020-04-09')

testdf <- testdf %>%
  mutate(post = ifelse(time >= treatment, 1, 0),
         time_to_treat = as.numeric(round(difftime(time, treatment, units = 'days'))))

est_did <- testdf %>%
  filter(time_to_treat < 30 & time_to_treat > -30) %>%
  feols(total_trades ~ i(time_to_treat, treat, ref = 0)|label + time_to_treat)

summary(est_did)

iplot(est_did)

#Remittance fees as treatment



