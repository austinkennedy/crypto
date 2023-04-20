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


#get volume
volume_country <- outflow_volume_country(outflows_us, amount_usd, 'day')

#join crypto and country data
df <- inner_join(volume_country, country_data, by = c("user_cc2" = "alpha.2"))

#add treatment date
treatment <- as.Date('2020-04-09')

#add month, year, and post variable

df <- df %>%
  mutate(post = ifelse(time >= treatment, 1, 0),
         month = month(time),
         year = year(time))

basic_reg_fml <- as.formula('log(volume) ~ log(fb1)*fees_median + log(fb1) + fees_median')

basic_reg <- df %>%
  feols(basic_reg_fml)

summary(basic_reg)

basic_reg_lowermiddle <- df %>%
  filter(income_group %in% c('L', 'LM')) %>%
  feols(basic_reg_fml)

summary(basic_reg_lowermiddle)

basic_reg_developed <- df %>%
  filter(!(income_group %in% c('L', 'LM'))) %>%
  feols(basic_reg_fml)

summary(basic_reg_developed)

fb_did <- as.formula('log(volume) ~ i(post, log(fb1), ref = 0) + i(post, ref = 0) + log(fb1)')

model_fb_full <- df %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fb_did, cluster = 'time')

summary(model_fb_full)

model_fb_lowermiddle <- df %>%
  filter(income_group %in% c('L', 'LM'),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fb_did, cluster = 'time')

summary(model_fb_lowermiddle)

model_fb_developed <- df %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fb_did, cluster = 'time')

summary(model_fb_developed)
  
fee_fml <- as.formula('log(volume) ~ i(post, fees_median, ref = 0) + i(post, ref = 0) + fees_median')

model_fee_full <- df %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fee_fml, cluster = 'time')

summary(model_fee_full)

model_fee_lowermiddle <- df %>%
  filter(income_group %in% c('L', 'LM'),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fee_fml, cluster = 'time')

summary(model_fee_lowermiddle)

model_fee_developed <- df %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fee_fml, cluster = 'time')

summary(model_fee_developed)







