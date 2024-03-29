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
volume_country <- outflow_volume_country(outflows_us, amount_usd, 'week')

#join crypto and country data
df <- inner_join(volume_country, country_data, by = c("user_cc2" = "alpha.2"))


#add treatment and pre-post
treatment <- as.Date('2020-04-09')

#add month, year, and post variable

df <- df %>%
  mutate(post = ifelse(time >= treatment, 1, 0),
         month = month(time),
         year = year(time))

 
xlim_iplot <- c(.5,-.4)


setFixest_coefplot(xlim.add = c(.5, -.4), xlab = "Date", ylab = "ln(Volume)")

est_did_fb <- df %>%
  # filter(time_to_treat < 30 & time_to_treat > -30) %>%
  feols(log(volume) ~ i(time, fb_above, ref = "2020-04-05")|label + time + month, cluster = 'label')

summary(est_did_fb)

# png('../output/fb_plot.png')

iplot(est_did_fb, main = 'Above vs. below median median foreign born population')

# dev.off()

est_did_fee <- df %>%
  drop_na(fees_above) %>%
  feols(log(volume) ~ i(time, fees_above, ref = "2020-04-05")|label + time + label^month, cluster = 'label')

summary(est_did_fee)

iplot(est_did_fee, main = 'Above vs. below median remittance fee')

est_did <- df %>%
  feols(log(volume) ~ i(post, fb1*fees_median, ref = 0) + i(post, fb1, ref = 0) + i(post, fees_median, ref = 0)|time + label)

summary(est_did)




############Playground

#Use other countries as control

outflows <- trades_matched %>%
  filter(user_cc != user_cc2)

#get volume
outflows_volume <- outflow_volume_country(outflows, amount_usd, 'day')

df <- inner_join(outflows_volume, country_data, by = c("user_cc2" = "alpha.2"))

disbursement <- as.Date('2020-04-09')

#create treated, post, time to treatment
df <- df %>%
  mutate(treat = ifelse(user_cc == 'US', 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0))

est_did <- df %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05'),
         income_group %in% c("L", "LM")) %>%
  feols(log(volume) ~ i(time, treat, ref = as.Date('2020-04-05'))|user_cc^user_cc2 + time)

summary(est_did)

setFixest_coefplot(xlim.add = c(0, 0), xlab = "Date", ylab = "ln(Volume)")

iplot(est_did, main = "US vs. Non-US Crypto Outflows")



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

#Individual-level

df <- inner_join(outflows_us, country_data, by = c("user_cc2" = "alpha.2"))

df <- df %>% mutate(week = as.Date(floor_date(date, 'week')))

treatment <- as.Date('2020-04-09')

df <- df %>%
  mutate(post = ifelse(week >= treatment, 1, 0),
         time_to_treat = as.numeric(round(difftime(week, treatment, units = 'weeks'))))

est_did <- df %>%
  filter(time_to_treat < 30 & time_to_treat > -30) %>%
  feols(amount_usd ~ i(time_to_treat, treat, ref = 0)|label + time_to_treat)

iplot(est_did, main = 'Average Trade Size, High vs. Low Remittance Fee')



