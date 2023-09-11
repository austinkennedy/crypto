#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(vroom)
library(lubridate)
library(fixest)
library(modelsummary)
library(synthdid)
library(data.table)
library(Synth)
source('functions.R')

#load matched trades
trades_matched <- vroom('../temporary/matched_paxful_trades.csv')
trades_matched$date <- as.POSIXct(trades_matched$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
country_data <- read.csv('../temporary/country_data.csv')

#overall outflows
flows <- trades_matched %>%
  filter(user_cc != user_cc2)

#get volume
flows_volume <- outflow_volume_country(flows, amount_usd, 'week')

outflows_by_country <- flows_volume %>%
  group_by(user_cc, time) %>%
  summarise(outflow = sum(volume))

dates <- unique(outflows_by_country$time)
countries <- unique(outflows_by_country$user_cc)

panel <- as_tibble(CJ(dates, countries)) %>% rename(time = dates, user_cc = countries)

panel <- panel %>% mutate(country_number = as.numeric(factor(user_cc)),
                          time_number = as.numeric(factor(time)))

outflows_by_country_balanced <- panel %>%
  left_join(outflows_by_country, by = c('time', 'user_cc')) %>%
  replace(is.na(.), 0)

disbursement <- as.Date('2020-04-09')

window_end <- as.Date('2021-08-05')

outflows_by_country_balanced <- outflows_by_country_balanced %>%
  mutate(time = as.Date(time)) %>%
  mutate(treated = ifelse((user_cc == "US" & time > disbursement), 1, 0)) %>%
  mutate(outflow_asinh = asinh(outflow)) %>%
  filter(time < window_end)





data_scm <- dataprep(foo = as.data.frame(outflows_by_country_balanced),
                     dependent = 'outflow',
                     unit.variable = 'country_number',
                     time.variable = 'time_number',
                     treatment.identifier = 200,
                     controls.identifier = c(1:199, 201:214),
                     time.optimize.ssr = c(140:160),
                     time.predictors.prior = c(140:160),
                     unit.names.variable = c('user_cc')
)

outflows_short <- outflows_by_country_balanced %>% filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-07-05'))

setup = panel.matrices(as.data.frame(outflows_short),
                       unit = 'user_cc',
                       time = 'time',
                       outcome = 'outflow',
                       treatment = 'treated')

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sprintf('point estimate: %1.2f', tau.hat)
plot(tau.hat, overlay = 1)

########TEST SYNTH DID

data('california_prop99')

data <- as_tibble(california_prop99)
setup = panel.matrices(california_prop99)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)
