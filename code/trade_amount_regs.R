#clear memory and setup
rm(list=ls())
options(scipen=999)

#packages
library(tidyverse)
library(vroom)
library(lubridate)
library(fuzzyjoin)
library(fixest)
library(modelsummary)
library(kableExtra)
library(data.table)
library(ggiplot)

source('functions.R')

transaction_amounts <- read.csv('../temporary/transaction_amounts_bilateral.csv')
country_data <- read.csv('../temporary/country_data.csv')


#add phases
announcement <- as.Date('2020-03-27')

disbursement <- as.Date('2020-04-05')

window_start <- as.Date('2020-01-01')
window_end <- as.Date('2020-12-31')

treated_countries <- c('JP', 'KR', 'SG')

#filter out treated countries
transaction_amounts <- transaction_amounts %>%
  filter(!user_cc %in% treated_countries,
         user_cc != user_cc2) %>%
  left_join(country_data, by = c('user_cc2' = 'alpha.2'))

amounts_all <- transaction_amounts %>%
  group_by(user_cc, time) %>%
  summarize(amounts_avg_all = mean(amount_usd_avg))

amounts_l <- transaction_amounts %>%
  filter(income_group %in% c('L')) %>%
  group_by(user_cc, time) %>%
  summarize(amounts_avg_l = mean(amount_usd_avg))

amounts_m <- transaction_amounts %>%
  filter(income_group %in% c('LM', 'UM')) %>%
  group_by(user_cc, time) %>%
  summarize(amounts_avg_m = mean(amount_usd_avg))

amounts_h <- transaction_amounts %>%
  filter(income_group %in% c('H')) %>%
  group_by(user_cc, time) %>%
  summarize(amounts_avg_h = mean(amount_usd_avg))

amounts_joined <- list(amounts_all, amounts_l, amounts_m, amounts_h) %>%
  reduce(left_join, by = c('user_cc', 'time')) %>%
  left_join(country_data, by = c('user_cc' = 'alpha.2')) %>%
  mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0),
         us_outflow = ifelse(user_cc == "US", 1, 0)
  )


# #get avg transaction amounts by source country, to varying country groups
# transaction_amounts_all <- transaction_amounts %>%
#   group_by(user_cc, time) %>%
#   summarize(amounts_avg_all = mean(amount_usd_avg)) %>%
#   left_join(country_data, by = c('user_cc' = 'alpha.2')) %>%
#   mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
#          disbursed = ifelse(time >= disbursement, 1, 0),
#          us_outflow = ifelse(user_cc == "US", 1, 0))

cluster_level_spillovers <- c('user_cc')

did_yvars <- names(amounts_joined)[grepl("amounts_avg_", names(amounts_joined))]

twfe_fml <- as.formula('.[did_yvars] ~ disbursed*us_outflow|user_cc + time')

twfe_qmle <- amounts_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(twfe_fml, cluster = cluster_level_spillovers, family = quasipoisson)

etable(twfe_qmle)

#Event Study


reference_period <- '2020-03-29'

es_qmle_highincome <- transaction_amounts_all %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(amounts_avg_all ~ i(time, us_outflow, ref = '2020-03-29')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle_highincome)








