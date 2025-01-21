#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(vroom)
library(data.table)
library(Synth)
source('functions.R')

#load matched trades
data <- vroom('../temporary/data_sdid.csv')
country_data <- read.csv('../temporary/country_data.csv')

#clean outflow data
data$time <- as.Date(data$time)
data[is.na(data)] <- 0

data_normalized <- prepare_data_synth(
  data = data,
  country_data = country_data,
  window_start = '2019-10-01',
  # window_end = '2020-06-07',
  window_end = '2020-09-01',
  disbursement = '2020-04-09',
  treated_unit = 'US',
  normalize_to_base_period = TRUE
)

predictor_names <- c('crypto_adoption', 'gdp_pc', 'network_readiness', 'economic_freedom')

data_normalized <- data_normalized[apply(data_normalized[predictor_names], 1, function(row) all(!is.na(row))), ]

treated_id <- max(data_normalized[data_normalized$user_cc == 'US',]$country_number)

control_ids <- setdiff(unique(data_normalized$country_number), treated_id)

post_id <- min(data_normalized[data_normalized$treated == 1,]$time_number)

min_time_id <- min(data_normalized$time_number)

max_time_id <- max(data_normalized$time_number)

mid_pretreatment_time_id <- round((min_time_id+post_id)/2)

min_country_id <- min(data_normalized$country_number)

max_country_id <- max(data_normalized$country_number)


data_scm <- dataprep(foo = as.data.frame(data_normalized),
                     # dependent = 'outflow',
                     dependent = 'outflow_normalized',
                     # dependent = 'outflow_log',
                     unit.variable = 'country_number',
                     time.variable = 'time_number',
                     treatment.identifier = treated_id,
                     controls.identifier = control_ids,
                     time.optimize.ssr = c(mid_pretreatment_time_id:(post_id - 1)),
                     time.predictors.prior = c(min_time_id:mid_pretreatment_time_id),
                     unit.names.variable = c('user_cc'),
                     predictors = predictor_names,
                     time.plot = min_time_id:max_time_id
)

synth_out <- synth(data_scm)

path.plot(synth.res = synth_out,
          dataprep.res = data_scm,
          tr.intake = 161)

gaps.plot(synth.res = synth_out,
          dataprep.res = data_scm)

tables <- synth.tab(dataprep.res = data_scm,
                     synth.res = synth_out)


