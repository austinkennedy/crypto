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

disbursement <- as.Date('2020-04-09')


window_start <- as.Date('2020-01-01')
window_end <- as.Date('2020-06-07')

data <- data %>%
  mutate(time = as.Date(time)) %>%
  mutate(treated = ifelse((user_cc == "US" & time > disbursement), 1, 0))

data[is.na(data)] <- 0

data_cut <- data %>%
  filter(time >= window_start & time <= window_end) %>%
  mutate(outflow_log = log(outflow)) %>%
  left_join(country_data, by = c('user_cc'='alpha.2')) %>%
  drop_na(label, PopTotal)

predictor_names <- c('PopTotal')

treated_id <- max(data_cut[data_cut$user_cc == 'US',]$country_number)

control_ids <- setdiff(unique(data_cut$country_number), treated_id)

post_id <- min(data_cut[data_cut$treated == 1,]$time_number)

min_time_id <- min(data_cut$time_number)

max_time_id <- max(data_cut$time_number)

min_country_id <- min(data_cut$country_number)

max_country_id <- max(data_cut$country_number)

data_scm <- dataprep(foo = as.data.frame(data_cut),
                     dependent = 'outflow',
                     unit.variable = 'country_number',
                     time.variable = 'time_number',
                     treatment.identifier = 199,
                     controls.identifier = control_ids,
                     time.optimize.ssr = c(min_time_id:(post_id - 1)),
                     time.predictors.prior = c(min_time_id:(post_id - 1)),
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




