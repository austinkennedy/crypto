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
library(data.table)
library(abind)

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
  mutate(outflow_log = log(outflow))

##Synthetic Control

# predictor_names <- shares_2019 %>% select(NG:BD) %>% colnames()

# data_cut <- data %>%
#   filter(time >= window_start & time <= window_end)
# 
# treated_id <- max(data_cut[data_cut$user_cc == 'US',]$country_number)
# 
# post_id <- min(data_cut[data_cut$treated == 1,]$time_number)
# 
# min_time_id <- min(data_cut$time_number)
# 
# max_time_id <- max(data_cut$time_number)
# 
# min_country_id <- min(data_cut$country_number)
# 
# max_country_id <- max(data_cut$country_number)
# 
# 
# data_scm <- dataprep(foo = as.data.frame(data_cut),
#                      dependent = 'outflow',
#                      unit.variable = 'country_number',
#                      time.variable = 'time_number',
#                      treatment.identifier = 199,
#                      controls.identifier = c(min_country_id:(treated_id - 1), (treated_id + 1):max_country_id),
#                      time.optimize.ssr = c(min_time_id:(post_id - 1)),
#                      time.predictors.prior = c(140:160),
#                      unit.names.variable = c('user_cc'),
#                      # predictors = predictor_names,
#                      time.plot = 140:180
# )
# 
# synth_out <- synth(data_scm)
# 
# path.plot(synth.res = synth_out,
#           dataprep.res = data_scm,
#           tr.intake = 161)
# 
# gaps.plot(synth.res = synth_out,
#           dataprep.res = data_scm)

###Synthetic DID

#Create X matrix
# 
# flows_shares_weekly_balanced <- as.data.table(flows_shares_weekly_balanced)
# 
# X_matrix <- flows_shares_weekly_balanced %>%
#   melt(id.var = c('user_cc', 'time')) %>%
#   nest_by(variable) %>%
#   mutate(X = list(
#     dcast(data.table(data), user_cc ~ time, value.var = 'value') %>%
#       .[data.table(user_cc = rownames(setup$Y)), on = 'user_cc'] %>%
#       .[, user_cc := NULL] %>%
#       as.matrix()
#   )) %>%
#   .$X %>%
#   abind(along=3)

#Synthdid setup

setup = panel.matrices(as.data.frame(data_cut),
                       unit = 'user_cc',
                       time = 'time',
                       outcome = 'outflow',
                       treatment = 'treated')
  
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
sprintf('point estimate: %1.2f', tau.hat)
synthdid_plot(tau.hat, overlay = 1)
synthdid_plot(tau.hat, overlay = 0 )

se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

synthdid_units_plot(tau.hat)

synthdid_units_plot(
  tau.hat,
  negligible.threshold = 0.001,
  negligible.alpha = 0.3,
  se.method = "jackknife",
  units = NULL
)


########TEST SYNTH DID

data('california_prop99')

data <- as_tibble(california_prop99)

data <- 
  data.table(california_prop99) %>% 
  .[, x1 := 100 * runif(nrow(.))] %>% 
  .[, x2 := runif(nrow(.))]

setup = panel.matrices(california_prop99)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)

N <- 100
T <- 10

data_test <- 
  expand.grid(
    id = 1:N,
    year = 1:T
  ) %>% 
  data.table() %>% 
  .[, trt_or_ctrl := ifelse(id <= (N / 2), "treatment", "control")] %>% 
  .[, treated := ifelse(trt_or_ctrl == "treatment" & year > (T - 5), 1, 0)] %>% 
  .[, `:=`(
    X_1 = rnorm(nrow(.)),
    X_2 = rnorm(nrow(.))
  )] %>%  
  .[, y := 1 + 1 * treated + rnorm(nrow(.)) + 1 * X_1 + 1 * X_2] 

sdid_setup <- 
  synthdid::panel.matrices(
    data_test[, .(id, year, y, treated)],
    unit = 1,
    time = 2,
    outcome = 3,
    treatment = 4
  )

#/*----------------------------------*/
#' ## Construct X 
#/*----------------------------------*/
X_mat <- 
  data_test[, .(id, year, X_1, X_2)] %>% 
  melt(id.var = c("id", "year")) %>% 
  #=== dataset by variable ===#
  nest_by(variable) %>% 
  mutate(X = list(
    dcast(data.table(data), id ~ year, value.var = "value") %>% 
      #=== order the observations to match that of sdid_setup$Y ===#
      .[data.table(id = as.numeric(rownames(sdid_setup$Y))), on = "id"] %>% 
      .[, id := NULL] %>% 
      as.matrix() 
  )) %>% 
  .$X %>% 
  #=== list of matrices to 3-D array of N * T * C ===#
  # C: number of covariates
  abind(along = 3) 

