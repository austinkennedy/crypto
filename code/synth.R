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
trades_matched <- vroom('../temporary/matched_paxful_trades.csv')
trades_matched$date <- as.POSIXct(trades_matched$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
country_data <- read.csv('../temporary/country_data.csv')

#overall outflows
flows <- trades_matched %>%
  filter(user_cc != user_cc2)

#get volume
flows_volume <- outflow_volume_country(flows, amount_usd, 'week')

flows_volume <- inner_join(flows_volume, country_data, by = c("user_cc2" = "alpha.2"))

#baseline (2019 for now) outflows by receiving country

flows_volume_yearly <- outflow_volume_country(flows, amount_usd, 'year')

#get yearly shares
flows_shares_yearly <- flows_volume_yearly %>%
  group_by(time, user_cc) %>%
  mutate(share = volume/sum(volume))

flows_shares_yearly <- flows_shares_yearly %>% select(-c(volume))

flows_shares_yearly <- flows_shares_yearly %>% pivot_wider(names_from = user_cc2, values_from = share)

shares_2019 <- flows_shares_yearly %>% filter(time == '2019-01-01') %>% ungroup() %>% select(-c(time))

shares_2019[is.na(shares_2019)] <- 0

shares_2019 <- shares_2019 %>% select(where(~ any(.!= 0)))

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

outflows_by_country_balanced <- outflows_by_country_balanced %>% left_join(shares_2019, by = 'user_cc')


outflows_by_country_balanced[is.na(outflows_by_country_balanced)] <- 0

##Synthetic Control

predictor_names <- shares_2019 %>% select(NG:BD) %>% colnames()


data_scm <- dataprep(foo = as.data.frame(outflows_by_country_balanced),
                     dependent = 'outflow_asinh',
                     unit.variable = 'country_number',
                     time.variable = 'time_number',
                     treatment.identifier = 199,
                     controls.identifier = c(1:198, 200:213),
                     time.optimize.ssr = c(140:160),
                     time.predictors.prior = c(140:160),
                     unit.names.variable = c('user_cc'),
                     predictors = predictor_names,
                     time.plot = 140:180
)

synth_out <- synth(data_scm)

path.plot(synth.res = synth_out,
          dataprep.res = data_scm,
          tr.intake = 161)

gaps.plot(synth.res = synth_out,
          dataprep.res = data_scm)

###Synthetic DID

window_start <- as.Date('2019-01-01')
window_end <- as.Date('2020-07-05')

outflows_short <- outflows_by_country_balanced %>% filter(time >= window_start & time <= window_end )

#Get week by receiving country flows

flows_volume_weekly <- outflow_volume_country(flows, amount_usd, 'week')

#get share of outflows to each country, by week
flows_shares_weekly <- flows_volume_weekly %>%
  group_by(time, user_cc) %>%
  mutate(share = volume/sum(volume)) %>%
  select(-c(volume)) %>%
  pivot_wider(names_from = user_cc2, values_from = share)

flows_shares_weekly[is.na(flows_shares_weekly)] <- 0

flows_shares_weekly_balanced <- panel %>%
  left_join(flows_shares_weekly, by = c('time', 'user_cc')) %>%
  select(-c(country_number, time_number)) %>%
  replace(is.na(.), 0) %>%
  filter(time >= window_start & time <= window_end)


#Synthdid setup
setup = panel.matrices(as.data.frame(outflows_short),
                       unit = 'user_cc',
                       time = 'time',
                       outcome = 'outflow_asinh',
                       treatment = 'treated')

#Create X matrix

flows_shares_weekly_balanced <- as.data.table(flows_shares_weekly_balanced)

X <- flows_shares_weekly_balanced %>%
  melt(id.var = c('user_cc', 'time')) %>%
  nest_by(variable) %>%
  mutate(X = list(
    dcast(data.table(flows_shares_weekly_balanced), user_cc ~ time, value.var = 'value') %>%
      .[data.table(id = as.numeric(rownames(sdid_setup$Y))), on = 'user_cc'] %>%
      .[, id := NULL] %>%
      as.matrix()
  )) %>%
  .$X %>%
  abind(along=3)
  

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
tau.sc = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.did = did_estimate(setup$Y, setup$N0, setup$T0)
sprintf('point estimate: %1.2f', tau.hat)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat, overlay=0)
plot(tau.sc)

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

data <- 
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
    data[, .(id, year, y, treated)],
    unit = 1,
    time = 2,
    outcome = 3,
    treatment = 4
  )

#/*----------------------------------*/
#' ## Construct X 
#/*----------------------------------*/
X_mat <- 
  data[, .(id, year, X_1, X_2)] %>% 
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

