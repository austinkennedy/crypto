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

flows <- vroom('../temporary/bilateral_flows_balanced.csv')
country_data <- read.csv('../temporary/country_data.csv')


#add phases
announcement <- as.Date('2020-03-27')

disbursement <- as.Date('2020-04-05')

window_start <- as.Date('2020-01-01')
window_end <- as.Date('2020-12-31')

treated_countries <- c('JP', 'KR', 'SG')

#filter out treated countries
flows <- flows %>%
  filter(!user_cc %in% treated_countries,
         user_cc != user_cc2)


#get total outflows by source country, to varying country groups
outflows_all <- flows %>%
  group_by(user_cc, time) %>%
  summarize(volume_all = sum(volume)) %>%
  left_join(country_data, by = c('user_cc' = 'alpha.2')) %>%
  mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0),
         us_outflow = ifelse(user_cc == "US", 1, 0))

cluster_level_spillovers <- c('user_cc')

twfe_fml <- as.formula('volume_all ~ disbursed*us_outflow|user_cc + time')

twfe_qmle <- outflows_all %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(twfe_fml, cluster = cluster_level_spillovers, family = quasipoisson)

etable(twfe_qmle)

#Event Study

reference_period <- '2020-03-29'

outflows_filtered <- outflows_all %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H')

es_qmle_highincome <- outflows_filtered %>%
  feglm(volume_all ~ i(time, us_outflow, ref = '2020-03-29')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle_highincome)


#######predict counterfactual

# outflows_all <- outflows_filtered %>%
#   mutate(
#     predicted_actual = predict(es_qmle_highincome,
#                                newdata= outflows_filtered,
#                                type='response')
#   )

us_flows_treated <- outflows_filtered %>%
  filter(user_cc == 'US')

us_flows_untreated <- outflows_filtered %>%
  filter(user_cc == 'US') %>%
  mutate(us_outflow = 0)

predicted_treated <- predict(es_qmle_highincome,
                     newdata= us_flows_treated,
                     type='response')

counterfactual <- predict(es_qmle_highincome,
                          newdata= us_flows_untreated,
                          type='response')

spillover <- predicted_treated - counterfactual

cumulative_spillover <- cumsum(spillover)

spillover_total <- sum(spillover)

spillover_percentage <- sum(spillover) / sum(counterfactual)

dates <- unique(outflows_filtered$time)

cols <- list(predicted_treated, counterfactual, spillover, cumulative_spillover)

counterfactual_df <- as.data.frame(do.call(cbind, cols))

counterfactual_df$date <- dates

names(counterfactual_df) <- c('treated', 'counterfactual', 'spillover', 'cumulative_spillover', 'date')

counterfactual_plot <- counterfactual_df %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y = treated, color = 'treated')) +
  geom_line(aes(y = counterfactual, color = 'counterfactual'))

show(counterfactual_plot)

spillover_plot <- counterfactual_df %>%
  ggplot(aes(x=date, y = spillover)) +
  geom_line()

show(spillover_plot)

cumulative_spillover_plot <- counterfactual_df %>%
  ggplot(aes(x=date, y = cumulative_spillover)) +
  geom_line()

show(cumulative_spillover_plot)