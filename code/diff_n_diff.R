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

source('functions.R')

#load matched trades
trades_matched <- vroom('../temporary/matched_paxful_trades.csv')
trades_matched$date <- as.POSIXct(trades_matched$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
country_data <- read.csv('../temporary/country_data.csv')

#overall outflows
flows <- trades_matched %>%
  filter(user_cc != user_cc2)

flows <- inner_join(flows, country_data, by = c("user_cc2" = "alpha.2"))

#US outflows
outflows_us <- trades_matched %>%
  filter(user_cc == "US" & user_cc2 != "US")

#get volume
flows_volume <- outflow_volume_country(flows, amount_usd, 'week')

us_outflows_volume <- outflow_volume_country(outflows_us, amount_usd, 'day')

#join crypto and country data
us_outflows_country <- inner_join(us_outflows_volume, country_data, by = c("user_cc2" = "alpha.2"))

flows_country <- inner_join(flows_volume, country_data, by = c("user_cc2" = "alpha.2"))

#add phases
announcement <- as.Date('2020-03-27')

disbursement <- as.Date('2020-04-09')


#######################Outflows only###################

#make balanced
dates <- unique(flows_country$time)
countries <- unique(flows_country$user_cc)

window_start <- as.Date('2020-01-01')
window_end <- as.Date('2020-06-07')

panel <- as_tibble(CJ(dates, countries)) %>% rename(time = dates, user_cc = countries)

#get total outflows by source country, to varying country groups
outflows_all <- flows_country %>%
  group_by(user_cc, time) %>%
  summarize(volume = sum(volume))

outflows_lm <- flows_country %>%
  filter(income_group %in% c('L','LM')) %>%
  group_by(user_cc, time) %>%
  summarize(volume = sum(volume))  

outflows_um <- flows_country %>%
  filter(income_group %in% c('UM','H')) %>%
  group_by(user_cc, time) %>%
  summarize(volume = sum(volume))

#create balanced panels

outflows_all <- outflows_all %>%
  right_join(panel, by = c('time', 'user_cc')) %>%
  replace(is.na(.), 0)

outflows_lm <- outflows_lm %>%
  right_join(panel, by = c('time', 'user_cc')) %>%
  replace(is.na(.), 0)

outflows_um <- outflows_um %>%
  right_join(panel, by = c('time', 'user_cc')) %>%
  replace(is.na(.), 0)

#add treatment dates
outflows_all <- outflows_all %>% mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
       disbursed = ifelse(time >= disbursement, 1, 0),
       us_outflow = ifelse(user_cc == "US", 1, 0)
)

outflows_lm <- outflows_lm %>% mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
                                      disbursed = ifelse(time >= disbursement, 1, 0),
                                      us_outflow = ifelse(user_cc == "US", 1, 0)
)

outflows_um <- outflows_um %>% mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
                                      disbursed = ifelse(time >= disbursement, 1, 0),
                                      us_outflow = ifelse(user_cc == "US", 1, 0)
)

#CLUSTERING LEVEL
cluster_level_spillovers <- c('user_cc')

spillovers_model <- function(df, yvar){
  reg <- df %>%
    filter(time >= window_start & time <= window_end) %>%
    feols(.[yvar] ~ disbursed*us_outflow + announced*us_outflow|time + user_cc, cluster = cluster_level_spillovers)

  return(reg)
}


did_all_levels <- spillovers_model(outflows_all, yvar = 'volume')

summary(did_all_levels)

did_lm_levels <- spillovers_model(outflows_lm, yvar = 'volume')

summary(did_lm_levels)

did_um_levels <- spillovers_model(outflows_um, yvar = 'volume')

summary(did_um_levels)

# baseline <- outflows %>%
#   filter(time >= as.Date('2020-01-01') & time <= disbursement,
#          user_cc == 'US') %>%
#   summarize(mean = mean(volume))

did_all_asinh <- spillovers_model(outflows_all, yvar = 'asinh(volume)')

summary(did_all_asinh)

did_lm_asinh <- spillovers_model(outflows_lm, yvar = 'asinh(volume)')

summary(did_lm_asinh)

did_um_asinh <- spillovers_model(outflows_um, yvar = 'asinh(volume)')

summary(did_um_asinh)

####EVENTSTUDY

es_model <- function(df, yvar){
    reg <- df %>%
      filter(time >= window_start & time <= window_end) %>%
      feols(.[yvar] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers)
    
    return(reg)
}

es_all_levels <- es_model(outflows_all, yvar = 'volume')

iplot(es_all_levels)

es_lm_levels <- es_model(outflows_lm, yvar = 'volume')

iplot(es_lm_levels)

es_um_levels <- es_model(outflows_um, yvar = 'volume')

iplot(es_um_levels)

es_all_asinh <- es_model(outflows_all, yvar = 'asinh(volume)')

iplot(es_all_asinh)

es_lm_asinh <- es_model(outflows_lm, yvar = 'asinh(volume)')

iplot(es_lm_asinh)

es_um_asinh <- es_model(outflows_um, yvar = 'asinh(volume)')

iplot(es_um_asinh)


##########US-only outflows

#add anounced and disbursed variables

us_outflows_country <- us_outflows_country %>% mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
       disbursed = ifelse(time >= disbursement, 1, 0))

vcov_us <- 'cluster'
#levels

fb_reg_fml_levels <- as.formula('volume ~ i(disbursed, asinh(fb1), ref = 0)|time + user_cc2')

fb_reg_all_levels <- us_outflows_country %>%
  filter(time >= window_start & time <= window_end) %>%
  feols(fb_reg_fml_levels, vcov = vcov_us)

summary(fb_reg_all_levels)

fb_reg_lm_levels <- us_outflows_country %>%
  filter(income_group %in% c('L', 'LM'),
         time >= window_start & time <= window_end) %>%
  feols(fb_reg_fml_levels, vcov = vcov_us)

summary(fb_reg_lm_levels)

fb_reg_um_levels <- us_outflows_country %>%
  filter(income_group %in% c('UM', 'H'),
         time >= window_start & time <= window_end) %>%
  feols(fb_reg_fml_levels, vcov = vcov_us)

summary(fb_reg_um_levels)

fb_reg_fml_asinh <- as.formula('log(volume) ~ i(disbursed, log(fb1), ref = 0)|time + user_cc2')

fb_reg_all_asinh <- us_outflows_country %>%
  filter(time >= window_start & time <= window_end) %>%
  feols(fb_reg_fml_asinh, vcov = vcov_us)

summary(fb_reg_all_asinh)

fb_reg_lm_asinh <- us_outflows_country %>%
  filter(income_group %in% c('L', 'LM'),
         time >= window_start & time <= window_end) %>%
  feols(fb_reg_fml_asinh, vcov = vcov_us)

summary(fb_reg_lm_asinh)

fb_reg_um_asinh <- us_outflows_country %>%
  filter(income_group %in% c('UM', 'H'),
         time >= window_start & time <= window_end) %>%
  feols(fb_reg_fml_asinh, vcov = vcov_us)

summary(fb_reg_um_asinh)













