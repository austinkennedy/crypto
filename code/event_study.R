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

#create treated vs. untreated
country_data <- country_data %>%
  mutate(treat = ifelse(fb1_per1000 >= median(sort(fb1_per1000)), 1,0))

#US outflows
outflows_us <- trades_matched %>%
  filter(user_cc == "US" & user_cc2 != "US")


#get weekly volume
weekly_country <- outflow_volume_country(outflows_us, amount_usd, 'week')

#join crypto and foreign-born data
df <- inner_join(weekly_country, country_data, by = c("user_cc2" = "alpha.2"))

#add treatment and pre-post
treatment <- as.Date('2020-04-01')

df <- df %>%
  mutate(post = ifelse(time >= treatment, 1, 0),
         time_to_treat = as.numeric(round(difftime(time, treatment, units = 'weeks'))))

est_did <- df %>%
  filter(time_to_treat < 30 & time_to_treat > -30, user_cc2 != 'NG') %>%
  feols(volume ~ i(time_to_treat, treat, ref = 0)|label + time_to_treat + region)

summary(est_did)

iplot(est_did)





############Playground

top_outflows <- df %>%
  group_by(label) %>%
  summarise(sum(volume))








total_volume <- df %>%
  group_by(treat, time) %>%
  summarise(volume = sum(volume))

stimulus1 <- which(total_volume$time %in% as.Date(c("2020-04-05")))
stimulus2 <- which(total_volume$time %in% as.Date(c("2021-01-03")))
stimulus3 <- which(total_volume$time %in% as.Date(c("2021-03-07")))

ggplot(total_volume, aes(x=time, y=volume, group = treat, color = factor(treat))) +
  # geom_line(color = "blue", size = 0.8) +
  geom_line() +
  geom_vline(xintercept = as.numeric(total_volume$time[stimulus1]), color='red', size =0.8) +
  geom_vline(xintercept = as.numeric(total_volume$time[stimulus2]), color = 'red', size = 0.8) +
  geom_vline(xintercept = as.numeric(total_volume$time[stimulus3]), color = 'red', size = 0.8) +
  xlab("time") +
  ylab("USD") +   
  # ylim(0,40000) +
  # scale_x_date(limit=c(as.Date("2021-09-01"), as.Date("2022-05-01"))) +
  ggtitle('Outflows from US') +
  theme_bw() +
  theme(plot.title = element_text(size = 15, hjust = 0.5))


total_volume <- df %>%
  filter(treat == 1) %>%
  group_by(time) %>%
  summarise(volume = sum(volume))

stimulus1 <- which(total_volume$time %in% as.Date(c("2020-04-05")))
stimulus2 <- which(total_volume$time %in% as.Date(c("2021-01-03")))
stimulus3 <- which(total_volume$time %in% as.Date(c("2021-03-07")))

ggplot(total_volume, aes(x=time, y=volume)) +
  # geom_line(color = "blue", size = 0.8) +
  geom_line() +
  geom_vline(xintercept = as.numeric(total_volume$time[stimulus1]), color='red', size =0.8) +
  geom_vline(xintercept = as.numeric(total_volume$time[stimulus2]), color = 'red', size = 0.8) +
  geom_vline(xintercept = as.numeric(total_volume$time[stimulus3]), color = 'red', size = 0.8) +
  xlab("time") +
  ylab("USD") +   
  # ylim(0,40000) +
  # scale_x_date(limit=c(as.Date("2021-09-01"), as.Date("2022-05-01"))) +
  ggtitle('Outflows from US') +
  theme_bw() +
  theme(plot.title = element_text(size = 15, hjust = 0.5))




total_volume <- df %>%
  filter(treat == 0) %>%
  group_by(time) %>%
  summarise(volume = sum(volume))

stimulus1 <- which(total_volume$time %in% as.Date(c("2020-04-05")))
stimulus2 <- which(total_volume$time %in% as.Date(c("2021-01-03")))
stimulus3 <- which(total_volume$time %in% as.Date(c("2021-03-07")))

ggplot(total_volume, aes(x=time, y=volume)) +
  # geom_line(color = "blue", size = 0.8) +
  geom_line() +
  geom_vline(xintercept = as.numeric(total_volume$time[stimulus1]), color='red', size =0.8) +
  geom_vline(xintercept = as.numeric(total_volume$time[stimulus2]), color = 'red', size = 0.8) +
  geom_vline(xintercept = as.numeric(total_volume$time[stimulus3]), color = 'red', size = 0.8) +
  xlab("time") +
  ylab("USD") +   
  # ylim(0,40000) +
  # scale_x_date(limit=c(as.Date("2021-09-01"), as.Date("2022-05-01"))) +
  ggtitle('Outflows from US') +
  theme_bw() +
  theme(plot.title = element_text(size = 15, hjust = 0.5))















