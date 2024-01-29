#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(vroom)
library(lubridate)
library(data.table)
# library(abind)
source('functions.R')

#global options
baseline <- '2019-01-01'

trades_matched <- vroom('../temporary/matched_paxful_trades.csv')
trades_matched$date <- as.POSIXct(trades_matched$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#get bilateral flows
flows <- getFlows(trades_matched, amount_usd, 'week') %>% na.omit()

#make balanced
flows_balanced <- balanceFlows(flows)

#aggregate into total outflows, by origin country & time

outflows <- flows_balanced %>%
  filter(user_cc != user_cc2) %>%
  group_by(user_cc, time) %>%
  summarize(outflow = sum(volume))

###########prepare SDID data
outflows_sdid <- data.frame(outflows)
outflows_sdid$time_number <- as.numeric(as.factor(outflows$time))
outflows_sdid$country_number <- as.numeric(as.factor(outflows$user_cc))

#get baseline shares
outflows_baseline <- getFlows(trades_matched, amount_usd, 'year') %>%
  filter(user_cc != user_cc2,
         time == baseline)


outflows_baseline <- balanceFlows(outflows_baseline)

shares_baseline <- outflows_baseline %>%
  group_by(time, user_cc) %>%
  mutate(share = volume/sum(volume)) %>%
  select(-c(volume))

shares_wide <- shares_baseline %>%
  pivot_wider(names_from = user_cc2, values_from = share) %>%
  ungroup() %>%
  select(-c(time))

outflows_sdid <- outflows_sdid %>% left_join(shares_wide, by = 'user_cc')

#export data
write.csv(flows_balanced, '../temporary/bilateral_flows_balanced.csv', row.names = FALSE)
write.csv(outflows, '../temporary/outflows_balanced.csv', row.names = FALSE)
write.csv(outflows_sdid, '../temporary/data_sdid.csv', row.names = FALSE)