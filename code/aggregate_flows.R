#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(vroom)
library(lubridate)
library(data.table)
# library(abind)
source('functions.R')

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

#prepare SDID data
outflows$time_number <- as.numeric(as.factor(outflows$time))
outflows$country_number <- as.numeric(as.factor(outflows$user_cc))

#export data
write.csv(flows_balanced, '../temporary/flows_balanced.csv', row.names = FALSE)
write.csv(outflows, '../temporary/outflows_balanced.csv', row.names = FALSE)