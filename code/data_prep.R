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

trades <- vroom('../temporary/trades_paxful_cleaned.csv')
trades_matched <- vroom('../temporary/matched_paxful_trades.csv')
trades_matched$date <- as.POSIXct(trades_matched$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
country_data <- read.csv('../temporary/country_data.csv')

#get bilateral flows
flows <- getFlows(trades_matched, amount_usd, 'week') %>% na.omit()

#make balanced
flows_balanced <- balanceFlows(flows)

#join country data
flows_balanced <- flows_balanced

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
  group_by(user_cc) %>%
  mutate(share = volume/sum(volume)) %>%
  select(-c(volume))

#select top destinations
us_top_baseline <- shares_baseline %>%
  filter(user_cc == "US") %>%
  slice_max(share, n = 10) %>%
  pull(user_cc2) 

shares_wide <- shares_baseline %>%
  filter(user_cc2 %in% us_top_baseline) %>%
  pivot_wider(names_from = user_cc2, values_from = share) %>%
  ungroup() %>%
  select(-c(time))

outflows_sdid <- outflows_sdid %>% left_join(shares_wide, by = 'user_cc')

##########Total Volume By Country

total_volume <- trades %>%
  group_by(user_cc) %>%
  summarize(total = sum(amount_usd))

#####US outflows
us_outflows <- flows_balanced %>%
  filter(user_cc == "US" & user_cc2 != "US")

#####US inflows
us_inflows <- flows_balanced %>%
  filter(user_cc != 'US' & user_cc2 == 'US')

###daily outflows
flows_daily <- getFlows(trades_matched, amount_usd, 'day') %>% na.omit() %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2021-01-01'))

flows_daily_balanced <- balanceFlows(flows_daily)

# outflows_daily <- flows_daily_balanced %>%
#   filter(user_cc != user_cc2) %>%
#   group_by(user_cc, time) %>%
#   summarize(outflow = sum(volume))
# 
# outflows_daily <- outflows_daily %>%
#   left_join(country_data, by = c('user_cc2' = 'alpha.2'))

#export data
write.csv(flows_balanced, '../temporary/bilateral_flows_balanced.csv', row.names = FALSE)
write.csv(flows_daily_balanced, '../temporary/bilateral_flows_balanced_daily.csv', row.names = FALSE)
write.csv(us_outflows, '../temporary/us_outflows_balanced.csv', row.names = FALSE)
write.csv(us_inflows, '../temporary/us_inflows_balanced.csv', row.names = FALSE)
write.csv(shares_wide, '../temporary/baseline_shares.csv', row.names = FALSE)
write.csv(outflows, '../temporary/outflows_balanced.csv', row.names = FALSE)
write.csv(outflows_sdid, '../temporary/data_sdid.csv', row.names = FALSE)
write.csv(total_volume, '../temporary/total_volume_by_country.csv', row.names = FALSE)


######Data playground

US_baseline_shares <- shares_baseline %>%
  filter(user_cc == "US") %>%
  arrange(desc(share)) %>%
  mutate(cumulutive = cumsum(share))



