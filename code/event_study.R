#clear memory and setup
rm(list=ls())
options(scipen=999)

#packages
library(tidyverse)
library(vroom)
library(lubridate)
library(fuzzyjoin)

#load matched trades
trades_matched <- vroom('../temporary/matched_paxful_trades.csv')
trades_matched$date <- as.POSIXct(trades_matched$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
acs <- read.csv("../temporary/acs_foreignborn_2019_cleaned.csv")
codes <- read.csv("../input/country_codes_alpha_2.csv")

#US outflows
outflows_us <- trades_matched %>%
  filter(user_cc == "US" & user_cc2 != "US")

#functions

get_total_volume <- function(data, unit, interval){
#get volume at different intervals
#'data' should be a dataframe of matched or unmatched crypto trades
#'unit' indicates the desired currency, right now either "amount" (BTC) or "amount_usd" (USD)
#'interval' indicates the desired interval. Provide a string such as "day", "week", "month", etc
  df <- data %>%
    group_by(time = as.Date(floor_date(date, interval))) %>%
    summarise(volume = sum({{unit}}))
  
  return(df)
}

outflow_volume_total <- function(data, unit, interval){
  #get volume at different intervals
  #'data' should be a dataframe of matched or unmatched crypto trades
  #'unit' indicates the desired currency, right now either "amount" (BTC) or "amount_usd" (USD)
  #'interval' indicates the desired interval. Provide a string such as "day", "week", "month", etc
  df <- data %>%
    group_by(time = as.Date(floor_date(date, interval))) %>%
    summarise(volume = sum({{unit}}))
  
  return(df)
}

outflow_volume_country <- function(data, unit, interval){
  #get volume at different intervals, by receiving country
  #'data' should be a dataframe of matched or unmatched crypto trades
  #'unit' indicates the desired currency, right now either "amount" (BTC) or "amount_usd" (USD)
  #'interval' indicates the desired interval. Provide a string such as "day", "week", "month", etc
  df <- data %>%
    group_by(time = as.Date(floor_date(date, interval)),
             user_cc2) %>%
    summarise(volume = sum({{unit}}))
  
  return(df)
}



weekly <- outflow_volume_total(outflows_us, amount_usd, 'week')

weekly_country <- outflow_volume_country(outflows_us, amount_usd, 'week')

weekly_country <- left_join(weekly_country, codes, by = c("user_cc2" = "Code"))

weekly_country <- left_join(weekly_country, acs, by = c("Name" = "country"))


############Playground

codes$Name <- gsub("(.*),.*", "\\1", codes$Name)

country_merge <- stringdist_join(acs, codes,
                                 by = c("country" = "Name"),
                                 mode = "left",
                                 method = "jw",
                                 max_dist = 0.1,
                                 distance_col = 'dist') %>%
  group_by(Name) %>%
  slice_min(order_by = dist, n=1)




