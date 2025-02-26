#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(vroom)
library(lubridate)
library(gt)
library(urca)
source('functions.R')

trades <- vroom('../temporary/trades_paxful_cleaned.csv')

volume_price <- getVolumePrice(trades, amount_usd, 'week')

structural_test <- ur.za(volume_price$volume, model = 'intercept')

summary(structural_test)