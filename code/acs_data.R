#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(tidycensus)
library(lubridate)
library(fuzzyjoin)
source("functions.R")


census_api_key("98c004c5e091cec7290cfdb00ae23e076895c593")

fb_1yr <- get_clean_fb_acs(yr = 2019, type = "acs1")

codes <- read.csv("../input/country_codes_alpha_2.csv")



codes$Name <- gsub("(.*),.*", "\\1", codes$Name)

country_merge <- stringdist_join(fb_1yr, codes,
                                 by = c("label" = "Name"),
                                 mode = "left",
                                 method = "jw",
                                 max_dist = 0.1,
                                 distance_col = 'dist') %>%
  group_by(Name) %>%
  slice_min(order_by = dist, n=1)

cats <- subset(fb_1yr, grepl("^.+:$", label))

country_merge_anti <- stringdist_join(fb_1yr, codes,
                                       by = c('label' = 'Name'),
                                       mode = "anti",
                                       method = "jw",
                                       max_dist = 0.1,
                                       distance_col = 'dist') %>%
  group_by(Name) %>%
  slice_min(order_by = dist, n=1)

left_joined <- fb_1yr %>%
  stringdist_left_join(codes, by = c(label = "Name"), max_dist = 1) %>%
  filter(is.na(Name))









