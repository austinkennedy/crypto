#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(tidycensus)
library(lubridate)
library(fuzzyjoin)
library(stringr)

source("functions.R")


census_api_key("98c004c5e091cec7290cfdb00ae23e076895c593")

fb_1yr <- get_clean_fb_acs(yr = 2019, type = "acs1")

codes <- read.csv("../input/country_codes_alpha_2.csv")

crypto_cc <- read.csv('../temporary/paxful_cc.csv')

codes$Name <- case_match(codes$Name,
              "Congo, the Democratic Republic of the" ~ "Democratic Republic of Congo",
              .default = codes$Name
             )
#Remove things like ', Republic of' that are not in the ACS country names
codes$Name <- gsub("(.*),.*", "\\1", codes$Name)

codes$Name <- str_replace(codes$Name, "Saint", "St.")

#Merge crypto 2-digit with country names

codes_merged <- crypto_cc %>% inner_join(codes, by = c("cc" = "Code"))

codes_merged <- codes_merged %>% filter(!(Name == "Niger"))

#manually change some labels that don't do well in fuzzy matching (arggg why does ACS not use standardized country names??)
fb_1yr$label <- case_match(fb_1yr$label,
                           "United Kingdom (inc. Crown Dependencies):" ~ "United Kingdom",
                           "North Macedonia (Macedonia)" ~ "Macedonia",
                           "Russia" ~ "Russian Federation",
                           "Burma" ~ "Myanmar",
                           "Laos" ~ "Lao People's Democratic Republic",
                           "Vietnam" ~ "Viet Nam",
                           "Syria" ~ "Syrian Arab Republic",
                           "Cabo Verde" ~ "Cape Verde",
                           "China, excluding Hong Kong and Taiwan" ~ "China",
                           .default = fb_1yr$label
                           )

country_merge <- stringdist_join(fb_1yr, codes_merged,
                                 by = c("label" = "Name"),
                                 mode = "left",
                                 method = "jw",
                                 max_dist = 0.1,
                                 distance_col = 'dist') %>%
  group_by(Name) %>%
  slice_min(order_by = dist, n=1) %>%
  drop_na() %>%
  ungroup()

country_merge <- country_merge %>%
  select(-c(Name, dist)) %>%
  relocate(label)


#####Playground

cats <- subset(fb_1yr, grepl("^.+:$", label))

country_merge_anti <- stringdist_join(fb_1yr, codes_merged,
                                 by = c("label" = "Name"),
                                 mode = "anti",
                                 method = "jw",
                                 max_dist = 0.1,
                                 distance_col = 'dist')

left_joined <- fb_1yr %>%
  stringdist_left_join(codes_merged, by = c(label = "Name"), max_dist = 1) %>%
  filter(is.na(Name))









