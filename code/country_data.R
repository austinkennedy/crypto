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

#Data
fb_1yr <- get_clean_fb_acs(yr = 2019, type = "acs1")
fb_5yr <- get_clean_fb_acs(yr = 2019, type =  "acs5")
codes <- read.csv("../input/country_codes_alpha_2.csv")
crypto_cc <- read.csv('../temporary/paxful_cc.csv')
cc_all <- read.csv('../input/all_cc_codes.csv')
pop <- read.csv('../input/un_pop.csv')



fb_1yr <- fb_1yr %>%
  rename(c(fb1 = 'estimate', moe1 = 'moe'))

fb_5yr <- fb_5yr %>%
  rename(c(fb5 = 'estimate', moe5 = 'moe'))
#merge acs estimates
fb <- inner_join(fb_1yr, fb_5yr, by = 'label') %>%
  select(-c(geography)) %>%
  relocate(label)




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
fb$label <- case_match(fb$label,
                           "United Kingdom (inc. Crown Dependencies):" ~ "United Kingdom",
                           "North Macedonia (Macedonia)" ~ "Macedonia",
                           "Russia" ~ "Russian Federation",
                           "Burma" ~ "Myanmar",
                           "Laos" ~ "Lao People's Democratic Republic",
                           "Vietnam" ~ "Viet Nam",
                           "Syria" ~ "Syrian Arab Republic",
                           "Cabo Verde" ~ "Cape Verde",
                           "China, excluding Hong Kong and Taiwan" ~ "China",
                           .default = fb$label
                           )

country_merge <- stringdist_join(fb, codes_merged,
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


#merge with rest of cc data
country_merge_all <- country_merge %>% inner_join(cc_all, by = c("cc" = "alpha.2"), keep = TRUE)

#drop unnecessary columns
country_merge_all <- country_merge_all %>%
  select(-c(iso_3166.2, name, cc))

#population data
pop <- pop %>%
  filter(Time == 2019, LocTypeName == 'Country/Area') %>%
  select(c(ISO3_code, Time, PopTotal, Location))

# pop$PopTotal <- pop$PopTotal * 1000 #foreign-born is per person

country_data <- inner_join(country_merge_all, pop, by = c("alpha.3" = "ISO3_code")) %>%
  select(-c(Time, Location)) %>%
  mutate(fb1_per1000 = fb1 / PopTotal, #foreign born per capita
         fb5_per1000 = fb5 / PopTotal)

#export
write.csv(country_data, '../temporary/country_data.csv')









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









