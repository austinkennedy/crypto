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

source('functions.R')

#load matched trades
trades_matched <- vroom('../temporary/matched_paxful_trades.csv')
trades_matched$date <- as.POSIXct(trades_matched$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
country_data <- read.csv('../temporary/country_data.csv')

#US outflows
outflows_us <- trades_matched %>%
  filter(user_cc == "US" & user_cc2 != "US")


#get volume
volume_country <- outflow_volume_country(outflows_us, amount_usd, 'day')

#join crypto and country data
df <- inner_join(volume_country, country_data, by = c("user_cc2" = "alpha.2"))

#add phases
announcement <- as.Date('2020-03-27')

disbursement <- as.Date('2020-04-09')

#add month, year, and post variable

df <- df %>%
  mutate(post = ifelse(time >= disbursement, 1, 0),
         month = month(time),
         year = year(time))

basic_reg_fml <- as.formula('log(volume) ~ log(fb1)*fees_median + log(fb1) + fees_median')

basic_reg <- df %>%
  feols(basic_reg_fml)

summary(basic_reg)

basic_reg_lowermiddle <- df %>%
  filter(income_group %in% c('L', 'LM')) %>%
  feols(basic_reg_fml)

summary(basic_reg_lowermiddle)

basic_reg_developed <- df %>%
  filter(!(income_group %in% c('L', 'LM'))) %>%
  feols(basic_reg_fml)

summary(basic_reg_developed)

fb_did <- as.formula('log(volume) ~ i(post, log(fb1), ref = 0) + i(post, ref = 0) + log(fb1)')

model_fb_full <- df %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fb_did, cluster = 'time')

summary(model_fb_full)

model_fb_lowermiddle <- df %>%
  filter(income_group %in% c('L', 'LM'),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fb_did, cluster = 'time')

summary(model_fb_lowermiddle)

model_fb_developed <- df %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fb_did, cluster = 'time')

summary(model_fb_developed)
  
fee_fml <- as.formula('log(volume) ~ i(post, fees_median, ref = 0) + i(post, ref = 0) + fees_median')

model_fee_full <- df %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fee_fml, cluster = 'time')

summary(model_fee_full)

model_fee_lowermiddle <- df %>%
  filter(income_group %in% c('L', 'LM'),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fee_fml, cluster = 'time')

summary(model_fee_lowermiddle)

model_fee_developed <- df %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fee_fml, cluster = 'time')

summary(model_fee_developed)

#####Phases




df <- df %>%
  mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0))

phases_fb_fml <- as.formula('log(volume) ~ i(announced, log(fb1), ref = 0) + i(disbursed, log(fb1), ref = 0) + i(announced, ref = 0) + i(disbursed, ref = 0) + log(fb1)')

model_phases_fb <- df %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fb_fml, cluster = 'time')

summary(model_phases_fb)

model_phases_fb_lowermiddle <- df %>%
  filter(income_group %in% c('L', 'LM' ),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fb_fml, cluster = 'time')

summary(model_phases_fb_lowermiddle)

model_phases_fb_developed <- df %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fb_fml, cluster = 'time')

summary(model_phases_fb_developed)

#Get models in list for modelsummary

fb_phases <- list("Full Sample" = model_phases_fb,
                  "Lower and Lower-Middle Income" = model_phases_fb_lowermiddle,
                  "Upper-Middle and High Income" = model_phases_fb_developed)


phases_fees_fml <- as.formula('log(volume) ~ i(announced, fees_median, ref = 0) + i(disbursed, fees_median, ref = 0) + i(announced, ref = 0) + i(disbursed, ref = 0) + fees_median')

model_phases_fees <- df %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fees_fml, cluster = 'time')

summary(model_phases_fees)

model_phases_fees_lowermiddle <- df %>%
  filter(income_group %in% c('L', 'LM'),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fees_fml, cluster = 'time')

summary(model_phases_fees_lowermiddle)

model_phases_fees_developed <- df %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fees_fml, cluster = 'time')

summary(model_phases_fees_developed)

fee_phases <- list("Full Sample" = model_phases_fees,
                   "Lower and Lower-Middle Income" = model_phases_fees_lowermiddle,
                   "Upper-Middle and High Income" = model_phases_fees_developed)

#Create and Export Tables

cm_fb <- c("disbursed::1:log(fb1)" = "$\\text{disbursed} \\times ln(\\text{FB})$",
           "announced::1:log(fb1)" = "$\\text{announced} \\times ln(\\text{FB})$",
           "disbursed::1" = "disbursed",
           "announced::1" = "announced",
           "log(fb1)" = "$ln(\\text{FB})$")

gof_omitted <- "AIC|BIC|RMSE|Std.Errors"

note <- "Standard errors clustered at the day level."

coef_omitted <- "(Intercept)"

fb_tab <- modelsummary(fb_phases,
                       stars = TRUE,
                       coef_map = cm_fb,
                       coef_omit = coef_omitted,
                       gof_omit = gof_omitted,
                       title = "Dependent Variable: $ln(\\text{Volume})$",
                       escape = FALSE,
                       threeparttable = TRUE,
                       notes = note,
                       output = '../output/regression_tables/phases_fb.tex')

fb_tab <- modelsummary(fb_phases,
             stars = TRUE,
             coef_map = cm_fb,
             coef_omit = coef_omitted,
             gof_omit = gof_omitted,
             title = "Dependent Variable: $ln(\\text{Volume})$",
             escape = FALSE,
             threeparttable = TRUE,
             notes = note,
             output = 'latex') %>%
  column_spec(1:5,width = "1.5in")

kableExtra::save_kable(fb_tab, file = '../output/regression_tables/phases_fb.tex')






















