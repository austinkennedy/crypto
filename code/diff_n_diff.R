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

#overall outflows
flows <- trades_matched %>%
  filter(user_cc != user_cc2)

#US outflows
outflows_us <- trades_matched %>%
  filter(user_cc == "US" & user_cc2 != "US")

#get volume
flows_volume <- outflow_volume_country(flows, amount_usd, 'day')

us_outflows_volume <- outflow_volume_country(outflows_us, amount_usd, 'day')

#join crypto and country data
us_outflows_country <- inner_join(us_outflows_volume, country_data, by = c("user_cc2" = "alpha.2"))

flows_country <- inner_join(flows_volume, country_data, by = c("user_cc2" = "alpha.2"))

#add phases
announcement <- as.Date('2020-03-27')

disbursement <- as.Date('2020-04-09')

#add month, year, and post variable

us_outflows_country <- us_outflows_country %>%
  mutate(post = ifelse(time >= disbursement, 1, 0),
         month = month(time),
         year = year(time))

basic_reg_fml <- as.formula('log(volume) ~ log(fb1)*fees_median + log(fb1) + fees_median')

basic_reg <- us_outflows_country %>%
  feols(basic_reg_fml)

summary(basic_reg)

basic_reg_lowermiddle <- us_outflows_country %>%
  filter(income_group %in% c('L', 'LM')) %>%
  feols(basic_reg_fml)

summary(basic_reg_lowermiddle)

basic_reg_developed <- us_outflows_country %>%
  filter(!(income_group %in% c('L', 'LM'))) %>%
  feols(basic_reg_fml)

summary(basic_reg_developed)

fb_did <- as.formula('log(volume) ~ i(post, log(fb1), ref = 0) + i(post, ref = 0) + log(fb1)')

model_fb_full <- us_outflows_country %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fb_did, cluster = 'time')

summary(model_fb_full)

model_fb_lowermiddle <- us_outflows_country %>%
  filter(income_group %in% c('L', 'LM'),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fb_did, cluster = 'time')

summary(model_fb_lowermiddle)

model_fb_developed <- us_outflows_country %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fb_did, cluster = 'time')

summary(model_fb_developed)
  
fee_fml <- as.formula('log(volume) ~ i(post, fees_median, ref = 0) + i(post, ref = 0) + fees_median')

model_fee_full <- us_outflows_country %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fee_fml, cluster = 'time')

summary(model_fee_full)

model_fee_lowermiddle <- us_outflows_country %>%
  filter(income_group %in% c('L', 'LM'),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fee_fml, cluster = 'time')

summary(model_fee_lowermiddle)

model_fee_developed <- us_outflows_country %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(fee_fml, cluster = 'time')

summary(model_fee_developed)

#####Phases

flows_country <- flows_country %>% 
  mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0),
         us_outflow = ifelse(user_cc == "US", 1, 0)
         )

us_outflows_country <- us_outflows_country %>%
  mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0)
         )

phases_flows_fml <- as.formula('log(volume) ~ i(announced, us_outflow, ref = 0) + i(disbursed, us_outflow, ref = 0) + i(announced, ref = 0) + i(disbursed, ref = 0) + i(us_outflow, ref = 0)|time + label')

# phases_flows_fml <- as.formula('log(volume) ~ i(disbursed, us_outflow, ref = 0)|time + label')

model_phases_flows <- flows_country %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_flows_fml, cluster = 'time')

summary(model_phases_flows)

model_phases_flows_lowermiddle <- flows_country %>%
  filter(income_group %in% c('L', 'LM'),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_flows_fml, cluster = 'time')

summary(model_phases_flows_lowermiddle)

model_phases_flows_developed <- flows_country %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_flows_fml, cluster = 'time')

summary(model_phases_flows_developed)

model_phases_flows_highfee <- flows_country %>%
  filter(fees_above == 1,
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_flows_fml, cluster = 'time')

summary(model_phases_flows_highfee)

phases_fb_fml <- as.formula('log(volume) ~ i(announced, log(fb1), ref = 0) + i(disbursed, log(fb1), ref = 0) + i(announced, ref = 0) + i(disbursed, ref = 0) + log(fb1)')

model_phases_fb <- us_outflows_country %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fb_fml, cluster = 'time')

summary(model_phases_fb)

model_phases_fb_lowermiddle <- us_outflows_country %>%
  filter(income_group %in% c('L', 'LM'),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fb_fml, cluster = 'time')

summary(model_phases_fb_lowermiddle)

model_phases_fb_developed <- us_outflows_country %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fb_fml, cluster = 'time')

summary(model_phases_fb_developed)


phases_fees_fml <- as.formula('log(volume) ~ i(announced, fees_median, ref = 0) + i(disbursed, fees_median, ref = 0) + i(announced, ref = 0) + i(disbursed, ref = 0) + fees_median')

model_phases_fees <- us_outflows_country %>%
  filter(time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fees_fml, cluster = 'time')

summary(model_phases_fees)

model_phases_fees_lowermiddle <- us_outflows_country %>%
  filter(income_group %in% c('L', 'LM'),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fees_fml, cluster = 'time')

summary(model_phases_fees_lowermiddle)

model_phases_fees_developed <- us_outflows_country %>%
  filter(!(income_group %in% c('L', 'LM')),
         time >= as.Date('2020-01-01') & time <= as.Date('2020-06-05')) %>%
  feols(phases_fees_fml, cluster = 'time')

summary(model_phases_fees_developed)



#Create and Export Tables

#Get models in list for modelsummary
fb_phases <- list("Full Sample" = model_phases_fb,
                  "Lower and Lower-Middle Income" = model_phases_fb_lowermiddle,
                  "Upper-Middle and High Income" = model_phases_fb_developed)

cm_fb <- c("disbursed::1:log(fb1)" = "$\\text{disbursed} \\times ln(\\text{FB})$",
           "announced::1:log(fb1)" = "$\\text{announced} \\times ln(\\text{FB})$",
           "disbursed::1" = "disbursed",
           "announced::1" = "announced",
           "log(fb1)" = "$ln(\\text{FB})$")

gof_omitted_fb <- "AIC|BIC|RMSE|Std.Errors"

note_fb <- "Standard errors clustered at the day level."

coef_omitted_fb <- "(Intercept)"

modelsummary(fb_phases,
                       stars = TRUE,
                       coef_map = cm_fb,
                       coef_omit = coef_omitted_fb,
                       gof_omit = gof_omitted_fb,
                       title = "Dependent Variable: $ln(\\text{Volume})$",
                       escape = FALSE,
                       threeparttable = TRUE,
                       notes = note_fb,
                       output = '../output/regression_tables/phases_fb.tex')

# fb_tab <- modelsummary(fb_phases,
#              stars = TRUE,
#              coef_map = cm_fb,
#              coef_omit = coef_omitted,
#              gof_omit = gof_omitted,
#              title = "Dependent Variable: $ln(\\text{Volume})$",
#              escape = FALSE,
#              threeparttable = TRUE,
#              notes = note,
#              output = 'latex') %>%
#   column_spec(1:5,width = "1.5in")
# 
# kableExtra::save_kable(fb_tab, file = '../output/regression_tables/phases_fb.tex')

fee_phases <- list("Full Sample" = model_phases_fees,
                   "Low and Middle Income" = model_phases_fees_lowermiddle,
                   "High Income" = model_phases_fees_developed)

cm_fees <- c("disbursed::1:fees_median" = "$\\text{disbursed} \\times \\text{Fee}$",
           "announced::1:fees_median" = "$\\text{announced} \\times \\text{Fee}$",
           "disbursed::1" = "disbursed",
           "announced::1" = "announced",
           "fees_median" = "$\\text{Fee}$")

gof_omitted_fees <- "AIC|BIC|RMSE|Std.Errors"

note_fees <- "Standard errors clustered at the day level."

coef_omitted_fees <- "(Intercept)"

modelsummary(fee_phases,
                       stars = TRUE,
                       coef_map = cm_fees,
                       coef_omit = coef_omitted_fees,
                       gof_omit = gof_omitted_fees,
                       title = "Dependent Variable: $ln(\\text{Volume})$",
                       escape = FALSE,
                       threeparttable = TRUE,
                       notes = note_fees,
                       output = '../output/regression_tables/phases_fees.tex')





















