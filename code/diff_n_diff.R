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
library(data.table)
library(ggiplot)

source('functions.R')

####OPTIONS

#trade aggregation level, should be 'day' or 'week'
agg_level <- 'week'

#add phases
announcement <- as.Date('2020-03-27')

disbursement <- as.Date('2020-04-09')

window_start <- as.Date('2020-01-01')
window_end <- as.Date('2020-06-07')


#load matched trades
trades_matched <- vroom('../temporary/matched_paxful_trades.csv')
trades_matched$date <- as.POSIXct(trades_matched$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
country_data <- read.csv('../temporary/country_data.csv')

#exclude other countries that have stimulus
country_data <- country_data %>%
  filter(!alpha.2 %in% c('JP', 'KR', 'SG'))

#overall outflows
flows <- trades_matched %>%
  filter(user_cc != user_cc2)

flows <- inner_join(flows, country_data, by = c("user_cc2" = "alpha.2"))

#US outflows
outflows_us <- trades_matched %>%
  filter(user_cc == "US" & user_cc2 != "US")

#get volume
flows_volume <- outflow_volume_country(flows, amount_usd, agg_level)

us_outflows_volume <- outflow_volume_country(outflows_us, amount_usd, agg_level)

#join crypto and country data
us_outflows_country <- inner_join(us_outflows_volume, country_data, by = c("user_cc2" = "alpha.2"))

flows_country <- inner_join(flows_volume, country_data, by = c("user_cc2" = "alpha.2"))


#######################Outflows only###################

#make balanced
dates <- unique(flows_country$time)
countries <- unique(flows_country$user_cc)

panel <- as_tibble(CJ(dates, countries)) %>% rename(time = dates, user_cc = countries)

#get total outflows by source country, to varying country groups
outflows_all <- flows_country %>%
  group_by(user_cc, time) %>%
  summarize(volume = sum(volume))

outflows_lm <- flows_country %>%
  filter(income_group %in% c('L','LM')) %>%
  group_by(user_cc, time) %>%
  summarize(volume = sum(volume))  

outflows_um <- flows_country %>%
  filter(income_group %in% c('UM','H')) %>%
  group_by(user_cc, time) %>%
  summarize(volume = sum(volume))

#create balanced panels

outflows_all <- outflows_all %>%
  right_join(panel, by = c('time', 'user_cc')) %>%
  replace(is.na(.), 0)

outflows_lm <- outflows_lm %>%
  right_join(panel, by = c('time', 'user_cc')) %>%
  replace(is.na(.), 0)

outflows_um <- outflows_um %>%
  right_join(panel, by = c('time', 'user_cc')) %>%
  replace(is.na(.), 0)

#add treatment dates
outflows_all <- outflows_all %>% mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
       disbursed = ifelse(time >= disbursement, 1, 0),
       us_outflow = ifelse(user_cc == "US", 1, 0)
)

outflows_lm <- outflows_lm %>% mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
                                      disbursed = ifelse(time >= disbursement, 1, 0),
                                      us_outflow = ifelse(user_cc == "US", 1, 0)
)

outflows_um <- outflows_um %>% mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
                                      disbursed = ifelse(time >= disbursement, 1, 0),
                                      us_outflow = ifelse(user_cc == "US", 1, 0)
)

#CLUSTERING LEVEL
cluster_level_spillovers <- c('user_cc')

spillovers_model <- function(df, yvar){
  reg <- df %>%
    filter(time >= window_start & time <= window_end) %>%
    feols(.[yvar] ~ disbursed*us_outflow + announced*us_outflow, cluster = cluster_level_spillovers)

  return(reg)
}


did_all_levels <- spillovers_model(outflows_all, yvar = 'volume')

summary(did_all_levels)

did_lm_levels <- spillovers_model(outflows_lm, yvar = 'volume')

summary(did_lm_levels)

did_um_levels <- spillovers_model(outflows_um, yvar = 'volume')

summary(did_um_levels)

# baseline <- outflows %>%
#   filter(time >= as.Date('2020-01-01') & time <= disbursement,
#          user_cc == 'US') %>%
#   summarize(mean = mean(volume))

did_all_asinh <- spillovers_model(outflows_all, yvar = 'asinh(volume)')

summary(did_all_asinh)

did_lm_asinh <- spillovers_model(outflows_lm, yvar = 'asinh(volume)')

summary(did_lm_asinh)

did_um_asinh <- spillovers_model(outflows_um, yvar = 'asinh(volume)')

summary(did_um_asinh)

####EVENTSTUDY

es_model <- function(df, yvar){
    reg <- df %>%
      filter(time >= window_start & time <= window_end) %>%
      feols(.[yvar] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers)
    
    return(reg)
}

es_all_levels <- es_model(outflows_all, yvar = 'volume')

iplot(es_all_levels)

es_lm_levels <- es_model(outflows_lm, yvar = 'volume')

iplot(es_lm_levels)

es_um_levels <- es_model(outflows_um, yvar = 'volume')

iplot(es_um_levels)

es_all_asinh <- es_model(outflows_all, yvar = 'asinh(volume)')

iplot(es_all_asinh)

es_lm_asinh <- es_model(outflows_lm, yvar = 'asinh(volume)')

iplot(es_lm_asinh)

es_um_asinh <- es_model(outflows_um, yvar = 'asinh(volume)')

iplot(es_um_asinh)


##########US-only outflows

#add announced and disbursed variables

us_outflows_country <- us_outflows_country %>% mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
       disbursed = ifelse(time >= disbursement, 1, 0))

#Create lm and um datasets

us_outflows_lm <- us_outflows_country %>% filter(income_group %in% c('L', 'LM'))

us_outflows_um <- us_outflows_country %>% filter(income_group %in% c('UM', 'H'))

fb_model <- function(df, yvar){
  reg <- df %>%
    filter(time >= window_start & time <= window_end) %>%
    feols(.[yvar] ~ i(disbursed, asinh(fb1), ref = 0)|time + user_cc2,
                      cluster = c('user_cc2'))
  
}

fb_reg_all_levels <- fb_model(us_outflows_country, yvar = 'volume')

summary(fb_reg_all_levels)

fb_reg_lm_levels <- fb_model(us_outflows_lm, yvar = 'volume')

summary(fb_reg_lm_levels)

fb_reg_um_levels <- fb_model(us_outflows_um, yvar = 'volume')

summary(fb_reg_um_levels)

fb_reg_all_asinh <- fb_model(us_outflows_country, yvar = 'asinh(volume)')

summary(fb_reg_all_asinh)

fb_reg_lm_asinh <- fb_model(us_outflows_lm, yvar = 'asinh(volume)')

summary(fb_reg_lm_asinh)

fb_reg_um_asinh <- fb_model(us_outflows_um, yvar = 'asinh(volume)')

summary(fb_reg_um_asinh)

#TABLES

#collect models
spillovers_levels <- list("Full Sample" = did_all_levels,
                          "Low and Lower-Middle Income" = did_lm_levels,
                          "Upper-Middle and High Income" = did_um_levels)

spillovers_asinh <- list("Full Sample" = did_all_asinh,
                         "Low and Lower-Middle Income" = did_lm_asinh,
                         "Upper-Middle and High Income" = did_um_asinh)

fb_levels <- list("Full Sample" = fb_reg_all_levels,
                  "Low and Lower-Middle Income" = fb_reg_lm_levels,
                  "Upper-Middle and High Income" = fb_reg_um_levels)

fb_asinh <- list("Full Sample" = fb_reg_all_asinh,
                 "Low and Lower-Middle Income" = fb_reg_lm_asinh,
                 "Upper-Middle and High Income" = fb_reg_um_asinh)

spillovers_map <- c('(Intercept)' = '$(\\text{Intercept})$',
                      'us_outflow' = '$\\text{US}$',
                      'announced' = '$\\text{announced}$',
                      'disbursed' = '$\\text{disbursed}$',
                      'us_outflow:announced' = '$\\text{announced} \\times \\text{US}$',
                      'disbursed:us_outflow' = '$\\text{disbursed} \\times \\text{US}$'
                      )

fb_map <- c('disbursed::1:asinh(fb1)' = '$\\text{disbursed} \\times asinh(\\text{FB})$')

gof_omitted <- "AIC|BIC|RMSE|Std.Errors|R2 Within"

note_spillovers <- "Standard errors clustered at the country level."

note_fb <- "Robust standard errors reported in parenthesis."

gm_spillovers <- tribble(~raw, ~clean, ~fmt,
                    "FE: time", "Time FE", "%.4f",
                    "FE: user_cc2", "Receiving Country FE", "%.4f",
                    "nobs", "$\\text{N}$", "%.0f",
                    "r.squared", "$R^{2}$", "%.2f",
                    "adj.r.squared", "$R^{2} Adj.$", "%.2f")

spillover_models <- list('Inverse Hyperbolic Sine' = spillovers_asinh,
                         'Levels (USD equivalent)' = spillovers_levels)

fb_models <- list('Inverse Hyperbolic Sine' = fb_asinh,
                  'Levels (USD equivalent)' = fb_levels)

spillovers_table <- modelsummary(spillover_models,
             stars = TRUE,
             shape = 'rbind',
             coef_map = spillovers_map,
             gof_omit = gof_omitted,
             gof_map = gm_spillovers,
             title = 'Dependent Variable: Cryptocurrency Outflows',
             escape = FALSE,
             output = 'latex') %>%
  add_footnote(note_spillovers, threeparttable = TRUE)

show(spillovers_table)

kableExtra::save_kable(spillovers_table, file = '../output/regression_tables/spillovers.tex')

fb_table <- modelsummary(fb_models,
                         stars = TRUE,
                         shape = 'rbind',
                         coef_map = fb_map,
                         gof_omit = gof_omitted,
                         gof_map = gm_spillovers,
                         title = 'Dependent Variable: US Cryptocurrency Outflows',
                         escape = FALSE,
                         output = 'latex') %>%
  add_footnote(note_fb, threeparttable = TRUE)

show(fb_table)

kableExtra::save_kable(fb_table, file = '../output/regression_tables/fb_table.tex')

#####Table with no panels
#spillovers

spillovers_models_no_panel <- list('Levels (USD)' = did_all_levels,
                                   'Inverse Hyperbolic Sine' = did_all_asinh,
                                   'Levels (USD)' = did_lm_levels,
                                   'Inverse Hyperbolic Sine' = did_lm_asinh,
                                   'Levels (USD)' = did_um_levels,
                                   'Inverse Hyperbolic Sine' = did_um_asinh)

spillovers_table_no_panel <- modelsummary(spillovers_models_no_panel,
                                          stars = TRUE,
                                          coef_map = spillovers_map,
                                          gof_moit = gof_omitted,
                                          gof_map = gm_spillovers,
                                          title = "Dependent Variable: Cryptocurrency Outflows",
                                          escape = FALSE,
                                          output = 'latex') %>%
  add_header_above(c(" " = 1, "Full Sample" = 2, "Low and Lower-Middle Income" = 2, "Upper-Middle and High Income" = 2)) %>%
  add_footnote(note_spillovers, threeparttable = TRUE)

show(spillovers_table_no_panel)

kableExtra::save_kable(spillovers_table_no_panel, file = '../output/regression_tables/spillovers_no_panel.tex')

#foreign-born

####EVENT STUDY GRAPHS

ggiplot(es_all_levels, col = 'deepskyblue3', geom_style = 'errorbar', ylab = 'Volume (USD)', main = 'Levels') + theme(axis.text.x = element_text(angle=90, vjust = .5))

ggsave('../output/event_study_plots/es_all_levels.png')

ggiplot(es_all_asinh, col = 'deepskyblue3', geom_style = 'errorbar', ylab = 'asinh(Volume)', main = 'Inverse Hyperbolic Sine') + theme(axis.text.x = element_text(angle=90, vjust = .5))

ggsave('../output/event_study_plots/es_all_asinh.png')

es_levels <- list("Full Sample" = es_all_levels,
                  "Low and Lower-Middle Income" = es_lm_levels,
                  "Upper-Middle and High Income" = es_um_levels)

ggiplot(es_levels, geom_style = "errorbar", ylab = "Volume (USD)", main = "Levels") + theme(axis.text.x = element_text(angle = 90, vjust = .5))

ggsave('../output/event_study_plots/es_levels.png')

es_asinh <- list("Full Sample" = es_all_asinh,
                 "Low and Lower-Middle Income" = es_lm_asinh,
                 "Upper-Middle and High Income" = es_um_asinh)

ggiplot(es_asinh, geom_style = 'errorbar', ylab = "asinh(Volume)", main = "Inverse Hyperbolic Sine") + theme(axis.text.x = element_text(angle = 90, vjust = .5))
 
ggsave('../output/event_study_plots/es_asinh.png')




