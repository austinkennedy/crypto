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

####Load Data
flows <- vroom('../temporary/bilateral_flows_balanced.csv')
outflows <- vroom('../temporary/outflows_balanced.csv')
outflows_us <- vroom('../temporary/us_outflows_balanced.csv')
country_data <- read.csv('../temporary/country_data.csv')
baseline_shares <- read.csv('../temporary/baseline_shares.csv')

####OPTIONS
#add phases
announcement <- as.Date('2020-03-27')

disbursement <- as.Date('2020-04-09')

window_start <- as.Date('2020-01-01')
window_end <- as.Date('2020-06-07')

treated_countries <- c('JP', 'KR', 'SG')

#filter other countries with stimulus
outflows_us <- outflows_us %>%
  filter(!user_cc2 %in% treated_countries)

flows <- flows %>%
  filter(!user_cc %in% treated_countries)


#######################Outflows only###################

#get total outflows by source country, to varying country groups
outflows_all <- flows %>%
  group_by(user_cc, time) %>%
  summarize(volume_all = sum(volume))

outflows_lm <- flows %>%
  filter(income_group %in% c('L','LM')) %>%
  group_by(user_cc, time) %>%
  summarize(volume_lm = sum(volume))  

outflows_um <- flows %>%
  filter(income_group %in% c('UM','H')) %>%
  group_by(user_cc, time) %>%
  summarize(volume_um = sum(volume))

outflows_joined <- list(outflows_all, outflows_lm, outflows_um) %>%
  reduce(left_join, by = c("user_cc", "time")) %>%
  left_join(country_data[, c('alpha.2', 'oecd', 'income_group')], by = c('user_cc' = 'alpha.2')) %>%
  left_join(baseline_shares, by = 'user_cc')

#add treatment dates
outflows_joined <- outflows_joined %>%
  mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0),
         us_outflow = ifelse(user_cc == "US", 1, 0)
)

#CLUSTERING LEVEL
cluster_level_spillovers <- c('user_cc')

did_yvars <- c("volume_all", "volume_lm", "volume_um")

baseline_controls <- outflows_joined %>%
  ungroup() %>%
  select(BJ:ZA) %>%
  colnames()

did_levels <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feols(.[did_yvars] ~ disbursed*us_outflow + announced*us_outflow, cluster = cluster_level_spillovers)

summary(did_levels)

did_qmle <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(.[did_yvars] ~ disbursed*us_outflow + announced*us_outflow, cluster = cluster_level_spillovers, family = quasipoisson)

etable(did_qmle)
 

did_qmle_oecd <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feglm(.[did_yvars] ~ disbursed*us_outflow + announced*us_outflow, cluster = cluster_level_spillovers, family = quasipoisson)

etable(did_qmle_oecd)

did_qmle_highincome <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(.[did_yvars] ~ disbursed*us_outflow + announced*us_outflow, cluster = cluster_level_spillovers, family = quasipoisson)

etable(did_qmle_highincome)

did_qmle_controls <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(.[did_yvars] ~ disbursed*us_outflow + announced*us_outflow + .[baseline_controls], cluster = cluster_level_spillovers, family = quasipoisson)

etable(did_qmle_controls)

did_qmle_controls_oecd <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feglm(.[did_yvars] ~ disbursed*us_outflow + announced*us_outflow + .[baseline_controls], cluster = cluster_level_spillovers, family = quasipoisson)

etable(did_qmle_controls_oecd)

did_levels_oecd <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feols(.[did_yvars] ~ disbursed*us_outflow + announced*us_outflow, cluster = cluster_level_spillovers)

etable(did_levels_oecd)

did_levels_controls <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feols(.[did_yvars] ~ disbursed*us_outflow + announced*us_outflow + .[baseline_controls], cluster = cluster_level_spillovers)
  
etable(did_levels_controls)
####EVENTSTUDY

es_yvars <- c("volume_all", "volume_lm", "volume_um")

es_levels <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feols(.[es_yvars] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers)

iplot(es_levels)

es_levels_oecd<- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feols(.[es_yvars] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers)

iplot(es_levels_oecd)

es_qmle <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(.[es_yvars] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle)

es_qmle_oecd <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feglm(.[es_yvars] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle_oecd)

es_qmle_highincome <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(.[es_yvars] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle_highincome)

es_qmle_controls <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(.[es_yvars] ~ i(time, us_outflow, ref = '2020-04-05') + .[baseline_controls]|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle_controls)

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
    feols(.[yvar] ~ i(disbursed, log(fb1), ref = 0)|time + user_cc2,
                      cluster = c('user_cc2'))
  
}

fb_model <- function(df, yvar){
  reg <- df %>%
    filter(time >= window_start & time <= window_end) %>%
    feglm(.[yvar] ~ i(disbursed, log(fb1), ref = 0)|time + user_cc2,
          cluster = c('user_cc2'), family = quasipoisson)
  
}

fb_reg_all_levels <- fb_model(us_outflows_country, yvar = 'volume')

summary(fb_reg_all_levels)

fb_reg_lm_levels <- fb_model(us_outflows_lm, yvar = 'volume')

summary(fb_reg_lm_levels)

fb_reg_um_levels <- fb_model(us_outflows_um, yvar = 'volume')

summary(fb_reg_um_levels)

fb_reg_all_log <- fb_model(us_outflows_country, yvar = 'log(volume)')

summary(fb_reg_all_log)

fb_reg_lm_log <- fb_model(us_outflows_lm, yvar = 'log(volume)')

summary(fb_reg_lm_log)

fb_reg_um_log <- fb_model(us_outflows_um, yvar = 'log(volume)')

summary(fb_reg_um_log)

############TABLES

#WITH PANELS

model_names <- c("All Destinations", "Low and Lower-Middle Income", "Upper-Middle and High Income")

names(did_qmle) <- model_names
names(did_qmle_highincome) <- model_names
names(did_qmle_oecd) <- model_names

did_models <- list('Full Control Group' = did_qmle,
                   'High Income Countries Only' = did_qmle_highincome,
                   'OECD Countries Only' = did_qmle_oecd)

cmap_did <- c('(Intercept)' = '$(\\text{Intercept})$',
                            'us_outflow' = '$\\text{US}$',
                            'announced' = '$\\text{announced}$',
                            'disbursed' = '$\\text{disbursed}$',
                            'us_outflow:announced' = '$\\text{announced} \\times \\text{US}$',
                            'disbursed:us_outflow' = '$\\text{disbursed} \\times \\text{US}$'
)

gof_omitted <- "AIC|BIC|RMSE|Std.Errors|R2 Within"

note_did <- "Standard Errors clustered at the country level."

gm_did <- tribble(~raw, ~clean, ~fmt,
                  "nobs", "$\\text{N}$", "%.0f",
                  "r.squared", "$R^{2}$", "%.2f",
                  "adj.r.squared", "$R^{2} Adj.$", "%.2f")

did_table <- modelsummary(did_models,
                          stars = TRUE,
                          shape = 'rbind',
                          coef_map = cmap_did,
                          # gof_map = gm_did,
                          # gof_omit = gof_omitted,
                          title = "Poisson QMLE–Dependent Variable: Cryptocurrency Outflows",
                          escape = FALSE,
                          output = 'latex') %>%
  add_footnote(note_did, threeparttable = TRUE)

show(did_table)

kableExtra::save_kable(did_table, file = "../output/regression_tables/did_qmle.tex")

fb_models <- list('Inverse Hyperbolic Sine' = fb_asinh,
                  'Levels (USD equivalent)' = fb_levels)

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

#change names of models
names(es_levels) <- model_names

names(es_qmle) <- model_names
names(es_qmle_highincome) <- model_names
names(es_qmle_oecd) <- model_names

es_plot <- function(es_model, title){
  ggiplot(es_model, geom_style = "errorbar", ylab = 'Estimate', main = title, multi_style = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .5))
}

es_qmle_plot_all <- es_plot(es_qmle[[1]], title = "Full Sample: All Destinations")

show(es_qmle_plot_all)

es_qmle_plot_split <- es_plot(es_qmle[2:3], title = "Full Sample: By Destination Income Group")

show(es_qmle_plot_split)

es_qmle_plot_all_highincome <- es_plot(es_qmle_highincome[[1]], title = "High Income Sample: All Destinations")

show(es_qmle_plot_all_highincome)

es_qmle_plot_split_highincome <- es_plot(es_qmle_highincome[2:3], title = "High Income Sample: By Destination Income Group")

show(es_qmle_plot_split_highincome)

es_qmle_plot_all_oecd <- es_plot(es_qmle_oecd[[1]], title = "OECD Sample: All Destinations")

show(es_qmle_plot_all_oecd)

es_qmle_plot_split_oecd <- es_plot(es_qmle_oecd[2:3], title = "OECD Sample: By Destination Income Group")

show(es_qmle_plot_split_oecd)



es_levels_plot <- ggiplot(es_levels, geom_style = "errorbar", ylab = "Estimate", main = "Levels (USD Equivalent)") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5))

show(es_levels_plot)

ggsave('../output/event_study_plots/es_levels.png', plot = es_levels_plot)

es_qmle_plot <- ggiplot(es_qmle, geom_style = "errorbar", ylab = "Estimate", main = "Poisson QMLE") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) 

show(es_qmle_plot)

ggsave('../output/event_study_plots/es_qmle.png', plot = es_qmle_plot)



ggiplot(es_asinh, geom_style = 'errorbar', ylab = "asinh(Volume)", main = "Inverse Hyperbolic Sine") + theme(axis.text.x = element_text(angle = 90, vjust = .5))
 
ggsave('../output/event_study_plots/es_asinh.png')



es_levels_all <- ggiplot(es_levels[[1]], col = 'deepskyblue3', geom_style = 'errorbar', ylab = 'Volume (USD)', main = 'Levels') + theme(axis.text.x = element_text(angle=90, vjust = .5))

ggsave('../output/event_study_plots/es_levels.png', plot = es_levels_all)



####Show flows with US vs. Non-US

global_flows <- outflows_all %>% filter(user_cc != 'US') %>% group_by(time) %>% summarize(volume_all = sum(volume_all))
global_flows$user_cc = 'Non-US'
us_flows <- outflows_all %>% filter(user_cc == 'US') %>% select(user_cc, volume_all, time)

outflows_us_non_us <- rbind(global_flows, us_flows)

outflows_us_non_us %>% filter(time >= window_start & time <= '2020-09-01') %>% ggplot(aes(x = time, y = volume_all, color = user_cc)) + geom_line(size = 1) + theme_bw() + ggtitle("Cryptocurrency Outflows") + theme(plot.title = element_text(size = 15, hjust = 0.5)) + labs(color = 'Legend')

show(outflows_us_non_us)

ggsave('../output/figures_paxful/global_flows.png')

######PLAYGROUND##########

total <- outflows %>%
  group_by(user_cc) %>%
  summarize(total = sum(outflow))








