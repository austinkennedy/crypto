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

outflows_l <- flows %>%
    filter(income_group %in% c('L')) %>%
    group_by(user_cc, time) %>%
    summarize(volume_l = sum(volume))

outflows_m <- flows %>%
  filter(income_group %in% c('LM','UM')) %>%
  group_by(user_cc, time) %>%
  summarize(volume_m = sum(volume))

outflows_h <- flows %>%
  filter(income_group %in% c('H')) %>%
  group_by(user_cc, time) %>%
  summarize(volume_h = sum(volume))

outflows_joined <- list(outflows_all, outflows_l, outflows_m, outflows_h) %>%
    reduce(left_join, by = c("user_cc", "time")) %>%
    left_join(country_data[, c('alpha.2', 'oecd', 'income_group')], by = c('user_cc' = 'alpha.2')) %>%
    left_join(baseline_shares, by = 'user_cc') %>%
    mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0),
         us_outflow = ifelse(user_cc == "US", 1, 0)
  )
  
# 
# outflows_lm <- flows %>%
#   filter(income_group %in% c('L','LM')) %>%
#   group_by(user_cc, time) %>%
#   summarize(volume_lm = sum(volume))  
# 
# outflows_um <- flows %>%
#   filter(income_group %in% c('UM','H')) %>%
#   group_by(user_cc, time) %>%
#   summarize(volume_um = sum(volume))
# 
# outflows_joined <- list(outflows_all, outflows_lm, outflows_um) %>%
#   reduce(left_join, by = c("user_cc", "time")) %>%
#   left_join(country_data[, c('alpha.2', 'oecd', 'income_group')], by = c('user_cc' = 'alpha.2')) %>%
#   left_join(baseline_shares, by = 'user_cc') %>%
#   mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
#        disbursed = ifelse(time >= disbursement, 1, 0),
#        us_outflow = ifelse(user_cc == "US", 1, 0)
# )


#CLUSTERING LEVEL
cluster_level_spillovers <- c('user_cc')

# did_yvars <- c("volume_all", "volume_lm", "volume_um")

did_yvars <- c('volume_all', 'volume_l', 'volume_m', 'volume_h')

twfe_fml <- as.formula('.[did_yvars] ~ disbursed*us_outflow|user_cc + time')

baseline_controls <- outflows_joined %>%
  ungroup() %>%
  select(BJ:ZA) %>%
  colnames()

twfe_qmle <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(twfe_fml, cluster = cluster_level_spillovers, family = quasipoisson)

etable(twfe_qmle)

twfe_qmle_highincome <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(twfe_fml, cluster = cluster_level_spillovers, family = quasipoisson)

etable(twfe_qmle_highincome)
 

twfe_qmle_oecd <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feglm(twfe_fml, cluster = cluster_level_spillovers, family = quasipoisson)

etable(twfe_qmle_oecd)

did_qmle_controls <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(.[did_yvars] ~ disbursed*us_outflow + .[baseline_controls], cluster = cluster_level_spillovers, family = quasipoisson)

etable(did_qmle_controls)

did_qmle_controls_oecd <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feglm(.[did_yvars] ~ disbursed*us_outflow + .[baseline_controls], cluster = cluster_level_spillovers, family = quasipoisson)

etable(did_qmle_controls_oecd)

twfe_levels_oecd <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feols(twfe_fml, cluster = cluster_level_spillovers)

etable(twfe_levels_oecd)

did_levels_controls <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feols(.[did_yvars] ~ disbursed*us_outflow + .[baseline_controls], cluster = cluster_level_spillovers)
  
etable(did_levels_controls)
####EVENTSTUDY

es_levels <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feols(.[did_yvars] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers)

iplot(es_levels)

es_levels_oecd<- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feols(.[did_yvars] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers)

iplot(es_levels_oecd)

es_qmle <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(.[did_yvars] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle)

es_qmle_oecd <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feglm(.[did_yvars] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle_oecd)

es_qmle_highincome <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(.[did_yvars] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle_highincome)

##########US-only outflows

#add announced and disbursed variables

outflows_us <- outflows_us %>% mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
       disbursed = ifelse(time >= disbursement, 1, 0))

#Create lm and um datasets

# us_outflows_lm <- us_outflows_country %>% filter(income_group %in% c('L', 'LM'))
# 
# us_outflows_um <- us_outflows_country %>% filter(income_group %in% c('UM', 'H'))

fb_model <- function(df, yvar, income_g){
  reg <- df %>%
    filter(time >= window_start & time <= window_end,
           income_group %in% income_g) %>%
    feglm(.[yvar] ~ i(disbursed, log(fb1), ref = 0)|time + user_cc2,
          cluster = c('user_cc2'), family = quasipoisson)
  
}

fb_reg_all_levels <- fb_model(outflows_us, yvar = 'volume', income_g = c('L', 'LM', 'UM', 'H'))

summary(fb_reg_all_levels)

fb_reg_l_levels <- fb_model(outflows_us, yvar = 'volume', income_g = c('L'))

summary(fb_reg_l_levels)

fb_reg_m_levels <- fb_model(outflows_us, yvar = 'volume', income_g = c('LM', 'UM'))

summary(fb_reg_m_levels)

fb_reg_h_levels <- fb_model(outflows_us, yvar = 'volume', income_g = c('H'))

summary(fb_reg_h_levels)

############TABLES

#WITH PANELS

model_names <- c("All Destinations", "Low Income", "Middle Income", "High Income")

names(twfe_qmle) <- model_names
names(twfe_qmle_highincome) <- model_names
names(twfe_qmle_oecd) <- model_names

twfe_models <- list('Full Control Group' = twfe_qmle,
                   'Control Group: High Income Countries Only' = twfe_qmle_highincome,
                   'Control Group: OECD Countries Only' = twfe_qmle_oecd)

cmap_twfe <- c('(Intercept)' = '$(\\text{Intercept})$',
                            'us_outflow' = '$\\text{US}$',
                            'announced' = '$\\text{announced}$',
                            'disbursed' = '$\\text{disbursed}$',
                            'us_outflow:announced' = '$\\text{announced} \\times \\text{US}$',
                            'disbursed:us_outflow' = '$\\text{disbursed} \\times \\text{US}$'
)

gof_omitted <- "AIC|BIC|RMSE|Std.Errors|R2 Within"

note_twfe <- "Standard errors clustered at the country level."

gm_twfe <- tribble(~raw, ~clean, ~fmt,
                  "nobs", "$\\text{Observations}$", "%.0f",
                  "r.squared", "$R^{2}$", "%.2f",
                  "adj.r.squared", "$R^{2} Adj.$", "%.2f",
                  "FE: user_cc", "Country FE", "%.4f",
                  "FE: time", "Week FE", "%.4f")

twfe_table <- modelsummary(twfe_models,
                          stars = TRUE,
                          shape = 'rbind',
                          coef_map = cmap_twfe,
                          gof_map = gm_twfe,
                          gof_omit = gof_omitted,
                          title = "Poisson QMLE–Dependent Variable: Cryptocurrency Outflows",
                          escape = FALSE,
                          output = 'latex') %>%
  add_footnote(note_twfe, threeparttable = TRUE)

show(twfe_table)

kableExtra::save_kable(twfe_table, file = "../output/regression_tables/twfe_qmle.tex")

fb_models <- list('Inverse Hyperbolic Sine' = fb_asinh,
                  'Levels (USD equivalent)' = fb_levels)

fb_table <- modelsummary(fb_models,
                         stars = TRUE,
                         shape = 'rbind',
                         coef_map = fb_map,
                         # gof_omit = gof_omitted,
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

es_qmle_plot_split <- es_plot(list("Middle-Income Destinations" = es_qmle[3],"High-Income Destinations" = es_qmle[4]), title = "Full Sample: By Destination Income Group")

show(es_qmle_plot_split)

es_qmle_plot_all_highincome <- es_plot(es_qmle_highincome[[1]], title = "High Income Sample: All Destinations")

show(es_qmle_plot_all_highincome)

es_qmle_plot_split_highincome <- es_plot(list("Middle-Income Destinations" = es_qmle_highincome[[3]], "High-Income Destinations" = es_qmle_highincome[[4]]), title = "High Income Sample: By Destination Income Group")

show(es_qmle_plot_split_highincome)

es_qmle_plot_all_oecd <- es_plot(es_qmle_oecd[[1]], title = "OECD Sample: All Destinations")

show(es_qmle_plot_all_oecd)

es_qmle_plot_split_oecd <- es_plot(list("Middle-Income Destinations" = es_qmle_oecd[[3]], "High-Income Destinations" = es_qmle_oecd[[4]]), title = "OECD Sample: By Destination Income Group")

show(es_qmle_plot_split_oecd)


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








