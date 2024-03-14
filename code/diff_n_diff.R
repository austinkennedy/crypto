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

##uncomment to run daily results
# flows <- vroom('../temporary/bilateral_flows_balanced_daily.csv')

####Load Data
flows <- vroom('../temporary/bilateral_flows_balanced.csv')


outflows <- vroom('../temporary/outflows_balanced.csv')
outflows_us <- vroom('../temporary/us_outflows_balanced.csv')
country_data <- read.csv('../temporary/country_data.csv')
baseline_shares <- read.csv('../temporary/baseline_shares.csv')

####OPTIONS
#add phases
announcement <- as.Date('2020-03-27')

disbursement <- as.Date('2020-04-05')

window_start <- as.Date('2020-01-01')
window_end <- as.Date('2020-06-07')

treated_countries <- c('JP', 'KR', 'SG')

#filter other countries with stimulus
outflows_us <- outflows_us %>%
  filter(!user_cc2 %in% treated_countries)

flows <- flows %>%
  filter(!user_cc %in% treated_countries,
         user_cc != user_cc2) %>%
  left_join(country_data, by = c('user_cc2' = 'alpha.2'))

#filter out NG
# flows <- flows %>%
#   filter(user_cc != 'NG',
#          user_cc2 != 'NG')


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
# 
# outflows_lm <- flows %>%
#   filter(income_group %in% c('LM')) %>%
#   group_by(user_cc, time) %>%
#   summarize(volume_lm = sum(volume))
# 
# outflows_um <- flows %>%
#   filter(income_group %in% c('UM')) %>%
#   group_by(user_cc, time) %>%
#   summarize(volume_um = sum(volume))

outflows_h <- flows %>%
  filter(income_group %in% c('H')) %>%
  group_by(user_cc, time) %>%
  summarize(volume_h = sum(volume))

outflows_joined <- list(outflows_all, outflows_l, outflows_m, outflows_h) %>%
    reduce(left_join, by = c("user_cc", "time")) %>%
    left_join(country_data, by = c('user_cc' = 'alpha.2')) %>%
    left_join(baseline_shares, by = 'user_cc') %>%
    mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0),
         us_outflow = ifelse(user_cc == "US", 1, 0)
  )

# outflows_joined <- list(outflows_all, outflows_l, outflows_lm, outflows_um, outflows_h) %>%
#   reduce(left_join, by = c("user_cc", "time")) %>%
#   left_join(country_data, by = c('user_cc' = 'alpha.2')) %>%
#   left_join(baseline_shares, by = 'user_cc') %>%
#   mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
#          disbursed = ifelse(time >= disbursement, 1, 0),
#          us_outflow = ifelse(user_cc == "US", 1, 0)
#   )


#CLUSTERING LEVEL
cluster_level_spillovers <- c('user_cc')

did_yvars <- names(outflows_joined)[grepl("volume_", names(outflows_joined))]

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

did_levels <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feols(.[did_yvars] ~ disbursed*us_outflow, cluster = cluster_level_spillovers)

etable(did_levels)

did_levels_controls <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feols(.[did_yvars] ~ disbursed*us_outflow + .[baseline_controls], cluster = cluster_level_spillovers)
  
etable(did_levels_controls)

####EVENTSTUDY

reference_period <- '2020-03-29'

es_levels <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feols(.[did_yvars] ~ i(time, us_outflow, ref = '2020-03-29')|time + user_cc, cluster = cluster_level_spillovers)

iplot(es_levels)

es_levels_oecd<- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feols(.[did_yvars] ~ i(time, us_outflow, ref = '2020-03-29')|time + user_cc, cluster = cluster_level_spillovers)

iplot(es_levels_oecd)

es_qmle <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(.[did_yvars] ~ i(time, us_outflow, ref = '2020-03-29')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle)

es_qmle_oecd <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         oecd == 1) %>%
  feglm(.[did_yvars] ~ i(time, us_outflow, ref = '2020-03-29')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle_oecd)

es_qmle_highincome <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(.[did_yvars] ~ i(time, us_outflow, ref = '2020-03-29')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_qmle_highincome)

##########FB regs


outflows_q1 <- flows %>%
  filter(quantile == 1) %>%
  group_by(user_cc, time) %>%
  summarize(volume_q1 = sum(volume))

outflows_q2 <- flows %>%
  filter(quantile == 2) %>%
  group_by(user_cc, time) %>%
  summarize(volume_q2 = sum(volume))

outflows_q3 <- flows %>%
  filter(quantile == 3) %>%
  group_by(user_cc, time) %>%
  summarize(volume_q3 = sum(volume))

outflows_q4 <- flows %>%
  filter(quantile == 4) %>%
  group_by(user_cc, time) %>%
  summarize(volume_q4 = sum(volume))

outflows_q <- list(outflows_q1, outflows_q2, outflows_q3, outflows_q4) %>%
  reduce(left_join, by = c("user_cc", "time")) %>%
  left_join(country_data, by = c('user_cc' = 'alpha.2')) %>%
  mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0),
         us_outflow = ifelse(user_cc == "US", 1, 0)
  )

did_yvars_q <- c('volume_q1', 'volume_q2', 'volume_q3', 'volume_q4')

twfe_qmle_q <- outflows_q %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(.[did_yvars_q]~disbursed*us_outflow|user_cc + time, cluster = cluster_level_spillovers, family = quasipoisson())

etable(twfe_qmle_q)

es_q <- outflows_q %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(.[did_yvars_q] ~ i(time, us_outflow, ref = '2020-03-29')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_q)



############TABLES

#WITH PANELS

model_names <- c("All Destinations", "Low-Income", "Middle-Income", "High-Income")

names(twfe_qmle) <- model_names
names(twfe_qmle_highincome) <- model_names
names(twfe_qmle_oecd) <- model_names

#this is the standard star map, not sure why modelsummary uses a different one
star_map = c('*' = .1, '**' = 0.05, '***' = 0.01)

twfe_models <- list('Full Control Group' = twfe_qmle,
                   'Control Group: High-Income Countries Only' = twfe_qmle_highincome,
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
                          stars = star_map,
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

######FOREIGN-BORN TABLES

names(twfe_qmle_q) <- c('Lowest Quartile', 'Second Quartile', 'Third Quartile', 'Highest Quartile')

cmap_fb <- c('disbursed:us_outflow' = '$\\text{disbursed} \\times \\text{US}$')

gof_fb <- tribble(~raw, ~clean, ~fmt,
                     "nobs", "$\\text{Observations}$", "%.0f",
                     "r.squared", "$R^{2}$", "%.2f",
                     "adj.r.squared", "$R^{2} Adj.$", "%.2f",
                     "FE: user_cc", "Country FE", "%.4f",
                     "FE: time", "Week FE", "%.4f")

note_fb <- 'Standard errors clustered at the country level. Columns refer to the outflows destined for countries depending on their level of foreign-born residents in the US, adjusted for their population, and split into quartiles.'

fb_table <- modelsummary(twfe_qmle_q,
                         stars = star_map,
                         coef_map = cmap_fb,
                         gof_omit = gof_omitted,
                         gof_map = gof_fb,
                         title = 'Poisson QMLE–Dependent Variable: Cryptocurrency Outflows',
                         escape = FALSE,
                         output = 'latex') %>%
  add_footnote(note_fb, threeparttable = TRUE)

show(fb_table)

kableExtra::save_kable(fb_table, file = '../output/regression_tables/fb_table.tex')

####EVENT STUDY GRAPHS


#change names of models
names(es_levels) <- model_names
names(es_qmle) <- model_names
names(es_qmle_highincome) <- model_names
names(es_qmle_oecd) <- model_names

es_plot <- function(es_model, title, filename){
  
  plot <- ggiplot(es_model,
                  geom_style = "errorbar",
                  ylab = 'Estimate',
                  main = title,
                  multi_style = "dodge",
                  ref.line = '2020-04-05') + 
    theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 13),
          axis.text.y = element_text(size = 13),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 13),
          plot.title = element_text(size = 20))
  
  #save plot
  filepath = paste('../output/event_study_plots/', filename, '.png', sep = '')
  ggsave(filepath, plot, width = 11, height = 8, dpi = 300)
  
  return(plot)
}

es_qmle_plot_all <- es_plot(es_qmle[[1]], title = "Full Control Group: All Destinations",
                            filename = 'es_qmle_fullcontrol_all')
show(es_qmle_plot_all)
es_qmle_plot_split <- es_plot(list("Middle-Income Destinations" = es_qmle[[3]],
                                   "High-Income Destinations" = es_qmle[[4]]),
                              title = "Full Control Group: By Destination Income Group",
                              filename = "es_qmle_fullcontrol_split")
show(es_qmle_plot_split)
es_qmle_plot_all_highincome <- es_plot(es_qmle_highincome[[1]],
                                       title = "High Income Control Group: All Destinations",
                                       filename = 'es_qmle_highincome_all')
show(es_qmle_plot_all_highincome)
es_qmle_plot_split_highincome <- es_plot(list("Middle-Income Destinations" = es_qmle_highincome[[3]],
                                              "High-Income Destinations" = es_qmle_highincome[[4]]),
                                         title = "High Income Control Group: By Destination Income Group",
                                         filename = 'es_qmle_highincome_split')
show(es_qmle_plot_split_highincome)
es_qmle_plot_all_oecd <- es_plot(es_qmle_oecd[[1]],
                                 title = "OECD Control Group: All Destinations",
                                 filename = 'es_qmle_oecd_all')
show(es_qmle_plot_all_oecd)
es_qmle_plot_split_oecd <- es_plot(list("Middle-Income Destinations" = es_qmle_oecd[[3]],
                                        "High-Income Destinations" = es_qmle_oecd[[4]]),
                                   title = "OECD Control Group: By Destination Income Group",
                                   filename = 'es_qmle_oecd_split')

show(es_qmle_plot_split_oecd)

es_qmle_fullcontrol_lowincome <- es_plot(es_qmle[[2]],
                                      title = "Full Control Group: Low-Income Destinations",
                                      filename = 'es_qmle_fullcontrol_lowincome')

show(es_qmle_fullcontrol_lowincome)

es_qmle_highincome_lowincome <- es_plot(es_qmle_highincome[[2]],
                                         title = 'High-Income Control Group: Low-Income Destinations',
                                         filename = 'es_qmle_highincome_lowincome')

show(es_qmle_highincome_lowincome)

es_qmle_oecd_lowincome <- es_plot(es_qmle_oecd[[2]],
                                  title = "OECD Control Group: Low-Income Destinations",
                                  filename = 'es_qmle_oecd_lowincome')

############ROBUSTNESS


###extend time window

##using only high-income sample

window_end <- as.Date('2020-07-07')

twfe_1_month <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(twfe_fml, cluster = cluster_level_spillovers, family = quasipoisson)

etable(twfe_1_month)

window_end <- as.Date('2020-08-07')

twfe_2_month <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(twfe_fml, cluster = cluster_level_spillovers, family = quasipoisson)

etable(twfe_2_month)

window_end <- as.Date('2020-09-07')

twfe_3_month <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(twfe_fml, cluster = cluster_level_spillovers, family = quasipoisson)

etable(twfe_3_month)

window_end <- as.Date('2021-01-01')

twfe_yearend <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(twfe_fml, cluster = cluster_level_spillovers, family = quasipoisson)

etable(twfe_yearend)

es_extended <- outflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(.[did_yvars] ~ i(time, us_outflow, ref = '2020-03-29')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

es_extended_all <- es_plot(es_extended[[1]],
                           title = 'Extended Event-Study: All Destinations',
                           filename = 'es_extended_all')

show(es_extended_all)

es_extended_split <- es_plot(list("Middle-Income Destinations" = es_extended[[3]],
                                  "High-Income Destinations" = es_extended[[4]]),
                             title = "Extended Event-Study: By Destination Income Group",
                             file = 'es_extended_split')

show(es_extended_split)

###table

names(twfe_1_month) <- model_names
names(twfe_2_month) <- model_names
names(twfe_3_month) <- model_names
names(twfe_yearend) <- model_names

extended_models <- list('1 Month' = twfe_1_month,
                        '2 Months' = twfe_2_month,
                        '3 Months' = twfe_3_month,
                        'Year End' = twfe_yearend)

extended_table <- modelsummary(extended_models,
                           stars = star_map,
                           shape = 'rbind',
                           coef_map = cmap_twfe,
                           gof_map = gm_twfe,
                           gof_omit = gof_omitted,
                           title = "Poisson QMLE–Dependent Variable: Cryptocurrency Outflows",
                           escape = FALSE,
                           output = 'latex') %>%
  add_footnote(note_twfe, threeparttable = TRUE)

show(extended_table)

kableExtra::save_kable(extended_table, file = "../output/regression_tables/extended_models.tex")

###use inflows as placebo

window_end <- as.Date('2020-06-07')

inflows_all <- flows %>%
  group_by(user_cc2, time) %>%
  summarize(volume_all = sum(volume))

inflows_l <- flows %>%
  select(user_cc, user_cc2, time, volume) %>%
  left_join(country_data[, c('alpha.2', 'oecd', 'income_group')], by = c('user_cc' = 'alpha.2')) %>%
  filter(income_group %in% c('L')) %>%
  group_by(user_cc2, time) %>%
  summarize(volume_l = sum(volume))

inflows_m <- flows %>%
  select(user_cc, user_cc2, time, volume) %>%
  left_join(country_data[, c('alpha.2', 'oecd', 'income_group')], by = c('user_cc' = 'alpha.2')) %>%
  filter(income_group %in% c('LM', 'UM')) %>%
  group_by(user_cc2, time) %>%
  summarize(volume_m = sum(volume))

# inflows_lm <- flows %>%
#   select(user_cc, user_cc2, time, volume) %>%
#   left_join(country_data[, c('alpha.2', 'oecd', 'income_group')], by = c('user_cc' = 'alpha.2')) %>%
#   filter(income_group %in% c('LM')) %>%
#   group_by(user_cc2, time) %>%
#   summarize(volume_lm = sum(volume))
# 
# inflows_um <- flows %>%
#   select(user_cc, user_cc2, time, volume) %>%
#   left_join(country_data[, c('alpha.2', 'oecd', 'income_group')], by = c('user_cc' = 'alpha.2')) %>%
#   filter(income_group %in% c('UM')) %>%
#   group_by(user_cc2, time) %>%
#   summarize(volume_um = sum(volume))

inflows_h <- flows %>%
  select(user_cc, user_cc2, time, volume) %>%
  left_join(country_data[, c('alpha.2', 'oecd', 'income_group')], by = c('user_cc' = 'alpha.2')) %>%
  filter(income_group %in% c('H')) %>%
  group_by(user_cc2, time) %>%
  summarize(volume_h = sum(volume))

inflows_joined <- list(inflows_all, inflows_l, inflows_m, inflows_h) %>%
  reduce(left_join, by = c('user_cc2', 'time')) %>%
  left_join(country_data[, c('alpha.2', 'oecd', 'income_group')], by = c('user_cc2' = 'alpha.2')) %>%
  mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0),
         us_inflow = ifelse(user_cc2 == "US", 1, 0)
  )

# inflows_joined <- list(inflows_all, inflows_l, inflows_lm, inflows_um, inflows_h) %>%
#   reduce(left_join, by = c('user_cc2', 'time')) %>%
#   left_join(country_data[, c('alpha.2', 'oecd', 'income_group')], by = c('user_cc2' = 'alpha.2')) %>%
#   mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
#          disbursed = ifelse(time >= disbursement, 1, 0),
#          us_inflow = ifelse(user_cc2 == "US", 1, 0)
#   )

twfe_inflows_highincome <- inflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(.[did_yvars] ~ disbursed*us_inflow|user_cc2 + time, cluster = c('user_cc2'), family = quasipoisson())

etable(twfe_inflows_highincome)

es_inflows_highincome <- inflows_joined %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(volume_m ~ i(time, us_inflow, ref = '2020-04-05')|time + user_cc2, cluster = c('user_cc2'), family = quasipoisson)

iplot(es_inflows_highincome) 

###table for inflows

cmap_inflows <- c('disbursed:us_inflow' = '$\\text{disbursed} \\times \\text{US}$')

names(twfe_inflows_highincome) <- c("All Origins", "Low-Income", "Middle-Income", "High-Income")

gm_inflows <- tribble(~raw, ~clean, ~fmt,
                   "nobs", "$\\text{Observations}$", "%.0f",
                   "r.squared", "$R^{2}$", "%.2f",
                   "adj.r.squared", "$R^{2} Adj.$", "%.2f",
                   "FE: user_cc2", "Country FE", "%.4f",
                   "FE: time", "Week FE", "%.4f")

inflows_table <- modelsummary(twfe_inflows_highincome,
                              stars = star_map,
                              coef_map = cmap_inflows,
                              gof_map = gm_inflows,
                              gof_omit = gof_omitted,
                              title = "Poisson QMLE–Dependent Variable: Cryptocurrency Inflows",
                              escape = FALSE,
                              output = 'latex') %>%
  add_footnote(note_twfe, threeparttable = TRUE)

show(inflows_table)

kableExtra::save_kable(inflows_table, file = "../output/regression_tables/inflows_table.tex")



####Show flows with US vs. Non-US

global_flows <- outflows_all %>% filter(user_cc != 'US') %>% group_by(time) %>% summarize(volume_all = sum(volume_all))
global_flows$user_cc = 'Non-US'
us_flows <- outflows_all %>% filter(user_cc == 'US') %>% select(user_cc, volume_all, time)

outflows_us_non_us <- rbind(global_flows, us_flows)

outflows_us_non_us %>% filter(time >= window_start & time <= '2020-09-01') %>% ggplot(aes(x = time, y = volume_all, color = user_cc)) + geom_line(size = 1) + theme_bw() + ggtitle("Cryptocurrency Outflows") + theme(plot.title = element_text(size = 15, hjust = 0.5)) + labs(color = 'Legend')

show(outflows_us_non_us)

ggsave('../output/figures_paxful/global_flows.png')

######PLAYGROUND##########

fb_reg <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(c(volume_fb_above, volume_fb_below) ~ disbursed*us_outflow|time+user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

etable(fb_reg)

fb_destinations <- outflows_joined %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(volume_all ~ disbursed*us_outflow|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

etable(fb_destinations)


fb_percap <- outflows_us %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(volume ~ fb1^2*disbursed + fb1*disbursed, cluster = c('user_cc2'), family = quasipoisson)

etable(fb_percap)

total <- outflows %>%
  group_by(user_cc) %>%
  summarize(total = sum(outflow))


fb_threshold <- outflows_us %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(volume ~ disbursed*fb_pc_above|user_cc2 + time, cluster = c('user_cc2'), family = quasipoisson)

etable(fb_threshold)

etable(es_fb)

fb_threshold_es <- outflows_us %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(volume ~ i(time,fb_above, ref = '2020-04-05')|user_cc2 + time, cluster = c('user_cc2'), family = quasipoisson)

iplot(fb_threshold_es)

fees_threshold <- outflows_us %>%
  filter(time >= window_start & time <= window_end) %>%
  feglm(volume ~ disbursed*fees_median|user_cc2 + time, cluster = c('user_cc2'), family = quasipoisson)

etable(fees_threshold)


#########FB DID

outflows_q1 <- flows %>%
  filter(quantile == 1) %>%
  group_by(user_cc, time) %>%
  summarize(volume_q1 = sum(volume))

outflows_q2 <- flows %>%
  filter(quantile == 2) %>%
  group_by(user_cc, time) %>%
  summarize(volume_q2 = sum(volume))

outflows_q3 <- flows %>%
  filter(quantile == 3) %>%
  group_by(user_cc, time) %>%
  summarize(volume_q3 = sum(volume))

outflows_q4 <- flows %>%
  filter(quantile == 4) %>%
  group_by(user_cc, time) %>%
  summarize(volume_q4 = sum(volume))

outflows_q <- list(outflows_q1, outflows_q2, outflows_q3, outflows_q4) %>%
  reduce(left_join, by = c("user_cc", "time")) %>%
  left_join(country_data, by = c('user_cc' = 'alpha.2')) %>%
  mutate(announced = ifelse((time > announcement & time < disbursement), 1, 0),
         disbursed = ifelse(time >= disbursement, 1, 0),
         us_outflow = ifelse(user_cc == "US", 1, 0)
  )

did_yvars_q <- c('volume_q1', 'volume_q2', 'volume_q3', 'volume_q4')

twfe_qmle_q <- outflows_q %>%
  filter(time >= window_start & time <= window_end,
        income_group == 'H') %>%
  feglm(.[did_yvars_q]~disbursed*us_outflow|user_cc + time, cluster = cluster_level_spillovers, family = quasipoisson())

etable(twfe_qmle_q)

es_q <- outflows_q %>%
  filter(time >= window_start & time <= window_end,
         income_group == 'H') %>%
  feglm(.[did_yvars_q] ~ i(time, us_outflow, ref = '2020-04-05')|time + user_cc, cluster = cluster_level_spillovers, family = quasipoisson)

iplot(es_q)

ggiplot(es_q)

#####net flows

net_flows <- inner_join(outflows_joined, inflows_joined,
                        by = c('user_cc' = 'user_cc2', 'time'),
                        suffix = c('_outflows', '_inflows')) %>%
  mutate(volume_net_all = volume_all_outflows - volume_all_inflows,
         volume_net_l = volume_l_outflows - volume_l_inflows,
         volume_net_m = volume_m_outflows - volume_m_inflows,
         volume_net_h = volume_h_outflows - volume_h_inflows)

net_yvars <- names(net_flows)[grepl("volume_net_", names(net_flows))]

qmle_net <- net_flows %>%
  filter(time >= window_start & time <= window_end,
         income_group_outflows == 'H') %>%
  feols(.[net_yvars] ~ disbursed_inflows*us_outflow|user_cc+time, cluster = c('user_cc'))

etable(qmle_net)














