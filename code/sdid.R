#clear memory and setup
rm(list=ls())
options(scipen=999)

library(tidyverse)
library(vroom)
library(lubridate)
library(fixest)
library(modelsummary)
library(synthdid)
library(data.table)
library(Synth)
source('functions.R')
library(data.table)
library(abind)

#load matched trades
data <- vroom('../temporary/data_sdid.csv')
country_data <- read.csv('../temporary/country_data.csv')

#clean outflow data
data$time <- as.Date(data$time)
data[is.na(data)] <- 0

prepare_data <- function(data, country_data, window_start, window_end, disbursement, treated_unit, normalize_to_base_period = FALSE) {
  # Ensure the dates are in Date format
  window_start <- as.Date(window_start)
  window_end <- as.Date(window_end)
  disbursement <- as.Date(disbursement)
  
  # Filter and transform the data
  data_cut <- data %>%
    filter(time >= window_start & time <= window_end) %>%
    mutate(
      treated = ifelse((user_cc == treated_unit & time > disbursement), 1, 0),
      outflow_log = log(outflow + 1)  # Add 1 to avoid log(0)
    ) %>%
    left_join(country_data, by = c('user_cc' = 'alpha.2')) %>%
    drop_na(label)
  
  if (normalize_to_base_period) {
    # Calculate the base period value for normalization
    base_values <- data_cut %>%
      filter(time == min(time)) %>%
      group_by(user_cc) %>%
      summarize(base_outflow = mean(outflow, na.rm = TRUE), .groups = "drop")
    
    # Remove countries with zero base period outflows
    base_values <- base_values %>% filter(base_outflow > 0)
    
    # Merge base values with data_cut
    data_cut <- data_cut %>%
      inner_join(base_values, by = "user_cc") %>%
      mutate(outflow_normalized = (outflow / base_outflow) * 100)
  }
  
  return(data_cut)
}


##Synthetic Control

# predictor_names <- shares_2019 %>% select(NG:BD) %>% colnames()


# data_cut <- data_cut %>%
#   filter(time >= window_start & time <= window_end)

# treated_id <- max(data_cut[data_cut$user_cc == 'US',]$country_number)
# 
# post_id <- min(data_cut[data_cut$treated == 1,]$time_number)
# 
# min_time_id <- min(data_cut$time_number)
# 
# max_time_id <- max(data_cut$time_number)
# 
# min_country_id <- min(data_cut$country_number)
# 
# max_country_id <- max(data_cut$country_number)
# 
# 
# data_scm <- dataprep(foo = as.data.frame(data_cut),
#                      dependent = 'outflow',
#                      unit.variable = 'country_number',
#                      time.variable = 'time_number',
#                      treatment.identifier = 199,
#                      controls.identifier = c(min_country_id:(treated_id - 1), (treated_id + 1):(max_country_id-1)),
#                      time.optimize.ssr = c(min_time_id:(post_id - 1)),
#                      # time.predictors.prior = c(min_time_id:(post_id - 1)),
#                      unit.names.variable = c('user_cc'),
#                      predictors = predictor_names,
#                      time.plot = min_time_id:max_time_id
# )
# 
# synth_out <- synth(data_scm)
# 
# path.plot(synth.res = synth_out,
#           dataprep.res = data_scm,
#           tr.intake = 161)
# 
# gaps.plot(synth.res = synth_out,
#           dataprep.res = data_scm)

###Synthetic DID

# Create X matrix
# 
# flows_shares_weekly_balanced <- as.data.table(flows_shares_weekly_balanced)
# 
# X_matrix <- flows_shares_weekly_balanced %>%
#   melt(id.var = c('user_cc', 'time')) %>%
#   nest_by(variable) %>%
#   mutate(X = list(
#     dcast(data.table(data), user_cc ~ time, value.var = 'value') %>%
#       .[data.table(user_cc = rownames(setup$Y)), on = 'user_cc'] %>%
#       .[, user_cc := NULL] %>%
#       as.matrix()
#   )) %>%
#   .$X %>%
#   abind(along=3)

#Synthdid setup

data_main <- prepare_data(
    data = data,
    country_data = country_data,
    window_start = '2020-01-01',
    # window_end = '2020-06-07',
    window_end = '2020-09-01',
    disbursement = '2020-04-09',
    treated_unit = 'US'  # Example for the US
)

setup = panel.matrices(as.data.frame(data_main),
                       unit = 'label',
                       time = 'time',
                       outcome = 'outflow',
                       treatment = 'treated')

Y <- setup$Y
N0 <- setup$N0
T0 <- setup$T0
  
tau.hat = synthdid_estimate(Y, N0, T0)
sprintf('point estimate: %1.2f', tau.hat)

sdid_main_plot <- synthdid_plot(tau.hat, overlay = 0, effect.alpha = 0, diagram.alpha = 0, treated.name = "US", control.name = "Synthetic Control", se.method='placebo') + scale_alpha_continuous(range= c(0,1)) + guides(alpha = FALSE)

sdid_overlaid_plot <- synthdid_plot(tau.hat, overlay = 1, effect.alpha = 0, diagram.alpha = 0, treated.name = "US", control.name = "Synthetic Control", se.method='placebo') + scale_alpha_continuous(range= c(0,1)) + guides(alpha = FALSE)

ggsave('../output/sdid_plots/sdid_main_plot.png', plot = sdid_main_plot, width = 9, height = 6, dpi = 300)
ggsave('../output/sdid_plots/sdid_overlaid_plot.png', plot = sdid_overlaid_plot, width = 9, height = 6, dpi = 300)

top.controls = synthdid_controls(tau.hat)[1:8, , drop=FALSE]

control_plot <- synthdid_units_plot(tau.hat, units = row.names(top.controls)) + ylab('Estimate')+ theme(strip.text = element_blank())

ggsave('../output/sdid_plots/control_plot.png', plot = control_plot, width = 11, height = 6, dpi = 300)

baseline_outflows_us <- data_main %>%
  filter(user_cc == 'US',
         treated == 0) %>%
  summarize(mean(outflow))

se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('SE: (%1.2f)', se)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)


synthdid_units_plot(
  tau.hat,
  negligible.threshold = 0.001,
  negligible.alpha = 0.3,
  se.method = "jackknife",
  units = NULL
)

#########################

#TIME EFFECTS (Placebo method)

##################Modified function from synthdid package

time_effects <- get_time_effects(tau.hat, replications = 200)

# Add confidence intervals to the data
time_effects <- time_effects %>%
  mutate(lower_ci = treatment_effect - 1.96 * se,
         upper_ci = treatment_effect + 1.96 * se)

time_effects

# Plot the treatment effect with confidence intervals
time_effects_graph <- ggplot(time_effects, aes(x = time, y = treatment_effect)) +
  geom_line(aes(group=1),color = "blue", linewidth = 1) +  # Treatment effect line
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, group=1), alpha = 0.2, fill = "blue") +  # Confidence interval
  geom_point(color = "blue", size = 2) +  # Points for treatment effects
  geom_vline(xintercept = '2020-04-05', color = 'red', linewidth = 0.8) +
  labs(
    title = "Treatment Effect Over Time with Confidence Intervals",
    x = "Time",
    y = "Treatment Effect"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))

show(time_effects_graph)

ggsave('../output/sdid_plots/time_effects_with_CI.png', plot = time_effects_graph, width = 9, height = 6, dpi = 300)

###############

#TIME BASED PLACEBO

###############

#use Jan 2020 instead of April
# data_time_placebo <- prepare_data(
#   data = data,
#   country_data = country_data,
#   window_start = '2019-09-01',
#   # window_end = '2019-06-07',
#   window_end = '2020-09-01',
#   disbursement = '2020-01-01',
#   treated_unit = 'US'  # Example for the US
# )

data_time_placebo <- prepare_data(
  data = data,
  country_data = country_data,
  window_start = '2018-01-01',
  # window_end = '2019-06-07',
  window_end = '2018-09-01',
  disbursement = '2018-04-09',
  treated_unit = 'US'  # Example for the US
)

setup_placebo_time = panel.matrices(as.data.frame(data_time_placebo),
                       unit = 'label',
                       time = 'time',
                       outcome = 'outflow',
                       treatment = 'treated')


tau.hat_time_placebo = synthdid_estimate(setup_placebo_time$Y, setup_placebo_time$N0, setup_placebo_time$T0)
sprintf('point estimate: %1.2f', tau.hat_time_placebo)

sdid_overlaid_plot_time_placebo <- synthdid_plot(tau.hat_time_placebo, overlay = 1, effect.alpha = 0, diagram.alpha = 0, treated.name = "US", control.name = "Synthetic Control") + scale_alpha_continuous(range= c(0,1)) + guides(alpha = FALSE)

show(sdid_overlaid_plot_time_placebo)

time_effects_time_placebo <- get_time_effects(tau.hat_time_placebo, replications = 200)

# Add confidence intervals to the data
time_effects_time_placebo <- time_effects_time_placebo %>%
  mutate(lower_ci = treatment_effect - 1.96 * se,
         upper_ci = treatment_effect + 1.96 * se)

time_effects_time_placebo

# Plot the treatment effect with confidence intervals
time_effects_time_placebo_graph <- ggplot(time_effects_time_placebo, aes(x = time, y = treatment_effect)) +
  geom_line(aes(group=1),color = "blue", linewidth = 1) +  # Treatment effect line
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, group=1), alpha = 0.2, fill = "blue") +  # Confidence interval
  geom_point(color = "blue", size = 2) +  # Points for treatment effects
  geom_vline(xintercept = '2020-01-05', color = 'red', linewidth = 0.8) +
  labs(
    title = "Treatment Effect Over Time with Confidence Intervals",
    x = "Time",
    y = "Treatment Effect"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))

show(time_effects_time_placebo_graph)

##########################

#Test normalization

#########################

data_normalized <- prepare_data(
  data = data,
  country_data = country_data,
  window_start = '2019-10-01',
  # window_end = '2020-06-07',
  window_end = '2020-09-01',
  disbursement = '2020-04-09',
  treated_unit = 'US',
  normalize_to_base_period = TRUE# Example for the US
)

setup_normalized <- panel.matrices(as.data.frame(data_normalized),
               unit = 'label',
               time = 'time',
               outcome = 'outflow_normalized',
               treatment = 'treated')

tau.hat_normalized = synthdid_estimate(setup_normalized$Y, setup_normalized$N0, setup_normalized$T0)
sprintf('point estimate: %1.2f', tau.hat_normalized)

sdid_overlaid_plot_normalized <- synthdid_plot(tau.hat_normalized, overlay = 1, effect.alpha = 0, diagram.alpha = 0, treated.name = "US", control.name = "Synthetic Control") + scale_alpha_continuous(range= c(0,1)) + guides(alpha = FALSE)

show(sdid_overlaid_plot_normalized)










########TEST SYNTH DID

data('california_prop99')

data <- as_tibble(california_prop99)

data <- 
  data.table(california_prop99) %>% 
  .[, x1 := 100 * runif(nrow(.))] %>% 
  .[, x2 := runif(nrow(.))]

setup = panel.matrices(california_prop99)
tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))
sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)

N <- 100
T <- 10

data_test <- 
  expand.grid(
    id = 1:N,
    year = 1:T
  ) %>% 
  data.table() %>% 
  .[, trt_or_ctrl := ifelse(id <= (N / 2), "treatment", "control")] %>% 
  .[, treated := ifelse(trt_or_ctrl == "treatment" & year > (T - 5), 1, 0)] %>% 
  .[, `:=`(
    X_1 = rnorm(nrow(.)),
    X_2 = rnorm(nrow(.))
  )] %>%  
  .[, y := 1 + 1 * treated + rnorm(nrow(.)) + 1 * X_1 + 1 * X_2] 

sdid_setup <- 
  synthdid::panel.matrices(
    data_test[, .(id, year, y, treated)],
    unit = 1,
    time = 2,
    outcome = 3,
    treatment = 4
  )

#/*----------------------------------*/
#' ## Construct X 
#/*----------------------------------*/
X_mat <- 
  data_test[, .(id, year, X_1, X_2)] %>% 
  melt(id.var = c("id", "year")) %>% 
  #=== dataset by variable ===#
  nest_by(variable) %>% 
  mutate(X = list(
    dcast(data.table(data), id ~ year, value.var = "value") %>% 
      #=== order the observations to match that of sdid_setup$Y ===#
      .[data.table(id = as.numeric(rownames(sdid_setup$Y))), on = "id"] %>% 
      .[, id := NULL] %>% 
      as.matrix() 
  )) %>% 
  .$X %>% 
  #=== list of matrices to 3-D array of N * T * C ===#
  # C: number of covariates
  abind(along = 3) 

