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

create_X_matrix <- function(data, setup, idvar, timevar){
  data <- as.data.table(data) %>%
      melt(id.var = c(idvar, timevar)) %>%
      nest_by(variable) %>%
      mutate(X = list(
        dcast(data.table(data), user_cc ~ time, value.var = 'value') %>%
          .[data.table(user_cc = rownames(setup$Y)), on = 'user_cc'] %>%
          .[, user_cc := NULL] %>%
          as.matrix()
      )) %>%
      .$X %>%
      abind(along=3)
}

data_main <- prepare_data_synth(
    data = data,
    country_data = country_data,
    # window_start = '2020-01-01',
    window_start = '2019-11-01',
    # window_end = '2020-06-07',
    window_end = '2020-09-01',
    disbursement = '2020-04-09',
    treated_unit = 'US'  # Example for the US
)

X_mat = data_main %>%
  select(user_cc, time, treated, BJ:ZA)

baseline_mean <- data_main %>%
  filter(user_cc == 'US' & treated == 0) %>%
  summarize(pre_treatment_mean = mean(outflow, na.rm = TRUE)) %>%
  pull(pre_treatment_mean)

# X_mat <- data_main[, c("user_cc", "time", "treated", BJ:ZA)]  # Replace BJ:ZA with actual column names if needed

# Step 2: Create an empty list to store individual matrices
X_list <- list()

# Step 3: Loop through the covariate columns to create matrices for each covariate
for (i in 4:ncol(X_mat)) {
  # Generate the matrix for the current covariate
  X_list[[i - 3]] <- panel.matrices(
    as.data.frame(X_mat), 
    unit = "user_cc", 
    time = "time", 
    outcome = colnames(X_mat)[i],  # Use the current covariate column name
    treatment = "treated"
  )$Y
}

# Step 4: Combine all covariate matrices along a new dimension (3rd dimension)
control <- abind::abind(X_list, along = 3)

setup = panel.matrices(as.data.frame(data_main),
                       unit = 'label',
                       time = 'time',
                       outcome = 'outflow',
                       treatment = 'treated')

# X = create_X_matrix(data = X_input, setup = setup, idvar = 'user_cc', timevar = 'time')

Y <- setup$Y
N0 <- setup$N0
T0 <- setup$T0
  
tau.hat = synthdid_estimate(Y,
                            N0,
                            T0,
                            X = control
                            )
sprintf('point estimate: %1.2f', tau.hat)

sprintf('Estimate relative to baseline mean: %1.5f', tau.hat/baseline_mean )

dates <- as.Date(colnames(setup$W))
dates_rescaled <- dates[seq(1,length(dates), 2)]

sdid_main_plot <- synthdid_plot(tau.hat, overlay = 0, effect.alpha = 0, diagram.alpha = 0, treated.name = "US", control.name = "Synthetic Control") + scale_alpha_continuous(range= c(0,1)) + guides(alpha = FALSE) +
  scale_x_continuous(labels = dates_rescaled,
                     breaks = dates_rescaled,
                     minor_breaks = dates_rescaled) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13))

show(sdid_main_plot)

sdid_overlaid_plot <- synthdid_plot(tau.hat, overlay = 1, effect.alpha = 0, diagram.alpha = 0, treated.name = "US", control.name = "Synthetic Control") + scale_alpha_continuous(range= c(0,1)) + guides(alpha = FALSE) +
  scale_x_continuous(labels = dates_rescaled,
                     breaks = dates_rescaled,
                     minor_breaks = dates_rescaled) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13))

show(sdid_overlaid_plot)

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
sprintf('SE: (%1.5f)', se)
sprintf('SE relative to baseline: %1.5f', se / baseline_mean)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
# Compute Z-score and p-value




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
         upper_ci = treatment_effect + 1.96 * se,
         treatment_effect_relative = treatment_effect/baseline_mean,
         lower_ci_relative = lower_ci/baseline_mean,
         upper_ci_relative = upper_ci/baseline_mean)

dates <- as.Date(colnames(setup$W))
dates_rescaled <- dates[seq(1,length(dates), 2)]

# Plot the treatment effect with confidence intervals
time_effects_graph <- ggplot(time_effects, aes(x = time, y = treatment_effect)) +
  geom_line(aes(group=1),color = "black", linewidth = 1) +  # Treatment effect line
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, group=1), alpha = 0.2, fill = "black") +  # Confidence interval
  geom_point(color = "black", size = 2) +  # Points for treatment effects
  geom_vline(xintercept = as.Date('2020-04-05'), color = 'black', linewidth = 0.8, linetype='longdash') +
  scale_x_date(labels = dates_rescaled,
                     breaks = dates_rescaled,
                     minor_breaks = dates_rescaled) +
  labs(
    x = "Time",
    y = "Treatment Effect"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle=90, vjust = 1, hjust = 1))

show(time_effects_graph)

ggsave('../output/sdid_plots/time_effects_with_CI.png', plot = time_effects_graph, width = 9, height = 6, dpi = 300)

#Plot relative treatment effects
time_effects_relative_graph <- ggplot(time_effects, aes(x = time, y = treatment_effect_relative)) +
  geom_line(aes(group=1),color = "black", linewidth = 1) +  # Treatment effect line
  geom_ribbon(aes(ymin = lower_ci_relative, ymax = upper_ci_relative, group=1), alpha = 0.2, fill = "black") +  # Confidence interval
  geom_point(color = "black", size = 2) +  # Points for treatment effects
  geom_vline(xintercept = as.Date('2020-04-05'), color = 'black', linewidth = 0.8, linetype='longdash') +
  scale_x_date(labels = dates_rescaled,
               breaks = dates_rescaled,
               minor_breaks = dates_rescaled) +
  labs(
    x = "Time",
    y = "Treatment Effect, Relative to Pre-Treatment Mean"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))

show(time_effects_relative_graph)

ggsave('../output/sdid_plots/time_effects_relative_with_CI.png', plot = time_effects_relative_graph, width = 9, height = 6, dpi = 300)


###############

#TIME BASED PLACEBO, JAN 2020

###############

# use Jan 2020 instead of April
data_jan_2020_placebo <- prepare_data_synth(
  data = data,
  country_data = country_data,
  window_start = '2019-09-01',
  # window_end = '2019-06-07',
  window_end = '2020-09-01',
  disbursement = '2020-01-01',
  treated_unit = 'US'  # Example for the US
)

setup_jan_2020_placebo = panel.matrices(as.data.frame(data_jan_2020_placebo),
                       unit = 'label',
                       time = 'time',
                       outcome = 'outflow',
                       treatment = 'treated')

X_mat = data_jan_2020_placebo %>%
  select(user_cc, time, treated, BJ:ZA)

# X_mat <- data_main[, c("user_cc", "time", "treated", BJ:ZA)]  # Replace BJ:ZA with actual column names if needed

# Step 2: Create an empty list to store individual matrices
X_list <- list()

# Step 3: Loop through the covariate columns to create matrices for each covariate
for (i in 4:ncol(X_mat)) {
  # Generate the matrix for the current covariate
  X_list[[i - 3]] <- panel.matrices(
    as.data.frame(X_mat), 
    unit = "user_cc", 
    time = "time", 
    outcome = colnames(X_mat)[i],  # Use the current covariate column name
    treatment = "treated"
  )$Y
}

# Step 4: Combine all covariate matrices along a new dimension (3rd dimension)
control <- abind::abind(X_list, along = 3)


tau.hat_jan_2020_placebo = synthdid_estimate(
  setup_jan_2020_placebo$Y,
  setup_jan_2020_placebo$N0,
  setup_jan_2020_placebo$T0,
  X = control
  )
sprintf('point estimate: %1.2f', tau.hat_jan_2020_placebo)

dates <- as.Date(colnames(setup_jan_2020_placebo$W))
dates_rescaled <- dates[seq(1,length(dates), 2)]

sdid_main_plot_jan_2020_placebo <- synthdid_plot(tau.hat_jan_2020_placebo, overlay = 0, effect.alpha = 0, diagram.alpha = 0, treated.name = "US", control.name = "Synthetic Control") +
  scale_alpha_continuous(range= c(0,1)) +
  guides(alpha = FALSE) +
  scale_x_continuous(labels = dates_rescaled,
               breaks = dates_rescaled,
               minor_breaks = dates_rescaled) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13))

show(sdid_main_plot_jan_2020_placebo)

sdid_overlaid_plot_jan_2020_placebo <- synthdid_plot(tau.hat_jan_2020_placebo, overlay = 1, effect.alpha = 0, diagram.alpha = 0, treated.name = "US", control.name = "Synthetic Control") + scale_alpha_continuous(range= c(0,1)) + guides(alpha = FALSE) +
  scale_x_continuous(labels = dates_rescaled,
                     breaks = dates_rescaled,
                     minor_breaks = dates_rescaled) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13))

show(sdid_overlaid_plot_jan_2020_placebo)

ggsave('../output/sdid_plots/sdid_main_plot_jan_2020_placebo.png', plot = sdid_main_plot_jan_2020_placebo, width = 9, height = 6, dpi = 300)
ggsave('../output/sdid_plots/sdid_overlaid_plot_jan_2020_placebo.png', plot = sdid_overlaid_plot_jan_2020_placebo, width = 9, height = 6, dpi = 300)


time_effect_jan_2020_placebo <- get_time_effects(tau.hat_jan_2020_placebo, replications = 1000)


time_effect_jan_2020_placebo <- get_confidence_intervals(data_jan_2020_placebo, time_effect_jan_2020_placebo, 'US')

# Plot the treatment effect with confidence intervals
time_effect_jan_2020_placebo_graph <- ggplot(time_effect_jan_2020_placebo, aes(x = time, y = treatment_effect_relative)) +
  geom_line(aes(group=1),color = "black", linewidth = 1) +  # Treatment effect line
  geom_ribbon(aes(ymin = lower_ci_relative, ymax = upper_ci_relative, group=1), alpha = 0.2, fill = "black") +  # Confidence interval
  geom_point(color = "black", size = 2) +  # Points for treatment effects
  geom_vline(xintercept = '2020-01-05', color = 'black', linewidth = 0.8, linetype = 'longdash') +
  labs(
    x = "Time",
    y = "Treatment effect, relative to pre-treatment mean"
  ) +
  theme_bw(base_size = 14) +
  ylim(-0.15, 0.55) +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))

show(time_effect_jan_2020_placebo_graph)

ggsave('../output/sdid_plots/jan_2020_placebo_time_effects.png', plot = time_effect_jan_2020_placebo_graph, width = 9, height = 6, dpi = 300)
########################################

#2018 as a placebo

########################################


data_2018_placebo <- prepare_data_synth(
  data = data,
  country_data = country_data,
  window_start = '2017-11-01',
  # window_end = '2019-06-07',
  window_end = '2018-09-01',
  disbursement = '2018-04-09',
  treated_unit = 'US'  # Example for the US
)

setup_2018_placebo = panel.matrices(as.data.frame(data_2018_placebo),
                                        unit = 'label',
                                        time = 'time',
                                        outcome = 'outflow',
                                        treatment = 'treated')

X_mat = data_2018_placebo %>%
  select(user_cc, time, treated, BJ:ZA)

# X_mat <- data_main[, c("user_cc", "time", "treated", BJ:ZA)]  # Replace BJ:ZA with actual column names if needed

# Step 2: Create an empty list to store individual matrices
X_list <- list()

# Step 3: Loop through the covariate columns to create matrices for each covariate
for (i in 4:ncol(X_mat)) {
  # Generate the matrix for the current covariate
  X_list[[i - 3]] <- panel.matrices(
    as.data.frame(X_mat), 
    unit = "user_cc", 
    time = "time", 
    outcome = colnames(X_mat)[i],  # Use the current covariate column name
    treatment = "treated"
  )$Y
}

# Step 4: Combine all covariate matrices along a new dimension (3rd dimension)
control <- abind::abind(X_list, along = 3)



tau.hat_2018_placebo = synthdid_estimate(
  setup_2018_placebo$Y,
  setup_2018_placebo$N0,
  setup_2018_placebo$T0,
  X = control
  )

sprintf('point estimate: %1.2f', tau.hat_2018_placebo)

dates <- as.Date(colnames(setup_2018_placebo$W))
dates_rescaled <- dates[seq(1,length(dates), 2)]

sdid_main_plot_2018_placebo <- synthdid_plot(tau.hat_2018_placebo, overlay = 0, effect.alpha = 0, diagram.alpha = 0, treated.name = "US", control.name = "Synthetic Control") +
  scale_alpha_continuous(range= c(0,1)) +
  guides(alpha = FALSE) +
  scale_x_continuous(labels = dates_rescaled,
                     breaks = dates_rescaled,
                     minor_breaks = dates_rescaled) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13))

show(sdid_main_plot_2018_placebo)

sdid_overlaid_plot_2018_placebo <- synthdid_plot(tau.hat_2018_placebo, overlay = 1, effect.alpha = 0, diagram.alpha = 0, treated.name = "US", control.name = "Synthetic Control") +
  scale_alpha_continuous(range= c(0,1)) +
  guides(alpha = FALSE) +
  scale_x_continuous(labels = dates_rescaled,
                     breaks = dates_rescaled,
                     minor_breaks = dates_rescaled) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 13))

show(sdid_overlaid_plot_2018_placebo)

ggsave('../output/sdid_plots/sdid_main_plot_2018_placebo.png', plot = sdid_main_plot_2018_placebo, width = 9, height = 6, dpi = 300)
ggsave('../output/sdid_plots/sdid_overlaid_plot_2018_placebo.png', plot = sdid_overlaid_plot_2018_placebo, width = 9, height = 6, dpi = 300)

time_effect_2018_placebo <- get_time_effects(tau.hat_2018_placebo, replications = 1000)

# Add confidence intervals to the data
time_effect_2018_placebo <- get_confidence_intervals(data_2018_placebo, time_effect_2018_placebo, 'US')

time_effect_2018_placebo

# Plot the treatment effect with confidence intervals
time_effect_2018_placebo_graph <- ggplot(time_effect_2018_placebo, aes(x = time, y = treatment_effect_relative)) +
  geom_line(aes(group=1),color = "black", linewidth = 1) +  # Treatment effect line
  geom_ribbon(aes(ymin = lower_ci_relative, ymax = upper_ci_relative, group=1), alpha = 0.2, fill = "black") +  # Confidence interval
  geom_point(color = "black", size = 2) +  # Points for treatment effects
  geom_vline(xintercept = '2018-04-08', color = 'black', linewidth = 0.8, linetype = 'longdash') +
  labs(
    x = "Time",
    y = "Treatment effect, relative to pre-treatment mean"
  ) +
  ylim(-0.2, 0.55) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))

show(time_effect_2018_placebo_graph)

ggsave('../output/sdid_plots/2018_placebo_time_effects.png', plot = time_effect_2018_placebo_graph, width = 9, height = 6, dpi = 300)


##########################

#Drop Nigeria

##########################

# data_drop_nigeria <- data_main %>%
#   filter(! user_cc %in% c("NG", "GH"))

data_drop_nigeria <- data_main %>%
  filter(income_group %in% c("H"))

X_mat = data_drop_nigeria %>%
  select(user_cc, time, treated, BJ:ZA)

# X_mat <- data_main[, c("user_cc", "time", "treated", BJ:ZA)]  # Replace BJ:ZA with actual column names if needed

# Step 2: Create an empty list to store individual matrices
X_list <- list()

# Step 3: Loop through the covariate columns to create matrices for each covariate
for (i in 4:ncol(X_mat)) {
  # Generate the matrix for the current covariate
  X_list[[i - 3]] <- panel.matrices(
    as.data.frame(X_mat), 
    unit = "user_cc", 
    time = "time", 
    outcome = colnames(X_mat)[i],  # Use the current covariate column name
    treatment = "treated"
  )$Y
}

# Step 4: Combine all covariate matrices along a new dimension (3rd dimension)
control <- abind::abind(X_list, along = 3)

setup_drop_nigeria = panel.matrices(as.data.frame(data_drop_nigeria),
                                     unit = 'label',
                                     time = 'time',
                                     outcome = 'outflow',
                                     treatment = 'treated')

tau.hat_drop_nigeria <- synthdid_estimate(
  setup_drop_nigeria$Y,
  setup_drop_nigeria$N0,
  setup_drop_nigeria$T0,
  X = control
)

sprintf('point estimate: %1.2f', tau.hat_drop_nigeria)

time_effects_drop_nigeria <- get_time_effects(tau.hat_drop_nigeria, replications = 1000)

time_effects_drop_nigeria <- get_confidence_intervals(data_drop_nigeria, time_effects_drop_nigeria, 'US')

# Plot the treatment effect with confidence intervals
time_effects_drop_nigeria_graph <- ggplot(time_effects_drop_nigeria, aes(x = time, y = treatment_effect_relative)) +
  geom_line(aes(group=1),color = "black", linewidth = 1) +  # Treatment effect line
  geom_ribbon(aes(ymin = lower_ci_relative, ymax = upper_ci_relative, group=1), alpha = 0.2, fill = "black") +  # Confidence interval
  geom_point(color = "black", size = 2) +  # Points for treatment effects
  geom_vline(xintercept = '2020-04-05', color = 'black', linewidth = 0.8, linetype = 'longdash') +
  labs(
    x = "Time",
    y = "Treatment effect, relative to pre-treatment mean"
  ) +
  theme_bw(base_size = 14) +
  # ylim(-0.15, 0.55) +
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))

show(time_effects_drop_nigeria_graph)


##########################

#Test normalization

#########################

data_normalized <- prepare_data_synth(
  data = data,
  country_data = country_data,
  window_start = '2019-10-01',
  # window_end = '2020-06-07',
  window_end = '2020-09-01',
  disbursement = '2020-04-09',
  treated_unit = 'US',
  # normalize_to_base_period = TRUE# Example for the US,
  normalize_by_growth_rate = TRUE
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


#########################################

#Compute Placebo distribution

# Compute placebo treatment effects
compute_placebo_sdid <- function(placebo_unit) {
  placebo_data <- data_main %>% mutate(treated = ifelse(user_cc == placebo_unit & time >= '2020-04-09', 1, 0))
  
  pre_treatment_mean_placebo <- placebo_data %>%
    filter(user_cc == placebo_unit & time < '2020-04-09') %>%
    summarise(mean_outflow = mean(outf low, na.rm = TRUE)) %>%
    pull(mean_outflow)
  
  tryCatch({
    result <- synthdid_estimate(
      Y = panel.matrices(as.data.frame(placebo_data), unit = 'user_cc', time = 'time', outcome = 'outflow', treatment = 'treated')$Y,
      N0 = setup$N0,
      T0 = setup$T0,
      X = control
    )
    return((result / pre_treatment_mean_placebo) * 100)
  }, error = function(e) return(NA))
}

control_countries <- data_main %>%
  group_by(user_cc) %>%
  filter(user_cc != 'US' & all(outflow != 0)) %>%
  pull(user_cc) %>%
  unique()

placebo_effects <- map_dbl(control_countries, compute_placebo_sdid)

placebo_df <- data.frame(effect = placebo_effects)

# Calculate pre-treatment mean for US
pre_treatment_mean_us <- data_main %>%
  filter(user_cc == 'US' & time < '2020-04-09') %>%
  summarise(mean_outflow = mean(outflow, na.rm = TRUE)) %>%
  pull(mean_outflow)

# Convert to percent change
tau.hat_percent <- (tau.hat / pre_treatment_mean_us) * 100

sprintf('point estimate (percent change): %1.2f%%', tau.hat_percent)




placebo_histogram <- ggplot(placebo_df, aes(x = effect)) +
  geom_histogram(bins = 100, fill = 'blue', alpha=0.5) +
  # geom_density(adjust = 3) +
  geom_vline(xintercept = tau.hat_percent, color = 'red', linetype = 'dashed', size = 1) +
  labs(x = 'Placebo Treatment Effects',
       y = 'Frequency') +
  theme_minimal()

show(placebo_histogram)





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

