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

prepare_data <- function(data, country_data, window_start, window_end, disbursement, treated_unit) {
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
    window_end = '2020-06-07',
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

#TEST TIME EFFECTS

#########################

treatment_effects <- synthdid_effect_curve(tau.hat)

# Step 3: Perform placebo tests
placebo_effects <- matrix(NA, nrow = length(treatment_effects), ncol = N0)

for (i in 1:N0) {
  # Remove control unit i from the setup
  placebo_Y <- Y[-i, ]
  placebo_N0 <- N0 - 1
  
  # Estimate synthetic DID for the placebo
  placebo_est <- synthdid_estimate(placebo_Y, placebo_N0, T0)
  
  print(length(synthdid_effect_curve(placebo_est)))
  print(dim(placebo_effects))
  
  # Extract the placebo time-varying effects
  placebo_effects[, i] <- synthdid_effect_curve(placebo_est)
}

# Step 4: Compute standard errors for post-treatment periods
standard_errors <- apply(placebo_effects, 1, sd, na.rm = TRUE)

# Step 5: Combine results
results <- data.frame(
  Period = post_treatment_indices,
  Treatment_Effect = treatment_effects[post_treatment_indices],
  Standard_Error = standard_errors
)

# View results
print(results)

##################Modified function from synthdid package

contract3 = function(X, v) {
  stopifnot(length(dim(X)) == 3, dim(X)[3] == length(v))
  out = array(0, dim = dim(X)[1:2])
  if (length(v) == 0) { return(out) }
  for (ii in 1:length(v)) {
    out = out + v[ii] * X[, , ii]
  }
  return(out)
}

synthdid_effect_curve_all_periods = function(estimate) {
  setup = attr(estimate, 'setup')
  weights = attr(estimate, 'weights')
  X.beta = contract3(setup$X, weights$beta)  # Contribution of covariates
  N1 = nrow(setup$Y) - setup$N0  # Number of treated units
  T = ncol(setup$Y)  # Total number of periods
  T0 = setup$T0  # Number of pre-treatment periods
  
  # Compute synthetic control estimates for all periods
  tau.sc = t(c(-weights$omega, rep(1 / N1, N1))) %*% (setup$Y - X.beta)
  
  # Calculate the pre-treatment weighted mean using weights$lambda
  pre_treatment_mean = c(tau.sc[1:T0] %*% weights$lambda)
  
  # Subtract the pre-treatment mean from all periods
  tau.curve = as.numeric(tau.sc - pre_treatment_mean)
  
  tau.curve  # Return the effect curve for all periods
}

sum_normalize = function(x) {
  if(sum(x) != 0) { x / sum(x) }
  else { rep(1/length(x), length(x)) }
  # if given a vector of zeros, return uniform weights
  # this fine when used in bootstrap and placebo standard errors, where it is used only for initialization
  # for jackknife standard errors, where it isn't, we handle the case of a vector of zeros without calling this function.
}

placebo_se_by_period = function(estimate, replications) {
  setup = attr(estimate, 'setup')
  opts = attr(estimate, 'opts')
  weights = attr(estimate, 'weights')
  N1 = nrow(setup$Y) - setup$N0
  if (setup$N0 <= N1) { stop('must have more controls than treated units to use the placebo se') }
  
  # Identify post-treatment periods
  post_periods = (setup$T0 + 1):ncol(setup$Y)
  num_post_periods = length(post_periods)
  
  # Define modified theta function
  theta = function(ind) {
    N0 = length(ind) - N1
    weights.boot = weights
    weights.boot$omega = sum_normalize(weights$omega[ind[1:N0]])
    est = do.call(synthdid_estimate, c(list(Y=setup$Y[ind,], N0=N0, T0=setup$T0, X=setup$X[ind, ,], weights=weights.boot), opts))
    # Extract period-specific estimates
    period_effects = synthdid_effect_curve_all_periods(est)
    return(period_effects)
  }
  
  # Bootstrap placebo estimates for each period
  placebo_estimates = replicate(replications, theta(sample(1:setup$N0)))
  
  # Compute standard errors for each period
  period_se = apply(placebo_estimates, 1, sd) * sqrt((replications - 1) / replications)
  
  return(period_se)
}

se_by_period = placebo_se_by_period(tau.hat, replications = 200)

all_period_effects <- synthdid_effect_curve_all_periods(tau.hat)

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

