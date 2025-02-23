
get_clean_fb_acs <- function(yr, type){
  #Requires tidycensus, tidyverse
  #Retrieves and cleans acs data for foreign-born population in us
  #type must be "acs1" or "acs5"
  
  acs <- get_acs(geography = "us", table = "B05006", year = yr, survey = type, cache_table = TRUE)
  variables <- load_variables(yr, type, cache = TRUE) #get all acs variables
  
  acs <- acs %>% 
    left_join(variables, by = c("variable" = "name")) %>% #get place of birth
    mutate(label = gsub('.*!', '', label)) %>% #remove wonky labeling in front of countries (why do they do it this way??) %>%
    subset(select = -c(concept, variable, NAME, GEOID))
  
  
  
  return(acs)
    
  
}

get_total_volume <- function(data, unit, interval){
  #get volume at different intervals
  #'data' should be a dataframe of matched or unmatched crypto trades
  #'unit' indicates the desired currency, right now either "amount" (BTC) or "amount_usd" (USD)
  #'interval' indicates the desired interval. Provide a string such as "day", "week", "month", etc
  df <- data %>%
    group_by(time = as.Date(floor_date(date, interval))) %>%
    summarise(volume = sum({{unit}}))
  
  return(df)
}

outflow_volume_total <- function(data, unit, interval){
  #get volume at different intervals
  #'data' should be a dataframe of matched or unmatched crypto trades
  #'unit' indicates the desired currency, right now either "amount" (BTC) or "amount_usd" (USD)
  #'interval' indicates the desired interval. Provide a string such as "day", "week", "month", etc
  df <- data %>%
    group_by(time = as.Date(floor_date(date, interval))) %>%
    summarise(volume = sum({{unit}}))
  
  return(df)
}

outflow_volume_country <- function(data, unit, interval){
  #get volume at different intervals, by receiving country
  #'data' should be a dataframe of matched or unmatched crypto trades
  #'unit' indicates the desired currency, right now either "amount" (BTC) or "amount_usd" (USD)
  #'interval' indicates the desired interval. Provide a string such as "day", "week", "month", etc
  df <- data %>%
    group_by(time = as.Date(floor_date(date, interval)),
             user_cc,
             user_cc2) %>%
    summarise(volume = sum({{unit}}))
  
  return(df)
}

outflow_volume_origin <- function(data, unit, interval){
  #get total outflows at different intervals for each origin country
  #'data' should be a dataframe of matched or unmatched crypto trades
  #'unit' indicates the desired currency, right now either "amount" (BTC) or "amount_usd" (USD)
  #'interval' indicates the desired interval. Provide a string such as "day", "week", "month", etc
  df <- data %>%
    filter(user_cc != user_cc2) %>%
    group_by(time = as.Date(floor_date(date, interval)),
             user_cc) %>%
    summarise(volume = sum({{unit}}))
}

getFlows <- function(data, unit, interval){
  #get volume at different intervals, by receiving country
  #'data' should be a dataframe of matched or unmatched crypto trades
  #'unit' indicates the desired currency, right now either "amount" (BTC) or "amount_usd" (USD)
  #'interval' indicates the desired interval. Provide a string such as "day", "week", "month", etc
  df <- data %>%
    group_by(time = as.Date(floor_date(date, interval)),
             user_cc,
             user_cc2) %>%
    summarise(volume = sum({{unit}}))
  
  return(df)
}

balanceFlows <- function(data){
  dates <- unique(data$time)
  origins <- unique(data$user_cc)
  destinations <- unique(data$user_cc2)
  
  panel <- as_tibble(CJ(dates, origins, destinations)) %>%
    rename(time = dates, user_cc = origins, user_cc2 = destinations) %>%
    drop_na()
  
  # panel <- panel %>% mutate(country_number = as.numeric(factor(user_cc)),
  #                           time_number = as.numeric(factor(time)))
  
  df <- panel %>%
    left_join(data, by = c('time', 'user_cc', 'user_cc2')) %>%
    replace(is.na(.), 0)

  return(df)
}

trade_count <- function(data, interval){
  #Get number of trades over intervals
  #'data' should be a df of matched or unmatched crypto trades
  #'interval' indicates the desired interval. Provide a string such as 'day', 'week', 'month', etc
  df <- data %>%
    group_by(time = as.Date(floor_date(date, interval)),
             user_cc2) %>%
    summarise(total_trades = n())
  
  return(df)
}

getVolume <- function(data, unit, interval){
  #get volume at different intervals
  #'data' should be a dataframe of matched or unmatched crypto trades
  #'unit' indicates the desired currency, right now either "amount" (BTC) or "amount_usd" (USD)
  #'interval' indicates the desired interval. Provide a string such as "day", "week", "month", etc
  df <- data %>%
    group_by(time = as.Date(floor_date(date, interval))) %>%
    summarise(volume = sum({{unit}}))
  
  return(df)
}

getVolumePrice <- function(data, unit, interval){
  #get volume at different intervals
  #'data' should be a dataframe of matched or unmatched crypto trades
  #'unit' indicates the desired currency, right now either "amount" (BTC) or "amount_usd" (USD)
  #'interval' indicates the desired interval. Provide a string such as "day", "week", "month", etc
  df <- data %>%
    group_by(time = as.Date(floor_date(date, interval))) %>%
    summarise(volume = sum({{unit}}), price = mean(crypto_rate_usd))
  
  return(df)
}

###############Modified functions from synthdid package

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

get_time_effects = function(estimate, replications) {
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
  all_period_effects <- synthdid_effect_curve_all_periods(estimate)
  
  results <- data.frame(
    time = colnames(setup$Y),
    treatment_effect = all_period_effects,
    se = period_se
  )
  
  
  return(results)
}


prepare_data_synth <- function(data, country_data, window_start, window_end, disbursement, treated_unit, normalize_to_base_period = FALSE, normalize_by_growth_rate = FALSE) {
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
  
  if (normalize_by_growth_rate) {
    data_cut <- data_cut %>%
      group_by(user_cc) %>%
      filter(!any(outflow==0)) %>%
      arrange(time) %>%
      mutate(outflow_normalized = (outflow - lag(outflow)) / lag(outflow)) %>%
      ungroup() %>%
      filter(!is.na(outflow_normalized))
  }
  
  return(data_cut)
}

get_confidence_intervals <- function(data, time_effects, country){
  baseline_mean <- data %>%
    filter(user_cc == country & treated == 0) %>%
    summarize(pre_treatment_mean = mean(outflow, na.rm = TRUE)) %>%
    pull(pre_treatment_mean)
  
  data_with_ci <- time_effects %>%
    mutate(lower_ci = treatment_effect - 1.96 * se,
           upper_ci = treatment_effect + 1.96 * se,
           treatment_effect_relative = treatment_effect/baseline_mean,
           lower_ci_relative = lower_ci/baseline_mean,
           upper_ci_relative = upper_ci/baseline_mean)
  
  return(data_with_ci)
  
}

transform_qmle <- function(es_model){
  beta <- es_model$coefficients
  se <- es_model$se
  
  beta_transformed <- exp(beta) - 1
  se_transformed <- exp(beta) * se
  
  lower_ci <- beta_transformed - (1.96 * se_transformed)
  upper_ci <- beta_transformed + (1.96 * se_transformed)

  # lower_ci <- beta_transformed - (1.645 * se_transformed)
  # upper_ci <- beta_transformed + (1.645 * se_transformed)
  
  # Compute Z-score and p-value
  z_stat <- beta / se
  p_values <- 2 * (1 - pnorm(abs(z_stat)))  # Two-tailed test
  
  
  results <- data.frame(
    Estimate = beta_transformed,
    Std_Error = se_transformed,
    CI_Lower = lower_ci,
    CI_Upper = upper_ci,
    P_Value = p_values
  )
  
  #add reference period
  ref_index <- es_model$model_matrix_info[[1]]$ref_id
  ref_date <- es_model$model_matrix_info[[1]]$ref
  
  ref_row <- data.frame(
    Estimate = 0,
    Std_Error = 0,
    CI_Lower = 0,
    CI_Upper = 0,
    P_Value = 1
  )
  
  #insert reference period into results
  results <- results %>%
    add_row(ref_row, .before = ref_index)
  
  results$date <- es_model$model_matrix_info[[1]]$items
  
  return(results)
}



plot_transformed_es <- function(es_model, title, filename){
  
  transformed = transform_qmle(es_model)
  
  p <- ggplot(transformed, aes(x = date, y = Estimate)) +
    geom_vline(xintercept = as.Date('2020-04-05'), linetype = "dashed", color = "red", linewidth = 1) +  # Reference period line
    geom_point(size = 3, color = "blue", position=position_dodge(width=1)) +  # Plot point estimates
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 2, color = "blue", position=position_dodge(width=1)) +  # Add CIs
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Zero effect line
    labs(
      title = title,
      x = "Date",
      y = "Estimate (ATE%)"
      ) +
    scale_x_date(labels = transformed$date, breaks = transformed$date, minor_breaks = transformed$date) +
    # ylim(-0.6, 1) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 13),
          axis.text.y = element_text(size = 13),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 13),
          plot.title = element_text(size = 20, hjust = 0.5))
  
  #save plot
  filepath = paste('../output/event_study_plots/', filename, '.png', sep = '')
  ggsave(filepath, p, width = 11, height = 8, dpi = 300)
  
  return(p)
  
}


