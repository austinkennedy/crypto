
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
  
  panel <- as_tibble(CJ(dates, origins, destinations)) %>% rename(time = dates, user_cc = origins, user_cc2 = destinations)
  
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








