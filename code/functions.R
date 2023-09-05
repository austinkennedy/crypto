
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
    group_by(time = as.Date(floor_date(date, interval)),
             user_cc) %>%
    summarise(volume = sum({{unit}}))
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












