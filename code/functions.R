
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
