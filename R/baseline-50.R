
score_baseline50_allocation <- function(dat, forecast, Kgrid) {
  require(tidyverse)
  require(reshape2)
  
  # Same procedure as per-cap, but the proportions come from the 0.500 quantile of the COVIDhub-baseline prediction for the target 
  # instead of population data
  
  pred50 <- forecast %>% dplyr::filter(model == "COVIDhub-baseline") %>% filter(quantile==0.500) %>% select(full_location_name, target_end_date, value)
  
  baseline50 <- dat %>% dplyr::filter(model == "COVIDhub-baseline")  %>% merge(Kgrid, by="target_end_date") %>% select(-K, -value) %>% 
    melt(id.vars = c("target_end_date","model", "full_location_name", "x", "y", "oracle", "components_oracle", "components_raw", "components")) %>%
    select(-variable) %>% rename(K = value) %>% 
    relocate(K, .after = target_end_date) %>%
    slice(1, .by = c(target_end_date, K ,full_location_name)) %>%
    mutate(model = "recent-case-counts") %>%
    left_join(pred50, by = c("target_end_date", "full_location_name")) %>%
    group_by(K, target_end_date) %>%
    mutate(estimprop = value / sum(value), .before = x)  %>%
    mutate(x = K * estimprop, components_raw = pmax(y-x,0),
           oracle = y * K / sum(y),
           components_oracle = pmax(y - oracle, 0),
           components = components_raw - components_oracle) %>%
    ungroup() %>%
    select(-estimprop, -value)
  
  return(baseline50)
  
}


