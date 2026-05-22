
score_per_capita_allocation <- function(dat, pops, Kgrid) {
  require(tidyverse)
  require(reshape2)
  percap <- dat %>%
    dplyr::filter(model == "COVIDhub-baseline") %>% merge(Kgrid, by="target_end_date") %>%
    select(-K, -value) %>% 
    melt(id.vars = c("target_end_date","model", "full_location_name", "x", "y", "oracle", "components_oracle", "components_raw", "components")) %>%
    select(-variable) %>% rename(K = value) %>% 
    relocate(K, .after = target_end_date) %>%
    slice(1, .by = c(target_end_date, K ,full_location_name)) %>%
    mutate(model = "per-capita") %>%
    left_join(pops[c('POPESTIMATE2021', 'full_location_name')], by = "full_location_name") %>%
    group_by(K, target_end_date) %>%
    mutate(popprop = POPESTIMATE2021 / sum(POPESTIMATE2021), .before = x)  %>%
    mutate(x = K * popprop, components_raw = pmax(y-x,0),
           oracle = y * K / sum(y),
           components_oracle = pmax(y - oracle, 0),
           components = components_raw - components_oracle) %>%
    ungroup() %>%
    select(-popprop, -POPESTIMATE2021)
  
  return(percap)
}
