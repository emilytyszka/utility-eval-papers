models_to_include <- c("CEID-Walk", "COVIDhub-4_week_ensemble", "COVIDhub-baseline", "CU-select", "FAIR-NRAR",  "IowaStateLW-STEM", "JHU_IDD-CovidSP", "JHUAPL-Bucky", "LANL-GrowthRate", "LNQ-ens1", "OneQuietNight-ML", "UChicagoCHATTOPADHYAY-UnIT")  

get_forecast_data <- function(forecast_dates, models, locations, timehorizon){
  require(covidHubUtils)
  load_forecasts(
    dates = forecast_dates,
    date_window_size = 6,
    models = models_to_include,
    locations = locations,
    types = c("quantile"),
    targets = paste(1:4, "wk ahead inc case"),
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() |>
    dplyr::filter(horizon == timehorizon) # use "horizon" for JHU data. >1 horizon creates alloscore() merge errors
}

get_truth_data <- function(){
  require(covidHubUtils)
  load_truth(
    truth_source = "JHU",
    target_variable = "inc case"
  ) %>%
    dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA")) | geo_type == "state" & (abbreviation %in% c("CA"))) %>%
    dplyr::mutate(value = pmax(value, 0))
}


get_forecast_scores <- function(forecast_data, truth_data){
  require(covidHubUtils)
  score_forecasts(
    forecasts = forecast_data,
    truth = truth_data,
    use_median_as_point = TRUE
  )
}

filter_problematic_data <- function(forecast_data){
  require(tidyverse)
  forecast_data |>
    ## this removes two state-week-model that has a deterministically zero forecast.
    filter(!(abbreviation=="KS" & reference_date=="2022-02-21" & model=="CU-select")) |>
    filter(!(abbreviation=="WA" & reference_date=="2022-02-28" & model=="CU-select"))
}
