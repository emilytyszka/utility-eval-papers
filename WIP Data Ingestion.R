
library(covidHubUtils)
forecast_dates <- as.character(seq.Date(as.Date("2021-12-18"), as.Date("2022-03-12"), by = "7 days"))
models_to_include <- modellist
locations <- hub_locations |>
  dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA") & location_name != "Los Angeles County")) |>
  dplyr::pull(fips)
timehorizon <- 1

# Forecast Data Pull
forecast_data <- load_forecasts(                                                 # Only changed the target weeks. NB: timehorizon and target_end_date filtering turned off here
    dates = forecast_dates,
    date_window_size = 6,
    models = models_to_include,
    locations = locations,
    types = c("quantile"),
    targets = paste(1:3, "wk ahead inc case"),
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() |>
    dplyr::filter(horizon == timehorizon) |> #|> # use "horizon" for JHU data. >1 horizon creates alloscore() merge errors
    select(-forecast_date)                       # removing "forecast_date" because it isn't used after this and it creates a headache in ensembling
    #dplyr::filter(target_end_date == date_of_interest) # adding the option to narrow the data being processed to just the dates where we want N week horizon


# Create Ensemble of Available Forecasts for a Given Day
forecast_data <- forecast_data %>% group_by(reference_date, location, horizon, relative_horizon, temporal_resolution, target_variable, 
                                                target_end_date, type, quantile, location_name, population, geo_type, geo_value, abbreviation, 
                                                full_location_name) %>%                                 # everything is grouped except for "model" and "value"
                                       summarise(value=mean(value)) %>%                                 # calculates mean estimate for the quantile + date + location
                                       mutate(model = "eligible-models-ensemble") %>% 
                                       ungroup() %>% rbind(forecast_data)


# Truth Data Pull
truth_test <- load_truth(                                                        # TWEAKED THE FILTERING
  truth_source = "JHU",
  target_variable = "inc case"
) %>%
  dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA") & location_name != "Los Angeles County")) %>%
  dplyr::mutate(value = pmax(value, 0))


# Scores
get_forecast_scores <- function(forecast_data, truth_data){                      # NO ISSUES
  require(covidHubUtils)
  score_forecasts(
    forecasts = forecast_data,
    truth = truth_data,
    use_median_as_point = TRUE
  )
}



filter_problematic_data <- function(forecast_data){                              # THIS IS NO LONGER NEEDED
  require(tidyverse)
  forecast_data |>
    ## this removes two state-week-model that has a deterministically zero forecast.
    filter(!(abbreviation=="KS" & reference_date=="2022-02-21" & model=="CU-select")) |>
    filter(!(abbreviation=="WA" & reference_date=="2022-02-28" & model=="CU-select"))
}



####### TESTING
generate_ensemble(get_forecast_data(values$forecast_dates, models = models_to_include, 
                  locations = reqd_locs, timehorizon=timehorizon1))
