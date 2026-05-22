## engine to run the alloscores for one reference date and one model - this time matching to the resource constraint of interest
run_alloscore_match_k <- function(
    forecast_data,
    truth_data,
    one_reference_date,
    one_model,
    Kgrid,
    slim = TRUE){
  require(tidyverse)
  require(alloscore)
  require(distfromq)
  
  ## process forecast data, adding distfromq output
  forecast_data_processed <- forecast_data |>
    ## forecast dates are different but reference dates are Mondays
    dplyr::filter(target_end_date %in% as.Date(one_reference_date)) |>
    dplyr::select(-type) |>
    nest(ps = quantile, qs = value) |>
    relocate(ps, qs) |>
    mutate(
      ps = map(ps, deframe),
      qs = map(qs, deframe)
    ) |>
    dplyr::filter(model == one_model) %>%
    dplyr::mutate(
      model = ifelse(model == "COVIDhub-4_week_ensemble", "COVIDhub-ensemble", model)
    ) |>
    add_pdqr_funs(dist = "distfromq", types = c("p", "q")) |>
    relocate(dist, F, Q) |>
    left_join(
      truth_data |> select(location, target_end_date, value),
      by = c("location", "target_end_date")) %>% 
    distinct(model, horizon, location_name, target_end_date, .keep_all = TRUE) # adding this because of known duplicate submission in some models
  
    # Ks that match date of interest - this way of getting the K vector is the major difference between this function and run_alloscore. 
    Ks <- Kgrid %>% dplyr::filter(`target_end_date` == one_reference_date) %>% select(-target_end_date, -value) %>% as.numeric()
  
  if (nrow(forecast_data_processed) > 0) {   
    ## run alloscore
    ascores <- forecast_data_processed %>%
      alloscore(
        K = Ks,
        y = .[["value"]],
        target_names = "full_location_name",
        slim = TRUE
      )
    ascores <- ascores %>% dplyr::mutate(
      target_end_date = one_reference_date,
      model = one_model,
      .before = 1
    )
  } else {
    ascores <- tibble(
      target_end_date = one_reference_date,
      model = one_model,
      message = "no forecasts"
    )
  }
  return(ascores)
}

## engine to run the alloscores for one reference date and all models sequentially - WITH NEW K METHOD
run_alloscore_one_date_match_k <- function(
    forecast_data,
    truth_data,
    one_reference_date,
    Kgrid = NULL) {
  require(tidyverse)
  require(alloscore)
  require(distfromq)
  
  ## vector of all models present in data
  models <- unique(forecast_data$model)
  
  ## for each model, compute allocation, return as df
  map(models,
      \(x) run_alloscore_match_k(forecast_data, truth_data, one_reference_date, one_model = x, Kgrid)) %>%
    list_rbind()
}

# Now run over all dates - AND MATCH K
run_and_assemble_alloscores_match_k <- function(
    forecast_data,
    truth_data,
    reference_dates,
    Kgrid) {
  require(tidyverse)
  require(alloscore)
  require(distfromq)
  
  ## for each reference date, compute allocation, return as df
  map(reference_dates,
      \(x) alloscore::slim(run_alloscore_one_date_match_k(forecast_data,
                                                  truth_data,
                                                  one_reference_date = x,
                                                  Kgrid = Kgrid),
                           id_cols = c("model", "target_end_date"))) %>%
    list_rbind()
}