determine_eligible_models <- function(forecast_dates, locations){
  require(covidHubUtils)
  require(tidyverse)

  ## Drop several covidhub ensembles: COVIDhub-4_week_ensemble, COVIDhub_CDC-ensemble (keep trained and COVIDhub-ensemble)
  ## drop CU non-primary models: CU-nochange, CU-scenario_low, CU-scenario_mid
  
  models_to_drop <- c("COVIDhub-4_week_ensemble", "COVIDhub_CDC-ensemble",
                      "CU-nochange", "CU-scenario_low", "CU-scenario_mid")

  ## load and filter forecasts
  load_forecasts(
    dates = forecast_dates,
    date_window_size = 6,
    types = c("quantile"),
    targets = paste(1:3, "wk ahead inc case"),
    locations = locations,
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() |>                   # After running this line: 17 models
    dplyr::filter(
      !(model %in% models_to_drop)) |>    # After running this line: 12 models 
    group_by(model, reference_date) |>
    summarize(nlocs = length(unique(location_name))) |>
    ungroup() |>
    filter(nlocs == length(locations)) |>  # After running this line: 9 models (drops PandemicCentral-COVIDForest, JHU_UNC_GAS-StatMechPool, FRBSF_Wilson-Econometric)
    pull(model) |>
    unique()
}
