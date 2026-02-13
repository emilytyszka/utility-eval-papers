determine_eligible_models <- function(forecast_dates, locations){
  require(covidHubUtils)
  require(tidyverse)

  ## Drop several covidhub ensembles: COVIDhub-4_week_ensemble, COVIDhub_CDC-ensemble (keep trained and COVIDhub-ensemble)
  ## drop CU non-primary models: CU-nochange, CU-scenario_low, CU-scenario_mid
  models_to_drop <- c("COVIDhub-4_week_ensemble", "COVIDhub_CDC-ensemble",
                      "CU-nochange", "CU-scenario_low", "CU-scenario_mid")

  models_to_include <- c("CEID-Walk", "COVIDhub-4_week_ensemble", "COVIDhub-baseline", "CU-select", "FAIR-NRAR",  "IowaStateLW-STEM", "JHU_IDD-CovidSP", "JHUAPL-Bucky", "LANL-GrowthRate", "LNQ-ens1", "OneQuietNight-ML", "UChicagoCHATTOPADHYAY-UnIT")  
 
  ## load and filter forecasts
  load_forecasts(
    dates = values$forecast_dates,
    date_window_size = 6,
    types = c("quantile"),
    targets = c("1 wk ahead inc case", "3 wk ahead inc case"), # See Model Eligibility Test to see how this was settled upon
    locations = reqd_locs,
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() |>
    dplyr::filter((!model %in% models_to_drop)) |> # See Model Eligibility Test to see how this was settled upon
    group_by(model, reference_date) |>
    dplyr::summarize(nlocs = length(unique(location_name))) |>
    ungroup() |> dplyr::filter(nlocs == length(reqd_locs)) |> # This was buggy - made more explicit
    group_by(model) |>
    mutate(ncomplete_weeks = n()) |>
    dplyr::filter(ncomplete_weeks >= 2) |> # See Model Eligibility Test to see how this was settled upon
    pull(model) |>
    unique()
}
