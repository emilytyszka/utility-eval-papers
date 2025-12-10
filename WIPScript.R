library(targets)
library(tarchetypes)
library(tibble)
library(crew)
library(covidHubUtils)
library(alloscore)
library(distfromq)
library(tidyverse)

tar_load(forecast_data)
tar_load(truth_data)
#tar_load(`values`)

# run_alloscore_one_date troubleshooting
run_alloscore_one_date <- function(
    forecast_data,
    truth_data,
    one_reference_date,
    one_K = NULL) {
  require(tidyverse)
  require(alloscore)
  require(distfromq)
  
  ## vector of all models present in data
  models <- unique(forecast_data$model)
  
  ## for each model, compute allocation, return as df
  map(models,
      \(x) run_alloscore(forecast_data, truth_data, one_reference_date, one_model = x, one_K)) %>%
    list_rbind()
}
run_alloscore_one_date(forecast_data = forecast_data,
                       truth_data = truth_data,
                       one_reference_date = "2021-02-06")

# run_alloscore
run_alloscore <- function(
    forecast_data,
    truth_data,
    one_reference_date,
    one_model,
    one_K = NULL,
    slim = TRUE){
  require(tidyverse)
  require(alloscore)
  require(distfromq)
  ## process forecast data, adding distfromq output
  forecast_data_processed <- forecast_data |>
    ## forecast dates are different but reference dates are Mondays
    dplyr::filter(reference_date %in% as.Date(one_reference_date)) |>
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
      by = c("location", "target_end_date")) %>% filter(horizon==4)
  
  if (nrow(forecast_data_processed) > 0) {
    if (!is.null(one_K)) {
      Ks <- one_K
    } else {
      Ks <- make_K_grid(values = forecast_data_processed$value)
    }
    
    ## run alloscore
    ascores <- forecast_data_processed %>%
      alloscore(
        K = Ks,
        y = .[["value"]],
        target_names = "abbreviation",
        slim = slim
      )
    ascores <- ascores %>% dplyr::mutate(
      reference_date = one_reference_date,
      model = one_model,
      .before = 1
    )
  } else {
    ascores <- tibble(
      reference_date = one_reference_date,
      model = one_model,
      message = "no forecasts"
    )
  }
  return(ascores)
}



forecast_data_processed <- forecast_data |>
  ## forecast dates are different but reference dates are Mondays
  dplyr::filter(reference_date %in% as.Date("2021-01-30")) |>
  dplyr::select(-type) |>
  nest(ps = quantile, qs = value) |>
  relocate(ps, qs) |>
  mutate(
    ps = map(ps, deframe),
    qs = map(qs, deframe)
  ) |>
  dplyr::filter(model == "CEID-Walk") %>%
  dplyr::mutate(
    model = ifelse(model == "COVIDhub-4_week_ensemble", "COVIDhub-ensemble", model)
  ) |>
  add_pdqr_funs(dist = "distfromq", types = c("p", "q")) |>
  relocate(dist, F, Q) |>
  left_join(
    truth_data |> select(location, target_end_date, value),
    by = c("location", "target_end_date")) %>% filter(horizon==4)

run_alloscore(forecast_data = forecast_data,
              truth_data = truth_data,
              one_model = "CEID-Walk",
              one_reference_date = "2021-02-06",
              one_K = NULL, 
              slim=TRUE)

Ks <- make_K_grid(values = forecast_data_processed$value)
ascores <- forecast_data_processed %>% filter(model== "CEID-Walk") %>%
  alloscore(
    K = 150000,
    y = .[["value"]],
    target_names = "full_location_name",
    slim = FALSE
  )
view(ascores)

ascores <- ascores %>% dplyr::mutate(
  reference_date = one_reference_date,
  model = one_model,
  .before = 1)

reqd_locs <- hub_locations |>
  dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA")) | geo_type == "state" & (abbreviation %in% c("CA"))) |>
  dplyr::pull(fips)

dataloadtest <- load_forecasts(
  dates = values$forecast_dates,
  date_window_size = 6,
  models = "CEID-Walk",
  locations = reqd_locs,
  types = c("quantile"),
  targets = paste(1:6, "wk ahead inc case"),
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US"))
dataloadtest <- dataloadtest |>
  align_forecasts()


# Fix per capita
library(readxl)
library(dplyr)
library(tidyverse)
popsold <- readr::read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/state/totals/NST-EST2022-ALLDATA.csv") |>
  dplyr::select(full_location_name = NAME, POPESTIMATE2021) |>
  dplyr::inner_join(hub_locations, by = join_by(full_location_name))
data("hub_locations")
view(hub_locations)
pops <- read_excel("data/CA_DeptOfFinance_PopEstim_2021-2025.xlsx", sheet = "Table 1 County State", skip = 2) |> 
  drop_na(`1/1/2021`) |> 
  mutate(full_location_name = ifelse(County == "State Total", "California", 
                                      paste(`County`, "County, CA")),
         POPESTIMATE2021 = `1/1/2021`) |> select(full_location_name, POPESTIMATE2021) |>
  dplyr::inner_join(hub_locations, by = join_by(full_location_name))

# With multi-day, merge issue is back - do we have duplicate forecasts?
view(forecast_data)
library(tidyverse)
library(alloscore)
library(distfromq)

forecast_data_processed <- forecast_data |>
  ## forecast dates are different but reference dates are Mondays
  #dplyr::filter(reference_date %in% as.Date("2021-01-30")) |>
  dplyr::select(-type) |>
  nest(ps = quantile, qs = value) |>
  relocate(ps, qs) |>
  mutate(
    ps = map(ps, deframe),
    qs = map(qs, deframe)
  ) |>
  #dplyr::filter(model == "CEID-Walk") %>%
  dplyr::mutate(
    model = ifelse(model == "COVIDhub-4_week_ensemble", "COVIDhub-ensemble", model)
  ) |>
  add_pdqr_funs(dist = "distfromq", types = c("p", "q")) |>
  relocate(dist, F, Q) |>
  left_join(
    truth_data |> select(location, target_end_date, value),
    by = c("location", "target_end_date")) #%>% filter(horizon==4)
forecast_data_processed_dedup <- forecast_data_processed %>% distinct(model, horizon, location_name, target_end_date, .keep_all = TRUE)
