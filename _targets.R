# Created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(tibble)
library(crew)
library(covidHubUtils)
library(readxl)
library(dplyr)
library(conflicted)
library(tidyverse)
library(reshape2)
library(alloscore)
library(distfromq)
data("hub_locations")
conflicts_prefer(stats::filter)
conflicts_prefer(stats::lag)

# Set target options:
tar_option_set(
  controller = crew_controller_local(workers = 2),
  packages = c("tidyverse", "covidHubUtils", "distfromq", "alloscore", "gh"), # packages that your targets need to run
  imports = c("alloscore"),
  format = "rds" # default storage format
)

## for custom package install
# remotes::install_github("aaronger/alloscore")
# remotes::install_github("reichlab/distfromq")
# remotes::install_github("reichlab/covidHubUtils")

# Run the R scripts in the R/ folder:
tar_source(files = c("R/data-ingestion.R",
                     "R/run-alloscore.R",
                     "R/determine-model-eligibility.R",
                     "R/exponential-examples.R",
                     "R/percap.R",
                     "R/plot_functions.R",
                     "R/resource-constraints.R",
                     "R/run-alloscore-match-K.R"))

datestopull <- tibble(forecast_dates = as.character(seq.Date(as.Date("2021-11-06"), as.Date("2022-03-12"), by = "7 days"))) %>% drop_na()
datesfinal <- tibble(forecast_dates = as.character(seq.Date(as.Date("2021-12-18"), as.Date("2022-03-12"), by = "7 days"))) %>% drop_na()
timehorizon1wk <- 1
timehorizon3wk <- 3

## set of required locations: all counties in CA
reqd_locs <- hub_locations |>
  dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA") & location_name != "Los Angeles County")) |>
  dplyr::pull(fips)

# Lists of targets:
setup <- list(
  tar_target(
    name = eligible_models,
    command = determine_eligible_models(forecast_dates = datestopull$forecast_dates, locations = reqd_locs)),
  
  tar_target(
    name = forecast_data1wk_raw,
    command = get_forecast_data(forecast_dates = datestopull$forecast_dates, models = eligible_models, 
                                locations = reqd_locs, timehorizon=timehorizon1wk,
                                dates_of_interest = datesfinal$forecast_dates)),
  
   tar_target(
     name = forecast_data3wk_raw,
     command = get_forecast_data(forecast_dates = datestopull$forecast_dates, models = eligible_models, 
                                 locations = reqd_locs, timehorizon=timehorizon3wk,
                                 dates_of_interest = datesfinal$forecast_dates)),

   tar_target(
     name = forecast_data1wk,
     command = generate_ensemble(forecast_data1wk_raw)),

   tar_target(
     name = forecast_data3wk,
     command = generate_ensemble(forecast_data3wk_raw)),

   tar_target(
     name = truth_data,
     command = get_truth_data()),

   tar_target(
     name = score_data1wk,
     command = get_forecast_scores(forecast_data1wk, truth_data)),

   tar_target(
    name = score_data3wk,
    command = get_forecast_scores(forecast_data3wk, truth_data)),

   tar_target(
    name = resource_constraint_grid,
    command = make_resource_constraint_grid(calculate_resource_constraints(truth_data))),

   tar_target(
    name = alloscores1wk,
    command = print(run_and_assemble_alloscores_match_k(forecast_data1wk,
                                               truth_data,
                                               reference_dates = resource_constraint_grid$target_end_date, #this used to use the `values` ref dates but dates were misaligned!
                                               Kgrid = resource_constraint_grid))),
  tar_target(
    name = alloscores3wk,
    command = print(run_and_assemble_alloscores_match_k(forecast_data3wk,
                                                        truth_data,
                                                        reference_dates = resource_constraint_grid$target_end_date, #this used to use the `values` ref dates but dates were misaligned!
                                                        Kgrid = resource_constraint_grid))),

  tar_target(
    name = pops22,
    command = readxl::read_excel("data/CA_DeptOfFinance_PopEstim_2021-2025.xlsx", sheet = "Table 1 County State", skip = 2) |>
       drop_na(`1/1/2021`) |>
       mutate(full_location_name = ifelse(County == "State Total", "California",
                                        paste(`County`, "County, CA")),
            POPESTIMATE2021 = `1/1/2021`) |> select(full_location_name, POPESTIMATE2021) |>
       dplyr::inner_join(hub_locations, by = join_by(full_location_name)))
)

 make_percap1 <- tar_target(
   name = percap1,
   command = score_per_capita_allocation(dat = alloscores1wk, pops = pops22, Kgrid = resource_constraint_grid)
 )

 make_percap3 <- tar_target(
   name = percap3,
   command = score_per_capita_allocation(dat = alloscores3wk, pops = pops22, Kgrid = resource_constraint_grid)
 )


list(setup, make_percap1, make_percap3)



