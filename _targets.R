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
                     "R/plot_functions.R"))

values <- tibble(forecast_dates = c(as.character(seq.Date(as.Date("2020-11-21"), as.Date("2020-12-19"), by = "7 days")), as.character(seq.Date(as.Date("2021-12-25"), as.Date("2022-01-22"), by = "7 days"))))
timehorizon1 <- 1
timehorizon2 <- 2
timehorizon3 <- 3
timehorizon4 <- 4


## create a group of alloscore targets
##values <- tidyr::expand_grid(models = mkeep, forecast_dates = forecast_dates)

## set of required locations: all states + DC
reqd_locs <- hub_locations |>
  dplyr::filter(geo_type == "county" & (abbreviation %in% c("CA")) | geo_type == "state" & (abbreviation %in% c("CA"))) |>
  dplyr::pull(fips)

# Lists of targets:
setup <- list(
  tar_target(
    name = eligible_models,
    command = determine_eligible_models(values$forecast_dates, locations = reqd_locs)
  ),
  tar_target(
    name = forecast_data1,
    command = get_forecast_data(values$forecast_dates, models = eligible_models, locations = reqd_locs, timehorizon=timehorizon1)
  ),
  tar_target(
    name = forecast_data2,
    command = get_forecast_data(values$forecast_dates, models = eligible_models, locations = reqd_locs, timehorizon=timehorizon2)
  ),
  tar_target(
    name = forecast_data3,
    command = get_forecast_data(values$forecast_dates, models = eligible_models, locations = reqd_locs, timehorizon=timehorizon3)
  ),
  tar_target(
    name = forecast_data4,
    command = get_forecast_data(values$forecast_dates, models = eligible_models, locations = reqd_locs, timehorizon=timehorizon4)
     ),
   tar_target(
     name = truth_data,
     command = get_truth_data()
   ),
   tar_target(
     name = score_data1,
     command = get_forecast_scores(forecast_data1, truth_data)
   ),
  tar_target(
    name = score_data2,
    command = get_forecast_scores(forecast_data2, truth_data)
  ),
  tar_target(
    name = score_data3,
    command = get_forecast_scores(forecast_data3, truth_data)
  ),
  tar_target(
    name = score_data4,
    command = get_forecast_scores(forecast_data4, truth_data)
    ) ,
 tar_target(
   name = exponential_example,
   command = make_exponential_example_figure()
 ),
 tar_target(
   name = Kat100k_alloscores1,
   command = print(run_and_assemble_alloscores(forecast_data1,
                                         truth_data,
                                         reference_dates = values$forecast_dates[1:9], #it's erroring on 10th date - not sure why
                                         one_K = 100000))
 ),
 tar_target(
   name = Kat100k_alloscores2,
   command = print(run_and_assemble_alloscores(forecast_data2,
                                         truth_data,
                                         reference_dates = values$forecast_dates[1:9],
                                         one_K = 100000))
 ),
   tar_target(
     name = Kat100k_alloscores3,
     command = print(run_and_assemble_alloscores(forecast_data3,
                                           truth_data,
                                           reference_dates = values$forecast_dates[1:9],
                                           one_K = 100000))
   ),
     tar_target(
       name = Kat100k_alloscores4,
       command = print(run_and_assemble_alloscores(forecast_data4,
                                             truth_data,
                                             reference_dates = values$forecast_dates[1:9],
                                             one_K = 100000))
     ),
 tar_target(
   name = Kat400k_alloscores1,
   command = print(run_and_assemble_alloscores(forecast_data1,
                                               truth_data,
                                               reference_dates = values$forecast_dates[1:9], #it's erroring on 10th date - not sure why
                                               one_K = 400000))
 ),
 tar_target(
   name = Kat400k_alloscores2,
   command = print(run_and_assemble_alloscores(forecast_data2,
                                               truth_data,
                                               reference_dates = values$forecast_dates[1:9],
                                               one_K = 400000))
 ),
 tar_target(
   name = Kat400k_alloscores3,
   command = print(run_and_assemble_alloscores(forecast_data3,
                                               truth_data,
                                               reference_dates = values$forecast_dates[1:9],
                                               one_K = 400000))
 ),
 tar_target(
   name = Kat400k_alloscores4,
   command = print(run_and_assemble_alloscores(forecast_data4,
                                               truth_data,
                                               reference_dates = values$forecast_dates[1:9],
                                               one_K = 400000))
 ),
 tar_target(
   name = pops22,
   command = readxl::read_excel("data/CA_DeptOfFinance_PopEstim_2021-2025.xlsx", sheet = "Table 1 County State", skip = 2) |>
     drop_na(`1/1/2021`) |>
     mutate(full_location_name = ifelse(County == "State Total", "California",
                                        paste(`County`, "County, CA")),
            POPESTIMATE2021 = `1/1/2021`) |> select(full_location_name, POPESTIMATE2021) |>
     dplyr::inner_join(hub_locations, by = join_by(full_location_name))
 )
 )

## from Ben's "overk" analysis
# tar_map(
#   values=values,
#   tar_target(alloscore_overk, run_alloscore_overk(forecast_data, truth_data, forecast_dates))
# )

mapped1 <- tar_map(
  unlist = FALSE,
  values = values,
  tar_target(
    alloscore1,
    run_alloscore_one_date(forecast_data1, truth_data, forecast_dates)
    ))
mapped2 <- tar_map(
  unlist = FALSE,
  values = values,
  tar_target(
    alloscore2,
    run_alloscore_one_date(forecast_data2, truth_data, forecast_dates)
  ))
mapped3 <- tar_map(
  unlist = FALSE,
  values = values,
  tar_target(
    alloscore3,
    run_alloscore_one_date(forecast_data3, truth_data, forecast_dates)
  ))
mapped4 <- tar_map(
  unlist = FALSE,
  values = values,
  tar_target(
    alloscore4,
    run_alloscore_one_date(forecast_data4, truth_data, forecast_dates)
  ))

combined1 <- tar_combine(
  name = all_alloscore_data1,
  mapped1[["alloscore1"]],
  command = assemble_alloscores(dplyr::bind_rows(!!!.x))
)
combined2 <- tar_combine(
  name = all_alloscore_data2,
  mapped2[["alloscore2"]],
  command = assemble_alloscores(dplyr::bind_rows(!!!.x))
)
combined3 <- tar_combine(
  name = all_alloscore_data3,
  mapped3[["alloscore3"]],
  command = assemble_alloscores(dplyr::bind_rows(!!!.x))
)
combined4 <- tar_combine(
  name = all_alloscore_data4,
  mapped4[["alloscore4"]],
  command = assemble_alloscores(dplyr::bind_rows(!!!.x))
)

 # tar_target(
 #   name = figure_K_v_alloscore,
 #   command = plot_K_v_alloscore(alloscore_df),
 #   format = "file" )

 make_percap1_100 <- tar_target(
   name = percap1_100,
   command = score_per_capita_allocation(dat = Kat100k_alloscores1, pops = pops22)
 )
 make_percap2_100 <- tar_target(
   name = percap2_100,
   command = score_per_capita_allocation(dat = Kat100k_alloscores2, pops = pops22)
 )
 make_percap3_100 <- tar_target(
   name = percap3_100,
   command = score_per_capita_allocation(dat = Kat100k_alloscores3, pops = pops22)
 )
 make_percap4_100 <- tar_target(
   name = percap4_100,
   command = score_per_capita_allocation(dat = Kat100k_alloscores4, pops = pops22)
 )
 make_percap1_400 <- tar_target(
   name = percap1_400,
   command = score_per_capita_allocation(dat = Kat400k_alloscores1, pops = pops22)
 )
 make_percap2_400 <- tar_target(
   name = percap2_400,
   command = score_per_capita_allocation(dat = Kat400k_alloscores2, pops = pops22)
 )
 make_percap3_400 <- tar_target(
   name = percap3_400,
   command = score_per_capita_allocation(dat = Kat400k_alloscores3, pops = pops22)
 )
 make_percap4_400 <- tar_target(
   name = percap4_400,
   command = score_per_capita_allocation(dat = Kat400k_alloscores4, pops = pops22)
 )

list(setup, mapped1, mapped2, mapped3, mapped4, combined1, combined2, combined3, combined4, make_percap1_100, make_percap2_100, make_percap3_100, make_percap4_100, make_percap1_400, make_percap2_400, make_percap3_400, make_percap4_400)



