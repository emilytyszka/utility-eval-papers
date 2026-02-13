###############################################################################

# TESTING OUT FILTERING METHODS
# List "to drop" vs. list "to include" vs. no list at all

###############################################################################

models_to_drop <- c("COVIDhub-4_week_ensemble", "COVIDhub_CDC-ensemble",
                      "CU-nochange", "CU-scenario_low", "CU-scenario_mid")
  
  models_to_include <- c("CEID-Walk", "COVIDhub-4_week_ensemble", "COVIDhub-baseline", "CU-select", "FAIR-NRAR",  "IowaStateLW-STEM", "JHU_IDD-CovidSP", "JHUAPL-Bucky", "LANL-GrowthRate", "LNQ-ens1", "OneQuietNight-ML", "UChicagoCHATTOPADHYAY-UnIT")  
  
  ## load and filter forecasts
nofilt <- load_forecasts(
    dates = values$forecast_dates,
    date_window_size = 6,
    types = c("quantile"),
    targets = paste(1:4, "wk ahead inc case"),
    locations = reqd_locs,
    source = "zoltar",
    verbose = FALSE,
    as_of = NULL,
    hub = c("US")) |>
    align_forecasts() 
nofilt2 <- nofilt |>
    group_by(model, reference_date) |>
    dplyr::summarize(nlocs = length(unique(location_name))) |>
    ungroup() |> dplyr::filter(nlocs == length(reqd_locs)) |>
    group_by(model) |>
    mutate(ncomplete_weeks = n()) |>
    #filter(ncomplete_weeks >= ) |>
    pull(model) |>
    unique()

filt <- load_forecasts(
  dates = values$forecast_dates,
  date_window_size = 6,
  types = c("quantile"),
  targets = paste(1:4, "wk ahead inc case"),
  locations = reqd_locs,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")) |>
  align_forecasts() |>
  dplyr::filter(
    (model %in% models_to_include))
filt2 <- filt |>
  group_by(model, reference_date) |>
  dplyr::summarize(nlocs = length(unique(location_name))) |>
  ungroup() |> dplyr::filter(nlocs == length(reqd_locs)) |>
  group_by(model) |>
  mutate(ncomplete_weeks = n()) |>
  #filter(ncomplete_weeks >= ) |>
  pull(model) |>
  unique()
  ## load and filter forecasts

filtexcl <- load_forecasts(
  dates = values$forecast_dates,
  date_window_size = 6,
  types = c("quantile"),
  targets = paste(1:4, "wk ahead inc case"),
  locations = reqd_locs,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")) |>
  align_forecasts() |>
  dplyr::filter((!model %in% models_to_drop))
filt2excl <- filtexcl |>
  group_by(model, reference_date) |>
  dplyr::summarize(nlocs = length(unique(location_name))) |>
  ungroup() |> dplyr::filter(nlocs == length(reqd_locs)) |>
  group_by(model) |>
  mutate(ncomplete_weeks = n()) |>
  #filter(ncomplete_weeks >= ) |>
  pull(model) |>
  unique()
## load and filter forecasts

view(filt2)
view(filt2excl)
view(nofilt2)

FilterIncl <- as.data.frame(filt2) %>% mutate("Scheme"="Inclusion List")
colnames(FilterIncl)[1] <- "Models"
FilterExl <- as.data.frame(filt2excl) %>% mutate("Scheme"="Exclusion List")
colnames(FilterExl)[1] <- "Models"
FilterNone <- as.data.frame(nofilt2) %>% mutate("Scheme"="No Filter")
colnames(FilterNone)[1] <- "Models"

Summary <- rbind(FilterExl, FilterIncl, FilterNone) %>% group_by(Scheme) %>%
  summarise("Model Count" = n_distinct(Models))

### Confirms suspicion that method of filtering really matters here. For now, I think exclusion list makes the most sense.



###############################################################################

# TESTING HORIZON + COUNT
# Currently looking for models reporting 4 or more out of 6 weeks into the future for the given dates. 
# I think it makes more sense to find ones complete for the horizons of interest: weeks 1 and 3 or all of weeks 1-4.

###############################################################################
currentscheme <- load_forecasts(
  dates = values$forecast_dates,
  date_window_size = 6,
  types = c("quantile"),
  targets = paste(1:6, "wk ahead inc case"),
  locations = reqd_locs,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")) |>
  align_forecasts() |>
  dplyr::filter((!model %in% models_to_drop)) |>
  group_by(model, reference_date) |>
  dplyr::summarize(nlocs = length(unique(location_name))) |>
  ungroup() |> dplyr::filter(nlocs == length(reqd_locs)) |>
  group_by(model) |>
  mutate(ncomplete_weeks = n()) |>
  dplyr::filter(ncomplete_weeks >= 4) |>
  pull(model) |>
  unique()

fouroffour <- load_forecasts(
  dates = values$forecast_dates,
  date_window_size = 6,
  types = c("quantile"),
  targets = paste(1:4, "wk ahead inc case"),
  locations = reqd_locs,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")) |>
  align_forecasts() |>
  dplyr::filter((!model %in% models_to_drop)) |>
  group_by(model, reference_date) |>
  dplyr::summarize(nlocs = length(unique(location_name))) |>
  ungroup() |> dplyr::filter(nlocs == length(reqd_locs)) |>
  group_by(model) |>
  mutate(ncomplete_weeks = n()) |>
  dplyr::filter(ncomplete_weeks >= 4) |>
  pull(model) |>
  unique()

just1and3 <- load_forecasts(
  dates = values$forecast_dates,
  date_window_size = 6,
  types = c("quantile"),
  targets = c("1 wk ahead inc case", "3 wk ahead inc case"),
  locations = reqd_locs,
  source = "zoltar",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")) |>
  align_forecasts() |>
  dplyr::filter((!model %in% models_to_drop)) |>
  group_by(model, reference_date) |>
  dplyr::summarize(nlocs = length(unique(location_name))) |>
  ungroup() |> dplyr::filter(nlocs == length(reqd_locs)) |>
  group_by(model) |>
  mutate(ncomplete_weeks = n()) |>
  dplyr::filter(ncomplete_weeks >= 2) |>
  pull(model) |>
  unique()

view(currentscheme)
view(fouroffour)
view(just1and3)

### A scheme looking at completeness for just weeks 1 and 3 gives us an additional model. I will opt for that one. 