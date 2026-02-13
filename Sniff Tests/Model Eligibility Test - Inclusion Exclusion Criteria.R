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