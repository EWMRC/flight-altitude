library(tidyverse)
library(sf)
library(suncalc)
library(here)
library(rstan)
library(shinystan)
library(truncnorm)

covariates <- read.csv(here("intermediate_files", "imported_movebank_data.csv"))

raw_data <- st_read(here("intermediate_files", "raw_elevation_values.shp")) %>% 
  st_drop_geometry() %>% 
  arrange(Field1) %>% 
  dplyr::select(t_hae_m)

raw_data <- covariates %>% 
  bind_cols(raw_data)

#calculate height above terrain and begin filtering to 3D fixes
raw_data <- raw_data %>% 
  mutate(height_above_terrain = height_above_wgs84 - t_hae_m) %>% 
  filter(fix == "3D") %>% 
  filter(point_state != "") #filter out empty point states

# determine whether each is a day or a night location
raw_data <- raw_data %>% 
  mutate(time_lubr = ymd_hms(time)) %>% 
  filter(!is.na(time_lubr))#discarding 118

raw_data %>%
  dplyr::transmute(date = as.Date(time_lubr), lat = lat, lon = lon) %>%
  getSunlightTimes(data = .) %>%
  pull(sunrise) -> 
  raw_data$sunrise

raw_data %>%
  dplyr::transmute(date = as.Date(time_lubr), lat = lat, lon = lon) %>%
  getSunlightTimes(data = .) %>%
  pull(sunset) -> 
  raw_data$sunset

raw_data %>%
  mutate(day_night = if_else(time_lubr > sunrise & time_lubr < sunset, "Day", "Night")) ->
  raw_data

altitude_data <- raw_data %>% 
  mutate(known_ground_location = if_else(day_night == "Day", TRUE, FALSE)) %>% 
  mutate(possible_flight_location = if_else(point_state %in% c("Point state: Migratory (spring)", "Point state: Migratory (fall)") & day_night == "Night" & moving == TRUE, TRUE, FALSE)) %>% 
  mutate(age = case_match(age,
                    "ASY" ~ "After Second Year",
                    "After second year" ~ "After Second Year",
                    "SY" ~ "Second Year",
                    "Second year" ~ "Second Year",
                    "HY" ~ "Hatch Year",
                    "Hatch year" ~ "Hatch Year",
                    "AHY" ~ "After Hatch Year",
                    "After hatch year" ~ "After Hatch Year",
                    "" ~ "Unknown",
                    NA ~ "Unknown", 
                    "Age Unknown" ~ "Unknown",
                    "Age unknown" ~ "Unknown",
                    .default = age)) %>% 
  rename(migratory_state = point_state) %>% 
  filter(migratory_state != "Point state: Unknown- bugged frequent schedule") %>% 
  dplyr::select("ID", "time", "lon", "lat", "sex", "age", "height_above_wgs84", "height_above_terrain", "migratory_state", "moving", "day_night", "known_ground_location", "possible_flight_location")

#unique(altitude_data$migratory_state)
#dim(altitude_data)

write_csv(altitude_data, here("dryad", "amwo_flight_altitudes.csv"))
