library(tidyverse)
library(sf)
library(suncalc)
library(here)
library(rstan)
library(shinystan)
library(truncnorm)

options(mc.cores = 4)

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

# hist(raw_data$height_above_terrain)
# summary(raw_data$height_above_terrain)

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

raw_data %>% 
  group_by(day_night) %>% 
  tally()

# possible flight location if a) the bird is migrating, b) the point is nocturnal, 
# c) the point demonstrates some movement between prior and subsequent points (1 km)
# this also implies that tracks cannot begin or end on a flight location. Given the small number of these in our
# dataset, I'm okay with that assumption for now

altitude_data <- raw_data %>% 
  mutate(HAT_index = if_else(point_state %in% c("Point state: Migratory (spring)", "Point state: Migratory (fall)") & day_night == "Night" & moving == TRUE, NA, 1))

altitude_data %>% 
  group_by(HAT_index) %>% 
  tally() #428 possible flight locations

# plot results
# altitude_data %>%
#   filter(!is.na(HAT_index)) %>%
#   pull(height_above_terrain) %>%
#   hist(main = "Presumed ground locations")
# 
# altitude_data %>%
#   filter(is.na(HAT_index)) %>%
#   pull(height_above_terrain) %>%
#   hist(main = "Possible flight locations")

# Scale locations between -1 and 1
altitude_data <- altitude_data %>% 
  mutate(hat_scaled = height_above_terrain/2183.475)

#splitting into two dataframes
known_ground_df <- altitude_data %>% 
  filter(HAT_index == 1)

unknown_df <- altitude_data %>% 
  filter(is.na(HAT_index))

# Recategorizing fall and spring locations as numeric
# fall 1, spring 2
unknown_df <- unknown_df %>% 
  mutate(season = as.numeric(as.factor(point_state)))

unknown_df_fall <- unknown_df %>% 
  filter(season == 1)

unknown_df_spring <- unknown_df %>% 
  filter(season == 2)

init <- function(){list(mu_bias = rnorm(1,0,0.2),
                        sigma_error = runif(1,0,0.2),
                        shape_fall = runif(1,3,5),
                        rate_fall = runif(1,5,10),
                        shape_spring = runif(1,3,5),
                        rate_spring = runif(1,5,10)
                        )}

model_compiled <- stan_model(here("bayesian_modeling", "stan", "gamma_season_stan.stan"))

fit <- sampling(model_compiled, data = list(n_obs_known = nrow(known_ground_df),
                                            HAT_known = known_ground_df$hat_scaled,
                                            n_obs_unknown_fall = nrow(unknown_df_fall),
                                            n_obs_unknown_spring = nrow(unknown_df_spring),
                                            HAT_unknown_fall = unknown_df_fall$hat_scaled,
                                            HAT_unknown_spring = unknown_df_spring$hat_scaled), 
                init = init,
                pars = c("mu_bias", "sigma_error", "shape_fall", "rate_fall", 
                         "shape_spring", "rate_spring", "sample_size_fall",
                         "sample_size_spring", "HAT_known_mean_gte", "HAT_known_sd_gte", 
                         "HAT_unknown_mean_fall_gte", "HAT_unknown_sd_fall_gte", 
                         "HAT_unknown_mean_spring_gte", "HAT_unknown_sd_spring_gte", "p_flight_fall", 
                         "p_flight_spring"),
                iter = 15000, #keep down to 5000 for graphical ppc. Extra parameters: "HAT_known_ppc", "HAT_unknown_fall_ppc", "HAT_unknown_spring_ppc"
                chains = 4)

print(fit)

saveRDS(fit, file = here("bayesian_modeling", "stan", "gamma_season_stan.rds"))

## additional variables for graphical ppc
# pp_known <- known_ground_df$hat_scaled
# pp_unknown_fall <- unknown_df_fall$hat_scaled
# pp_unknown_spring <- unknown_df_spring$hat_scaled

launch_shinystan(fit) #diagnostics
