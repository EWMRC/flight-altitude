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
  mutate(HAT_index = if_else(point_state %in% c("Point state: Migratory (spring)", "Point state: Migratory (fall)") & day_night == "Night" & moving == TRUE, NA, 1)) %>% 
  filter(sex != "") #exclude 187 locations from individuals with unknown sex

altitude_data %>% 
  group_by(HAT_index) %>% 
  tally() #428 possible flight locations

#classify sex
altitude_data$sex_num <- altitude_data$sex %>% 
  as.factor() %>% 
  as.numeric() #female 1, male 2

# Scale locations between -1 and 1
altitude_data <- altitude_data %>% 
  mutate(hat_scaled = height_above_terrain/2183.475)

#splitting into two dataframes
known_ground_df <- altitude_data %>% 
  filter(HAT_index == 1)

unknown_df <- altitude_data %>% 
  filter(is.na(HAT_index))

unknown_df_female <- unknown_df %>% 
  filter(sex_num == 1)

unknown_df_male <- unknown_df %>% 
  filter(sex_num == 2)

init <- function(){list(mu_bias = rnorm(1,0,0.2),
                        sigma_error = runif(1,0,0.2),
                        shape_male = runif(1,3,5),
                        rate_male = runif(1,5,10),
                        shape_female = runif(1,3,5),
                        rate_female = runif(1,5,10))}

model_compiled <- stan_model(here("bayesian_modeling", "stan", "gamma_sex_stan.stan"))

fit <- sampling(model_compiled, data = list(n_obs_known = nrow(known_ground_df),
                                            HAT_known = known_ground_df$hat_scaled,
                                            n_obs_unknown_male = nrow(unknown_df_male),
                                            HAT_unknown_male = unknown_df_male$hat_scaled,
                                            n_obs_unknown_female = nrow(unknown_df_female),
                                            HAT_unknown_female = unknown_df_female$hat_scaled
                                            ), 
                init = init,
                pars = c("mu_bias", "sigma_error", "shape_male", "rate_male", 
                         "shape_female", "rate_female", "sample_size_male",
                         "sample_size_female", "HAT_known_mean_gte", "HAT_known_sd_gte", "HAT_unknown_mean_male_gte", "HAT_unknown_sd_male_gte", 
                         "HAT_unknown_mean_female_gte", "HAT_unknown_sd_female_gte", "p_flight_male", 
                         "p_flight_female"),
                iter = 15000, #keep down to 5000 for graphical ppc. Extra parameters: "HAT_known_ppc", "HAT_unknown_male_ppc", "HAT_unknown_female_ppc"
                chains = 4)

print(fit)

saveRDS(fit, file = here("bayesian_modeling", "stan", "gamma_sex_stan.rds"))

## additional variables for graphical ppc
# pp_known <- known_ground_df$hat_scaled
# pp_unknown_male <- unknown_df_male$hat_scaled
# pp_unknown_female <- unknown_df_female$hat_scaled

launch_shinystan(fit) #diagnostics
