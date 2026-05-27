## AMWO altitude code from Liam Berigan (used for Berigan et al. 2025) 
## adapted by Rachel Darling for AMWO night light flight analyses


## last updated: 12-May-2026


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
  dplyr::select(Terrain)

raw_data <- covariates %>% 
  bind_cols(raw_data)

raw_data <- rename(raw_data, t_hae_m = Terrain)

#calculate height above terrain and begin filtering to 3D fixes
raw_data <- raw_data %>% 
  mutate(height_above_terrain = height_above_wgs84 - t_hae_m) %>% 
  filter(fix == "3D")  ## 22396 observations w/o removing empty point states
# %>% filter(point_state != "") #filter out empty point states

point_state_empty <- filter(raw_data, point_state == "")

hist(raw_data$height_above_terrain)
summary(raw_data$height_above_terrain)

# determine whether each is a day or a night location
raw_data <- raw_data %>% 
  mutate(time_lubr = ymd_hms(time)) %>% 
  filter(!is.na(time_lubr))  # no observations discarded

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

# possible flight location if a) the bird is migrating or does not have a point state assigned, 
# b) the point is nocturnal, c) the point demonstrates some movement between prior and subsequent points (6.6 km)
# this also implies that tracks cannot begin or end on a flight location. Given the small number of these in our
# dataset, I'm okay with that assumption for now

altitude_data <- raw_data %>% 
  mutate(probable_ground = if_else(day_night == "Day", 1, NA)) %>% 
  mutate(possible_flight = if_else(point_state %in% c("Point state: Migratory (spring)", "Point state: Migratory (fall)", "") & day_night == "Night" & moving == TRUE, 1, NA)) %>% 
  mutate(possible_dayflight = if_else(point_state %in% c("Point state: Migratory (spring)", "Point state: Migratory (fall)", "") & day_night == "Day" & moving == TRUE, 1, NA))  # because occasionally woodcock are migrating during the day?

altitude_data %>% 
  group_by(possible_flight) %>% 
  tally() #258, down from 428 possible flight locations
          #370 as of May 2026

altitude_data %>% 
  group_by(possible_dayflight) %>% 
  tally()  # 337 possible day flight points; total of 337+370 = 707 possible flight points.

# plot results
altitude_data %>%
  filter(probable_ground == 1) %>%
  pull(height_above_terrain) %>%
  hist(main = "Presumed ground locations", breaks = 120
    # , xlim = c(0,100)
    )

negative_alt <- filter(altitude_data, height_above_terrain < -30) # when the ~30m error is accounted for, many less altitudes are negative, down from ~15,000 to 4314.

altitude_data %>%
  filter(possible_flight == 1) %>%
  pull(height_above_terrain) %>%
  hist(main = "Possible night flight locations")

altitude_data %>%
  filter(possible_dayflight == 1) %>%
  pull(height_above_terrain) %>%
  hist(main = "Possible flight locations")  # distribution is more negatively skewed than night flight locations.

# Scale locations between -1 and 1
altitude_data <- altitude_data %>% 
  mutate(hat_scaled = height_above_terrain/2183.475) # maximum altitude recorded

# splitting into three dataframes
known_ground_df <- altitude_data %>% 
  filter(probable_ground == 1)

unknown_df <- altitude_data %>% 
  filter(possible_flight == 1)

unknown_df_day <- altitude_data %>% 
  filter(possible_dayflight == 1)

# Just to get a rough estimate of the % of flight locations we should expect
threshold <- known_ground_df$height_above_terrain %>% quantile(0.95) #31.5227, 29.52786 in May 2026
unknown_df %>% 
  mutate(check = if_else(height_above_terrain > threshold,1,0)) %>% 
  pull(check) %>% 
  mean() #the true model should give us a proportion close to 0.5658915
          # 2026 update: 0.5972973


##### first run through, use Liam's code as written #####
init <- function(){list(nu_obs = rgamma(1,2,0.1),
                        mu_obs = rnorm(1,0,0.2),
                        sigma_obs = runif(1,0,0.2),
                        mu_alt = runif(1,-1,1),
                        sigma_alt = runif(1,0,1),
                        flight_prior = rbeta(1,2,2))}

model_compiled <- stan_model(here("modeling", "stan", "lnorm_original.stan"))

fit <- sampling(model_compiled, data = list(n_obs_known = nrow(known_ground_df),
                                            HAT_known = known_ground_df$hat_scaled,
                                            n_obs_unknown = nrow(unknown_df),
                                            HAT_unknown = unknown_df$hat_scaled), 
                init = init,
                pars = c("nu_obs", "mu_obs", "sigma_obs", "mu_alt", "sigma_alt", "flight_prior", "p_flight"), #, "HAT_known_ppc", "HAT_unknown_ppc", "HAT_known_mean_gte", "HAT_known_sd_gte"
                iter = 15000,
                #control = list(adapt_delta = 0.99), #, max_treedepth = 20
                chains = 4, 
                init_r = 0,
                seed = 8)

print(fit)
traceplot(fit, pars = c("nu_obs", "mu_obs", "sigma_obs", "mu_alt", "sigma_alt", "flight_prior"))

saveRDS(fit, file = here("modeling", "results", "lnorm_original_2026.rds"))

## additional variables for graphical ppc
pp_known <- known_ground_df$hat_scaled
pp_unknown <- unknown_df$hat_scaled

launch_shinystan(fit) #diagnostics

## extract altitude and p_flight data for each point
unknown_df_results <- unknown_df %>% 
  mutate(row = 1:nrow(unknown_df)) %>% 
  mutate(param = paste0("p_flight[", row, "]"))

unknown_df_results$p_flight <- unknown_df_results$param %>%
  map(function(x){
    median(rstan::extract(fit, x)[[1]])
  }) %>% 
  unlist()

saveRDS(unknown_df_results, here("intermediate_results_2026.rds"))

unknown_df_results <- unknown_df_results %>% 
  dplyr::select(event_id, height_above_terrain, 
    on_land,
    p_flight)

known_df_results <- known_ground_df %>% 
  dplyr::select(event_id, height_above_terrain, 
    on_land
    ) %>% 
  mutate(p_flight = 0)

movebank_upload <- bind_rows(known_df_results, unknown_df_results)

## checking that all overwater locations are flight locations
movebank_upload %>% # one is a "ground location"
  filter(on_land == FALSE) %>%
  View()

movebank_upload <- movebank_upload %>% # set all overwater locations to flight locations
  mutate(p_flight = if_else(on_land == FALSE, 1, p_flight))

## recategorize locations into binary flight/nonflight using a 0.5 threshold

movebank_upload <- movebank_upload %>% # 131 locations 2025; 226 in 2026! probably excluding low-flying points
  mutate(flight_binary = if_else(p_flight >= 0.5, 1, 0))

## trim
movebank_upload <- movebank_upload %>% 
  dplyr::select(-on_land)

## and save for upload to Movebank
saveRDS(movebank_upload, here("movebank_upload_2026.rds"))
write.csv(movebank_upload, file = here("movebank_upload_2026.csv"), row.names = FALSE)





##########################################################################################
##########################################################################################
## second run through, adapting Liam's code to include potential day flight locations ####

known_ground <- anti_join(known_ground_df, unknown_df_day)
unknown_day <- rbind(unknown_df, unknown_df_day)

unknown_day %>% 
  mutate(check = if_else(height_above_terrain > threshold,1,0)) %>% 
  pull(check) %>% 
  mean() #the true model should give us a proportion close to 0.5658915
# 2026 day flight update: 0.3422914

init <- function(){list(nu_obs = rgamma(1,2,0.1),
  mu_obs = rnorm(1,0,0.2),
  sigma_obs = runif(1,0,0.2),
  mu_alt = runif(1,-1,1),
  sigma_alt = runif(1,0,1),
  flight_prior = rbeta(1,2,2))}

model_compiled <- stan_model(here("modeling", "stan", "lnorm_original.stan"))

fit <- sampling(model_compiled, data = list(n_obs_known = nrow(known_ground),
  HAT_known = known_ground$hat_scaled,
  n_obs_unknown = nrow(unknown_day),
  HAT_unknown = unknown_day$hat_scaled), 
  init = init,
  pars = c("nu_obs", "mu_obs", "sigma_obs", "mu_alt", "sigma_alt", "flight_prior", "p_flight"), #, "HAT_known_ppc", "HAT_unknown_ppc", "HAT_known_mean_gte", "HAT_known_sd_gte"
  iter = 15000,
  #control = list(adapt_delta = 0.99), #, max_treedepth = 20
  chains = 4, 
  init_r = 0,
  seed = 8)

print(fit)
traceplot(fit, pars = c("nu_obs", "mu_obs", "sigma_obs", "mu_alt", "sigma_alt", "flight_prior"))

saveRDS(fit, file = here("modeling", "results", "lnorm_dayflight_2026.rds"))

## additional variables for graphical ppc
pp_known <- known_ground$hat_scaled
pp_unknown <- unknown_day$hat_scaled

launch_shinystan(fit) #diagnostics

## extract altitude and p_flight data for each point
unknown_day_results <- unknown_day %>% 
  mutate(row = 1:nrow(unknown_day)) %>% 
  mutate(param = paste0("p_flight[", row, "]"))

unknown_day_results$p_flight <- unknown_day_results$param %>%
  map(function(x){
    median(rstan::extract(fit, x)[[1]])
  }) %>% 
  unlist()

saveRDS(unknown_day_results, here("intermediate_dayflight_results_2026.rds"))





save.image(file = "C:\\Users\\rachel.darling\\Documents\\GitHub\\AMWO_research\\Dissertation\\Night_light\\flight_altitude_2026.RData")
