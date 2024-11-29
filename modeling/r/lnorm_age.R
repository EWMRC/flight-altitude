library(tidyverse)
library(sf)
library(suncalc)
library(here)
library(rstan)
library(shinystan)
library(truncnorm)
library(readxl)

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
  mutate(probable_ground = if_else(day_night == "Day", 1, NA)) %>% 
  mutate(possible_flight = if_else(point_state %in% c("Point state: Migratory (spring)", "Point state: Migratory (fall)") & day_night == "Night" & moving == TRUE, 1, NA)) %>% 
  dplyr::select(-age) #take out the old age category, use the one from the capture sheet

altitude_data %>% 
  group_by(possible_flight) %>% 
  tally() #258, down from 428 possible flight locations

# doing the age calculations

capture_sheet <- read_excel(here("capture_sheet.xlsx"), 
                            col_types = c("text", "text", "date", 
                                          "numeric", "numeric", "text", "text",
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", 
                                          "text", "text")) %>% 
  transmute(ID = Movebank_ID,
            capture_date = Date,
            age = Age) %>% 
  filter(ID != "NA")

capture_sheet <- capture_sheet %>% 
  arrange(capture_date) %>% 
  distinct(ID, .keep_all = TRUE)

altitude_data <- altitude_data %>% 
  left_join(capture_sheet, by = "ID") %>% 
  filter(!is.na(capture_date)) #should mostly be individuals without capture data

altitude_data$standard_capture_date <- mdy(paste0(month(altitude_data$capture_date), "/", day(altitude_data$capture_date), "/2020"))

altitude_data <- altitude_data %>% 
  mutate(current_age = if_else(standard_capture_date >= mdy("7/1/2020"), 
                               case_match(age,
                                          "AHY" ~ "Adult",
                                          "HY" ~ "Juvenile",
                                          "NA" ~ "Unknown", 
                                          .default = "Unknown"),
                               case_match(age,
                                          "ASY" ~ "Adult",
                                          "SY" ~ "Juvenile",
                                          "AHY" ~ "Unknown",
                                          "NA" ~ "Unknown", 
                                          .default = "Unknown")
  ))

altitude_data <- altitude_data %>% 
  mutate(banding_year_capture = year(capture_date - days(182)),
         banding_year_obs = year(ymd_hms(time) - days(182))
  )

altitude_data <- altitude_data %>% 
  mutate(current_age = if_else(banding_year_capture != banding_year_obs, "Adult", current_age)) %>% 
  filter(current_age != "Unknown")

altitude_data$age_num <- altitude_data$current_age %>% 
  as.factor() %>% 
  as.numeric() #Adults are 1, Juveniles are 2

# Scale locations between -1 and 1
altitude_data <- altitude_data %>% 
  mutate(hat_scaled = height_above_terrain/2183.475)

#splitting into two dataframes
known_ground_df <- altitude_data %>% 
  filter(probable_ground == 1)

unknown_df <- altitude_data %>% 
  filter(possible_flight == 1)

unknown_df_adult <- unknown_df %>% 
  filter(age_num == 1)

unknown_df_juv <- unknown_df %>% 
  filter(age_num == 2)

init <- function(){list(nu_obs = rgamma(1,2,0.1),
                        mu_obs = rnorm(1,0,0.2),
                        sigma_obs = runif(1,0,0.2),
                        mu_alt_adult = runif(1,-1,1),
                        sigma_alt_adult = runif(1,0,1),
                        mu_alt_juv = runif(1,-1,1),
                        sigma_alt_juv = runif(1,0,1),
                        flight_prior_adult = rbeta(1,2,2),
                        flight_prior_juv = rbeta(1,2,2))}

model_compiled <- stan_model(here("modeling", "stan", "lnorm_age.stan"))

fit <- sampling(model_compiled, data = list(n_obs_known = nrow(known_ground_df),
                                            HAT_known = known_ground_df$hat_scaled,
                                            n_obs_unknown_adult = nrow(unknown_df_adult),
                                            n_obs_unknown_juv = nrow(unknown_df_juv),
                                            HAT_unknown_adult = unknown_df_adult$hat_scaled,
                                            HAT_unknown_juv = unknown_df_juv$hat_scaled), 
                init = init,
                pars = c("nu_obs", "mu_obs", "sigma_obs", "flight_prior_adult", "flight_prior_juv", 
                         "mu_alt_adult", "mu_alt_juv", "sigma_alt_adult", "sigma_alt_juv",
                         # "HAT_known_ppc", "HAT_unknown_adult_ppc", "HAT_unknown_juv_ppc",
                         # "HAT_unknown_mean_adult_gte", "HAT_unknown_sd_adult_gte",
                         # "HAT_unknown_mean_juv_gte", "HAT_unknown_sd_juv_gte",
                         "p_flight_adult", "p_flight_juv"),
                iter = 15000, #5000 for graphical ppc
                chains = 4, 
                init_r = 0,
                seed = 8)

print(fit)

saveRDS(fit, file = here("modeling", "results", "lnorm_age.rds"))

## additional variables for graphical ppc
pp_known <- known_ground_df$hat_scaled
pp_unknown_adult <- unknown_df_adult$hat_scaled
pp_unknown_juv <- unknown_df_juv$hat_scaled

launch_shinystan(fit) #diagnostics
