library(tidyverse)
library(sf)
library(suncalc)
library(here)
library(jagsUI)

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

# if this is an overwater location, and it fits all of the prior assumption, we designate it as a known flight location

height_cutoff <- altitude_data %>% 
  filter(is.na(HAT_index)) %>% 
  pull(height_above_terrain) %>% 
  quantile(0.80)

altitude_data <- altitude_data %>%
  mutate(HAT_index = if_else(height_above_terrain >= height_cutoff & is.na(HAT_index), 2, HAT_index))

# altitude_data <- altitude_data %>% 
#   mutate(HAT_index = if_else(on_land == FALSE & is.na(HAT_index), 2, HAT_index))

altitude_data %>% 
  group_by(HAT_index) %>% 
  tally() # 43 "known" flight locations, 423 possible flight locations
  
# plot results
altitude_data %>%
  filter(!is.na(HAT_index)) %>%
  pull(height_above_terrain) %>%
  hist(main = "Presumed ground locations")

altitude_data %>%
  filter(is.na(HAT_index)) %>%
  pull(height_above_terrain) %>%
  hist(main = "Possible flight locations")

altitude_data %>%
  filter(HAT_index ==2) %>%
  pull(height_above_terrain) %>%
  hist(main = "Known flight locations")

inits <- function(){list(mu_bias=rnorm(1,0,1),
                         mu_flight=runif(1,0,500),
                         sigma_error=runif(1,0,100),
                         sigma_flight=runif(1,0,200))}

parameters <- c("mu_bias", "sigma_error", "mu_flight", "sigma_flight", "real_alt") #

jags_data <- list(HAT = altitude_data$height_above_terrain,
                  n_obs = nrow(altitude_data),
                  HAT_index = altitude_data$HAT_index)
# running jags
nc <- 3 # number of chains
ni <- 5000 # number of iterations
nb <- 2000 # burnin
nt <- 1 # thin rate (keeps every 5th iteration)

m_test <- jags(data=jags_data, inits=inits, parameters.to.save = parameters, 
               model.file=here("bayesian_modeling", "height_cutoff_model.jags"), n.chains=nc, n.iter=ni, n.burnin=nb,
               parallel=T)

print(m_test)

traceplot(m_test)
