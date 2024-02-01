library(tidyverse)
library(sf)
library(suncalc)
library(here)
library(jagsUI)

covariates <- read.csv(here("intermediate_files", "imported_movebank_data.csv"))

raw_data <- st_read(here("intermediate_files", "raw_elevation_values.shp")) %>% 
  st_drop_geometry() %>% 
  dplyr::select(t_hae_m)

raw_data <- covariates %>% 
  bind_cols(raw_data)

raw_data <- raw_data %>% 
  mutate(height_above_terrain = height_above_wgs84 - t_hae_m) %>% 
  filter(fix == "3D")

hist(raw_data$height_above_terrain)
summary(raw_data$height_above_terrain)

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

# plot results
raw_data %>% 
  filter(day_night == "Day") %>% 
  pull(height_above_terrain) %>% 
  hist()

raw_data %>% 
  filter(day_night == "Night") %>% 
  pull(height_above_terrain) %>% 
  hist()

## running the model (starting with the basic, "will it run" model)
ground_data <- raw_data %>% 
  filter(day_night == "Day")

inits <- function(){list(mu_bias=rnorm(1,0,1),
                         sigma_error=runif(1,0,100))}

parameters <- c("mu_bias", "sigma_error") #

jags_data <- list(HAT = ground_data$height_above_terrain,
                  n_obs = nrow(ground_data))
# running jags
nc = 3 # number of chains
ni = 5000 # number of iterations
nb = 2000 # burnin
nt = 1 # thin rate (keeps every 5th iteration)

m_test <- jags(data=jags_data, inits=inits, parameters.to.save = parameters, 
               model.file=here("night_model.jags"), n.chains=nc, n.iter=ni, n.burnin=nb,
               parallel=F)

print(m_test)

traceplot(m_test)
