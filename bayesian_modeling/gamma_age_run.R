library(tidyverse)
library(sf)
library(suncalc)
library(here)
library(jagsUI)
library(truncnorm)
library(readxl)
# covariates <- read.csv(here("intermediate_files", "imported_movebank_data.csv"))

raw_data <- read.csv(here("intermediate_files", "raw_elevation_values.csv")) #%>% 
# st_drop_geometry() %>% 
# arrange(Field1) %>% 
# dplyr::select(t_hae_m)

# raw_data <- covariates %>% 
#   bind_cols(raw_data)

#calculate height above terrain and begin filtering to 3D fixes
raw_data <- raw_data %>% 
  mutate(height_above_terrain = height_above_wgs84_1 - Terrain) %>% 
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
  mutate(HAT_index = if_else(point_state %in% c("Point state: Migratory (spring)", "Point state: Migratory (fall)") & day_night == "Night" & moving == TRUE, NA, 1)) #%>% 
  #dplyr::select(-age) #take out the old age category, use the one from the capture sheet

altitude_data %>% 
  group_by(HAT_index) %>% 
  tally() #428 possible flight locations

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

age_vect <- altitude_data$current_age %>% 
  as.factor() %>% 
  as.numeric() #Adults are 1, Juveniles are 2

# Scale locations between -1 and 1
altitude_data <- altitude_data %>% 
  mutate(hat_scaled = height_above_terrain/2218.084)


inits <- function(){list(mu_bias = rnorm(1,0,1),
                         sigma_error = runif(1,0,1),
                         shape_flight = runif(2,0,5), #
                         rate_flight = runif(2,0,10))}

parameters <- c("mu_bias", "sigma_error", "shape_flight", "rate_flight") #

jags_data <- list(HAT = altitude_data$hat_scaled,
                  n_obs = nrow(altitude_data),
                  HAT_index = altitude_data$HAT_index,
                  age = age_vect)

# running jags
nc <- 4 # number of chains
ni <- 200000 # number of iterations
nb <- 10000 # burnin
nt <- 5 # thin rate (keeps every 5th iteration)

m_test <- jags(data=jags_data, inits=inits, parameters.to.save = parameters, 
               model.file=here("bayesian_modeling", "gamma_age_model.jags"), n.chains=nc, n.iter=ni, n.burnin=nb,
               parallel=T)

saveRDS(m_test, file = here("bayesian_modeling", "gamma_age_new_2.rds"))

print(m_test)

traceplot(m_test)
