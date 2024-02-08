flight_altitudes <- altitude_data %>% 
  filter(HAT_index == 2) %>% 
  pull(height_above_terrain)

max(flight_altitudes) # 2183.475

scaled_flight_altitudes <- flight_altitudes/2183.475
MASS::fitdistr(flight_altitudes, "gamma")
q <- MASS::fitdistr(scaled_flight_altitudes, "gamma")
q

par(mfrow=c(2,1))
rgamma(10000, shape = 0.491, rate = 4.930) %>% 
  density() %>% 
  plot(xlim = c(0,1))

known_flight_df$HAT %>% 
  density() %>% 
  plot(xlim = c(0,1))
