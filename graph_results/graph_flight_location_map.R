library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(here)

res <- readRDS(here("intermediate_results.rds")) %>% 
  mutate(flight_binary = if_else(p_flight >= 0.5, 1, 0))

#count number of flight locations per bird: max is 6
locs_per_bird <- res %>% 
  filter(flight_binary == 1) %>% 
  group_by(ID) %>% 
  tally()

res_sf <- res %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  mutate(flight_binary = factor(flight_binary, levels = c(0,1), labels = c("Possible flight locations", "Flight locations")))

basemap <- rast("/Users/liamberigan/Library/CloudStorage/OneDrive-KansasStateUniversity/MaineAnalysis/Data/basemaps/esri_lightgrey/esri_lightgrey_36_48_4326.tif")
baselines <- st_read("/Users/liamberigan/Library/CloudStorage/OneDrive-KansasStateUniversity/MaineAnalysis/Data/Govt_boundaries/states_provinces_5070.shp") %>% 
  st_cast("MULTILINESTRING")

map_plot <- ggplot() +
  geom_spatraster_rgb(data = basemap) +
  geom_sf(data = baselines, linewidth = 0.1, color = "grey42") +
  geom_sf(mapping = aes(color = flight_binary), 
          data = res_sf,
          size = 0.25) + 
  coord_sf(xlim = c(-110, -55), ylim = c(20, 60)) +
  guides(color="none") +
  scale_color_manual(values = c("blue", "red")) +
  theme_bw()

ggsave(plot = map_plot,
       filename = here("graph_results", "figures", "flight_location_map.png"),
       width = 7/1.2,
       height = 5/1.2)
