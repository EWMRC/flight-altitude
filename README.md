# flight-altitude
Repository accompanying the paper *Low migratory flight altitudes may explain increased collision risk for American Woodcock*, currently under consideration at Ornithological Applications.

### To run this code from start to finish:

- Import locations from Movebank using **importing_flight_locations.Rmd** (requires credentials to log into the Eastern Woodcock Migration Research Cooperative Movebank repository).

- Open **flight_locations.aprx** in ArcPro, and run the ModelBuilder tool saved in **flight_locations.atbx**. This tool transforms the height above ellipsoid measurements collected by the transmitter into height above terrain measurements. Note that this model runs slowly (takes 4-6 hours on an older PC).

- Run the flight altitude models in Stan. The primary model (used to determine true flight altitude for reupload to Movebank) is **lnorm_original.R**. Other models (**lnorm_sex.R**, **lnorm_season.R**, **lnorm_age.R**) are used to examine differences in flight altitude for certain subsets of the data. Note that these models run comparatively quickly (<5 minutes each).

- Run the scripts in the **statistics** folder to generate the parameter estimates and credible intervals included in the paper. The graphs can be generated using the scripts in the **graph_results** folder.

- Manually upload the estimates of true flight altitude (generated using **lnorm_original.R**) to Movebank through the web portal. The new data is stored in Movebank attributes as follows: 
  1. BAS confidence: Binary variable expressing whether a location has a >50% probability of being recorded during a migratory flight.
  2. BAS transition 1: Probability of a given location being recorded during a migratory flight.
  3. barometric height: Estimate of true height of a given location above ground level.