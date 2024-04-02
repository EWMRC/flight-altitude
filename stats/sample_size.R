library(tidyverse)
library(jagsUI)
library(bayestestR)
library(here)

original_test <- readRDS(here("bayesian_modeling", "gamma_original_index.rds"))

original_index <- (original_test$sims.list$mean_HAT_index - 1) * 12558

# presumed # of flight locations
median(original_index) #144

# using 95, but take note:
# "as the 89% level gives more stable results (Kruschke, 2014) and reminds us about the arbitrariness of such conventions (McElreath, 2018)."
hdi(original_index, ci = 0.50) #140, 150
hdi(original_index, ci = 0.95) #131, 161

## season

season_test <- readRDS(file = here("bayesian_modeling", "gamma_season_index.rds"))

season_index <- (season_test$sims.list$mean_HAT_index - 1) * 12558

# presumed # of flight locations
median(season_index) #144

hdi(season_index, ci = 0.50) #140.00, 150.00
hdi(season_index, ci = 0.95) #129.00, 158.00

# season-specific
# fall
season_test$sims.list$season_1_ss %>% 
  median() #fall 78

season_test$sims.list$season_1_ss %>% 
  hdi(ci = 0.50) #76.00, 84.00

season_test$sims.list$season_1_ss %>% 
  hdi(ci = 0.95) #67.00, 90.00

#spring
season_test$sims.list$season_2_ss %>% 
  median() #spring 65

season_test$sims.list$season_2_ss %>% 
  hdi(ci = 0.50) #63.00, 68.00

season_test$sims.list$season_2_ss %>% 
  hdi(ci = 0.95) #58.00, 74.00

## age
age_test <- readRDS(file = here("bayesian_modeling", "gamma_age_index.rds"))

age_index <- (age_test$sims.list$mean_HAT_index - 1) * 12558

# presumed # of flight locations
median(age_index) #147.2766

# age-specific
#Adult
age_test$sims.list$age_1_ss %>% 
  median() # Adult 58

age_test$sims.list$age_1_ss %>% 
  hdi(ci = 0.50) # 50% HDI: [55.00, 60.00]

age_test$sims.list$age_1_ss %>% 
  hdi(ci = 0.95) # 95% HDI: [51.00, 67.00]

#Juvenile
age_test$sims.list$age_2_ss %>% 
  median() # 80

age_test$sims.list$age_2_ss %>% 
  hdi(ci = 0.50) # 50% HDI: [77.00, 84.00]

age_test$sims.list$age_2_ss %>% 
  hdi(ci = 0.95) # 95% HDI: [71.00, 93.00]
