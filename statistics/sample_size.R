library(tidyverse)
library(rstan)
library(bayestestR)
library(here)

original_results <- readRDS(here("modeling", "results", "lnorm_original.rds"))

sample_size_original <- rstan::extract(original_results, "flight_prior")[[1]] * 258

# presumed # of flight locations
median(sample_size_original) # 144.0108

# using 95, but take note:
# "as the 89% level gives more stable results (Kruschke, 2014) and reminds us about the arbitrariness of such conventions (McElreath, 2018)."
hdi(sample_size_original, ci = 0.50) # 50% HDI: [138.55, 150.46]
hdi(sample_size_original, ci = 0.95) # 95% HDI: [127.04, 161.30]

## season
season_results <- readRDS(here("modeling", "results", "lnorm_season.rds"))

sample_size_season_fall <- rstan::extract(season_results, "flight_prior_fall")[[1]] * 118
sample_size_season_spring <- rstan::extract(season_results, "flight_prior_spring")[[1]] * 140

sample_size_season <- sample_size_season_fall + sample_size_season_spring

# total
median(sample_size_season) # 143.6666

hdi(sample_size_season, ci = 0.50) # 50% HDI: [137.34, 148.90]
hdi(sample_size_season, ci = 0.95) # 95% HDI: [127.00, 160.78]

# fall
median(sample_size_season_fall) # 76.43724

hdi(sample_size_season_fall, ci = 0.50) # 50% HDI: [72.32, 80.00]
hdi(sample_size_season_fall, ci = 0.95) # 95% HDI: [65.34, 87.54]

# spring
median(sample_size_season_spring) # 67.11886

hdi(sample_size_season_spring, ci = 0.50) # 50% HDI: [62.49, 71.19]
hdi(sample_size_season_spring, ci = 0.95) # 95% HDI: [54.32, 79.61]

## age
age_results <- readRDS(here("modeling", "results", "lnorm_age.rds"))

sample_size_age_adult <- rstan::extract(age_results, "flight_prior_adult")[[1]] * 98
sample_size_age_juv <- rstan::extract(age_results, "flight_prior_juv")[[1]] * 134

sample_size_age <- sample_size_age_adult + sample_size_age_juv

# total
median(sample_size_age) # 130.218

hdi(sample_size_age, ci = 0.50) # 50% HDI: [124.24, 135.20]
hdi(sample_size_age, ci = 0.95) # 95% HDI: [114.65, 146.16]

# adult
median(sample_size_age_adult) # 54.86732

hdi(sample_size_age_adult, ci = 0.50) # 50% HDI: [51.71, 58.74]
hdi(sample_size_age_adult, ci = 0.95) # 95% HDI: [44.84, 65.08]

# juv
median(sample_size_age_juv) # 75.36119

hdi(sample_size_age_juv, ci = 0.50) # 50% HDI: [71.27, 79.62]
hdi(sample_size_age_juv, ci = 0.95) # 95% HDI: [63.48, 87.73]

## sex
sex_results <- readRDS(here("modeling", "results", "lnorm_sex.rds"))

sample_size_sex_male <- rstan::extract(sex_results, "flight_prior_male")[[1]] * 121
sample_size_sex_female <- rstan::extract(sex_results, "flight_prior_female")[[1]] * 128

sample_size_sex <- sample_size_sex_male + sample_size_sex_female

# total
median(sample_size_sex) # 137.7585

hdi(sample_size_sex, ci = 0.50) # 50% HDI: [131.73, 143.21]
hdi(sample_size_sex, ci = 0.95) # 95% HDI: [120.88, 154.52]

# male
median(sample_size_sex_male) # 72.13711

hdi(sample_size_sex_male, ci = 0.50) # 50% HDI: [68.18, 75.99]
hdi(sample_size_sex_male, ci = 0.95) # 95% HDI: [60.92, 83.38]

# female
median(sample_size_sex_female) # 65.66282

hdi(sample_size_sex_female, ci = 0.50) # 50% HDI: [61.20, 69.71]
hdi(sample_size_sex_female, ci = 0.95) # 95% HDI: [53.58, 78.22]
