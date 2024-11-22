library(tidyverse)
library(rstan)
library(bayestestR)
library(here)

# probably going to want to redo this in light of the new flight prior

original_results <- readRDS(here("modeling", "results", "lnorm_original.rds"))

sample_size_original <- rstan::extract(original_results, "flight_prior")[[1]] * 258

# presumed # of flight locations
median(sample_size_original) # 158.7242

# using 95, but take note:
# "as the 89% level gives more stable results (Kruschke, 2014) and reminds us about the arbitrariness of such conventions (McElreath, 2018)."
hdi(sample_size_original, ci = 0.50) # 50% HDI: [145.46, 166.48]
hdi(sample_size_original, ci = 0.95) # 95% HDI: [129.95, 193.97]

## season
season_results <- readRDS(here("modeling", "results", "lnorm_season.rds"))

sample_size_season_fall <- rstan::extract(season_results, "flight_prior_fall")[[1]] * 118
sample_size_season_spring <- rstan::extract(season_results, "flight_prior_spring")[[1]] * 140

sample_size_season <- sample_size_season_fall + sample_size_season_spring

# total
median(sample_size_season) # 155.6482

hdi(sample_size_season, ci = 0.50) # 50% HDI: [144.77, 162.99]
hdi(sample_size_season, ci = 0.95) # 95% HDI: [130.77, 183.75]

# fall
median(sample_size_season_fall) # 86.03969

hdi(sample_size_season_fall, ci = 0.50) # 50% HDI: [78.17, 91.25]
hdi(sample_size_season_fall, ci = 0.95) # 95% HDI: [68.04, 105.51]

# spring
median(sample_size_season_spring) # 69.0335

hdi(sample_size_season_spring, ci = 0.50) # 50% HDI: [61.68, 73.69]
hdi(sample_size_season_spring, ci = 0.95) # 95% HDI: [52.26, 89.33]

## age
age_results <- readRDS(here("modeling", "results", "lnorm_age.rds"))

sample_size_age_adult <- rstan::extract(age_results, "flight_prior_adult")[[1]] * 98
sample_size_age_juv <- rstan::extract(age_results, "flight_prior_juv")[[1]] * 134

sample_size_age <- sample_size_age_adult + sample_size_age_juv

# total
median(sample_size_age) # 136.9268

hdi(sample_size_age, ci = 0.50) # 50% HDI: [128.75, 144.30]
hdi(sample_size_age, ci = 0.95) # 95% HDI: [114.32, 160.84]

# adult
median(sample_size_age_adult) # 57.04772

hdi(sample_size_age_adult, ci = 0.50) # 50% HDI: [52.23, 61.50]
hdi(sample_size_age_adult, ci = 0.95) # 95% HDI: [43.90, 71.66]

# juv
median(sample_size_age_juv) # 79.37433

hdi(sample_size_age_juv, ci = 0.50) # 50% HDI: [72.32, 84.62]
hdi(sample_size_age_juv, ci = 0.95) # 95% HDI: [61.81, 99.16]

## sex
sex_results <- readRDS(here("modeling", "results", "lnorm_sex.rds"))

sample_size_sex_male <- rstan::extract(sex_results, "flight_prior_male")[[1]] * 121
sample_size_sex_female <- rstan::extract(sex_results, "flight_prior_female")[[1]] * 128

sample_size_sex <- sample_size_sex_male + sample_size_sex_female

# total
median(sample_size_sex) # 149.4675

hdi(sample_size_sex, ci = 0.50) # 50% HDI: [139.25, 157.70]
hdi(sample_size_sex, ci = 0.95) # 95% HDI: [122.89, 177.63]

# male
median(sample_size_sex_male) # 76.80369

hdi(sample_size_sex_male, ci = 0.50) # 50% HDI: [70.01, 82.06]
hdi(sample_size_sex_male, ci = 0.95) # 95% HDI: [60.56, 96.54]

# female
median(sample_size_sex_female) # 71.85023

hdi(sample_size_sex_female, ci = 0.50) # 50% HDI: [63.93, 77.49]
hdi(sample_size_sex_female, ci = 0.95) # 95% HDI: [53.64, 94.68]
