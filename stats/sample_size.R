library(tidyverse)
library(rstan)
library(bayestestR)
library(here)

original_results <- readRDS(here("bayesian_modeling", "gamma_original_stan.rds"))

sample_size_original <- rstan::extract(original_results, "sample_size")[[1]]

# presumed # of flight locations
median(sample_size_original) #144

# using 95, but take note:
# "as the 89% level gives more stable results (Kruschke, 2014) and reminds us about the arbitrariness of such conventions (McElreath, 2018)."
hdi(sample_size_original, ci = 0.50) #50% HDI: [139.72, 148.42]
hdi(sample_size_original, ci = 0.95) #95% HDI: [131.84, 156.88]

## season
season_results <- readRDS(here("bayesian_modeling", "gamma_season_stan.rds"))

sample_size_season_fall <- rstan::extract(season_results, "sample_size_fall")[[1]]
sample_size_season_spring <- rstan::extract(season_results, "sample_size_spring")[[1]]

sample_size_season <- sample_size_season_fall + sample_size_season_spring

# total
median(sample_size_season) #144.0806

hdi(sample_size_season, ci = 0.50) #50% HDI: [139.85, 148.16]
hdi(sample_size_season, ci = 0.95) #95% HDI: [132.42, 155.93]

# fall
median(sample_size_season_fall) #78.23179

hdi(sample_size_season_fall, ci = 0.50) #50% HDI: [75.11, 81.86]
hdi(sample_size_season_fall, ci = 0.95) #95% HDI: [68.67, 87.74]

# spring
median(sample_size_season_spring) #65.63828

hdi(sample_size_season_spring, ci = 0.50) #50% HDI: [62.92, 67.78]
hdi(sample_size_season_spring, ci = 0.95) #95% HDI: [58.85, 72.86]

## age
age_results <- readRDS(here("bayesian_modeling", "gamma_age_stan.rds"))

sample_size_age_adult <- rstan::extract(age_results, "sample_size_adult")[[1]]
sample_size_age_juv <- rstan::extract(age_results, "sample_size_juv")[[1]]

sample_size_age <- sample_size_age_adult + sample_size_age_juv

# total
median(sample_size_age) #138.7966

hdi(sample_size_age, ci = 0.50) #50% HDI: [134.42, 142.25]
hdi(sample_size_age, ci = 0.95) #95% HDI: [127.67, 150.25]

# adult
median(sample_size_age_adult) #58.18246

hdi(sample_size_age_adult, ci = 0.50) #50% HDI: [55.67, 60.39]
hdi(sample_size_age_adult, ci = 0.95) #95% HDI: [51.84, 65.37]

# juv
median(sample_size_age_juv) #80.42849

hdi(sample_size_age_juv, ci = 0.50) #50% HDI: [77.16, 83.53]
hdi(sample_size_age_juv, ci = 0.95) #95% HDI: [71.96, 90.14]

## sex
sex_results <- readRDS(here("bayesian_modeling", "gamma_sex_stan.rds"))

sample_size_sex_male <- rstan::extract(sex_results, "sample_size_male")[[1]]
sample_size_sex_female <- rstan::extract(sex_results, "sample_size_female")[[1]]

sample_size_sex <- sample_size_sex_male + sample_size_sex_female

# total
median(sample_size_sex) #139.4651

hdi(sample_size_sex, ci = 0.50) #50% HDI: [135.21, 143.42]
hdi(sample_size_sex, ci = 0.95) #95% HDI: [128.02, 151.57]

# male
median(sample_size_sex_male) #72.81798

hdi(sample_size_sex_male, ci = 0.50) #50% HDI: [69.15, 75.68]
hdi(sample_size_sex_male, ci = 0.95) #95% HDI: [64.32, 82.88]

# female
median(sample_size_sex_female) #66.45771

hdi(sample_size_sex_female, ci = 0.50) #50% HDI: [63.87, 68.83]
hdi(sample_size_sex_female, ci = 0.95) #95% HDI: [59.91, 74.19]
