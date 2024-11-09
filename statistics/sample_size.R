library(tidyverse)
library(rstan)
library(bayestestR)
library(here)

# probably going to want to redo this in light of the new flight prior

original_results <- readRDS(here("modeling", "results", "lnorm_original.rds"))

sample_size_original <- rstan::extract(original_results, "sample_size")[[1]]

# presumed # of flight locations
median(sample_size_original) #

# using 95, but take note:
# "as the 89% level gives more stable results (Kruschke, 2014) and reminds us about the arbitrariness of such conventions (McElreath, 2018)."
hdi(sample_size_original, ci = 0.50) #
hdi(sample_size_original, ci = 0.95) #

## season
season_results <- readRDS(here("modeling", "results", "lnorm_season.rds"))

sample_size_season_fall <- rstan::extract(season_results, "sample_size_fall")[[1]]
sample_size_season_spring <- rstan::extract(season_results, "sample_size_spring")[[1]]

sample_size_season <- sample_size_season_fall + sample_size_season_spring

# total
median(sample_size_season) #

hdi(sample_size_season, ci = 0.50) #
hdi(sample_size_season, ci = 0.95) #

# fall
median(sample_size_season_fall) #

hdi(sample_size_season_fall, ci = 0.50) #
hdi(sample_size_season_fall, ci = 0.95) #

# spring
median(sample_size_season_spring) #

hdi(sample_size_season_spring, ci = 0.50) #
hdi(sample_size_season_spring, ci = 0.95) #

## age
age_results <- readRDS(here("modeling", "results", "lnorm_age.rds"))

sample_size_age_adult <- rstan::extract(age_results, "sample_size_adult")[[1]]
sample_size_age_juv <- rstan::extract(age_results, "sample_size_juv")[[1]]

sample_size_age <- sample_size_age_adult + sample_size_age_juv

# total
median(sample_size_age) #

hdi(sample_size_age, ci = 0.50) #
hdi(sample_size_age, ci = 0.95) #

# adult
median(sample_size_age_adult) #

hdi(sample_size_age_adult, ci = 0.50) #
hdi(sample_size_age_adult, ci = 0.95) #

# juv
median(sample_size_age_juv) #

hdi(sample_size_age_juv, ci = 0.50) #
hdi(sample_size_age_juv, ci = 0.95) #

## sex
sex_results <- readRDS(here("modeling", "results", "lnorm_sex.rds"))

sample_size_sex_male <- rstan::extract(sex_results, "sample_size_male")[[1]]
sample_size_sex_female <- rstan::extract(sex_results, "sample_size_female")[[1]]

sample_size_sex <- sample_size_sex_male + sample_size_sex_female

# total
median(sample_size_sex) #

hdi(sample_size_sex, ci = 0.50) #
hdi(sample_size_sex, ci = 0.95) #

# male
median(sample_size_sex_male) #

hdi(sample_size_sex_male, ci = 0.50) #
hdi(sample_size_sex_male, ci = 0.95) #

# female
median(sample_size_sex_female) #

hdi(sample_size_sex_female, ci = 0.50) #
hdi(sample_size_sex_female, ci = 0.95) #
