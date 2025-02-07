library(tidyverse)
library(jagsUI)
library(tidybayes)
library(ggpubr)
library(here)
library(ggnewscale)

season_results <- readRDS(here("modeling", "results", "lnorm_season.rds"))
age_results <- readRDS(here("modeling", "results", "lnorm_age.rds"))
sex_results <- readRDS(here("modeling", "results", "lnorm_sex.rds"))

## Season
mean_altitude_fall <- map2_dbl(rstan::extract(season_results, "mu_alt_fall")[[1]], rstan::extract(season_results, "sigma_alt_fall")[[1]],
     function(mu, sigma){
       exp(mu + ((sigma^2)/2))*2183.475
     })
median(mean_altitude_fall) #337.767

mean_altitude_spring <- map2_dbl(rstan::extract(season_results, "mu_alt_spring")[[1]], rstan::extract(season_results, "sigma_alt_spring")[[1]],
                                 function(mu, sigma){
                                   exp(mu + ((sigma^2)/2))*2183.475
                                 })
median(mean_altitude_spring) #444.0911

mean_altitude_fall <- tibble(season = "Fall", samples = mean_altitude_fall)
mean_altitude_spring <- tibble(season = "Spring", samples = mean_altitude_spring)
mean_altitude_season <- bind_rows(mean_altitude_fall, mean_altitude_spring)

# note that plot uses median and highest density interval
plot_mean_season <- ggplot(mean_altitude_season, aes(x = samples, 
                                             group = season,
                                             color = season)) +
  stat_slab(aes(fill = season),
            slab_alpha = 0.5, slab_color = "black") +
  stat_slab(slab_color = "black",
            fill = NA) +
  stat_pointinterval(position = position_nudge(x = c(-0.03, -0.08), 
                                               y = c(-0.03, -0.08)),
                     linewidth = 1,
                     point_size = 2,
                     .width	= c(0.95)) +
  theme_bw() +
  labs(x = "", 
       y = "Probability density",
       title  = "Season",
       fill = "Season") + 
  scale_fill_manual(values = c("Fall" = "#e6550d", "Spring" = "#756bb1")) +
  scale_color_manual(values = c("Fall" = "#e6550d", "Spring" = "#756bb1")) +
  lims(x = c(0,1500), y = c(-0.08, 1)) +
  guides(color = "none") +
  scale_x_continuous(#label = scales::label_number(big.mark = "",
                    #                              suffix = "m"),
                     breaks = seq(200, 1600, 200)) +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  expand_limits(x = c(200, 1200)) +
  annotate("text", label = "Fall", x = 318, y = 0.965, col = "#e6550d") +
  annotate("text", label = "Spring", x = 520, y = 0.66, col = "#756bb1")

plot_mean_season

## Age
mean_altitude_adult <- map2_dbl(rstan::extract(age_results, "mu_alt_adult")[[1]], rstan::extract(age_results, "sigma_alt_adult")[[1]],
                                function(mu, sigma){
                                  exp(mu + ((sigma^2)/2))*2183.475
                                })
median(mean_altitude_adult) #433.7849

mean_altitude_juv <- map2_dbl(rstan::extract(age_results, "mu_alt_juv")[[1]], rstan::extract(age_results, "sigma_alt_juv")[[1]],
                              function(mu, sigma){
                                exp(mu + ((sigma^2)/2))*2183.475
                              })
median(mean_altitude_juv) #369.3922

mean_altitude_adult <- tibble(age = "Adult", samples = mean_altitude_adult)
mean_altitude_juv <- tibble(age = "Juvenile", samples = mean_altitude_juv)
mean_altitude_age <- bind_rows(mean_altitude_adult, mean_altitude_juv)

# note that plot uses median and highest density interval
plot_mean_age <- ggplot(mean_altitude_age, aes(x = samples, 
                                             group = age,
                                             color = age)) +
  stat_slab(aes(fill = age),
            slab_alpha = 0.5, slab_color = "black") +
  stat_slab(slab_color = "black",
            fill = NA) +
  stat_pointinterval(position = position_nudge(x = c(-0.03, -0.08), 
                                               y = c(-0.03, -0.08)),
                     linewidth = 1,
                     point_size = 2,
                     .width	= c(0.95)) +
  theme_bw() +
  labs(x = "Mean altitude (m)", 
       y = "",
       title  = "Age",
       fill = "Age") + 
  scale_fill_manual(values = c("Adult" = "#de2d26", "Juvenile" = "#3182bd")) +
  scale_color_manual(values = c("Adult" = "#de2d26", "Juvenile" = "#3182bd")) +
  lims(x = c(0,1500), y = c(-0.08, 1)) + 
  guides(color = "none")  +
  scale_x_continuous(#label = scales::label_number(big.mark = "",
                    #                              suffix = "m"),
                     breaks = seq(200, 1600, 200)) +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  expand_limits(x = c(200, 1200)) +
  annotate("text", label = "Juvenile", x = 350, y = 0.96, col = "#3182bd") +
  annotate("text", label = "Adult", x = 525, y = 0.67, col = "#de2d26")

plot_mean_age
 
## Sex
mean_altitude_female <- map2_dbl(rstan::extract(sex_results, "mu_alt_female")[[1]], rstan::extract(sex_results, "sigma_alt_female")[[1]],
                                 function(mu, sigma){
                                   exp(mu + ((sigma^2)/2))*2183.475
                                 })
median(mean_altitude_female) #351.6217

mean_altitude_male <- map2_dbl(rstan::extract(sex_results, "mu_alt_male")[[1]], rstan::extract(sex_results, "sigma_alt_male")[[1]],
                               function(mu, sigma){
                                 exp(mu + ((sigma^2)/2))*2183.475
                               })
median(mean_altitude_male) #416.6841

mean_altitude_female <- tibble(sex = "Female", samples = mean_altitude_female)
mean_altitude_male <- tibble(sex = "Male", samples = mean_altitude_male)
mean_altitude_sex <- bind_rows(mean_altitude_female, mean_altitude_male)

# note that plot uses median and highest density interval
plot_mean_sex <- ggplot(mean_altitude_sex, aes(x = samples, 
                                                     group = sex,
                                                     color = sex)) +
  stat_slab(aes(fill = sex),
            slab_alpha = 0.5, slab_color = "black") +
  stat_slab(slab_color = "black",
            fill = NA) +
  stat_pointinterval(position = position_nudge(x = c(-0.03, -0.08), 
                                               y = c(-0.03, -0.08)),
                     linewidth = 1,
                     point_size = 2,
                     .width	= c(0.95)) +
  theme_bw() +
  labs(x = "", 
       y = "",
       title  = "Sex",
       fill = "Sex") + 
  scale_fill_manual(values = c("Female" = "#c5247d", "Male" = "#4d9221")) +
  scale_color_manual(values = c("Female" = "#c5247d", "Male" = "#4d9221")) +
  lims(x = c(0,1500), y = c(-0.08, 1)) +
  guides(color = "none") +
  scale_x_continuous(#label = scales::label_number(big.mark = "",
                     #                              suffix = "m"),
                     breaks = seq(200, 1600, 200)) +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) +
  expand_limits(x = c(200, 1200)) +
  annotate("text", label = "Female", x = 330, y = 0.97, col = "#c5247d") +
  annotate("text", label = "Male", x = 550, y = 0.82, col = "#4d9221") 

plot_mean_sex

## Graph combined plots
# extract legends
plot_mean_season_p <- plot_mean_season +
  theme(legend.position = "none")
plot_mean_season_l <- get_legend(plot_mean_season) %>% 
  as_ggplot()

plot_mean_age_p <- plot_mean_age +
  theme(legend.position = "none")
plot_mean_age_l <- get_legend(plot_mean_age) %>% 
  as_ggplot()

plot_mean_sex_p <- plot_mean_sex +
  theme(legend.position = "none")
plot_mean_sex_l <- get_legend(plot_mean_sex) %>% 
  as_ggplot()


plot_combined <- ggarrange(plot_mean_season_p, plot_mean_age_p, plot_mean_sex_p, 
                           nrow = 1, ncol = 3) 

ggsave(plot = plot_combined, 
       filename = here("graph_results", "figures", "subset_plot_stan.png"),
       width = 8.5/1.2,
       height = 3/1.2,
       units = "in")
 
