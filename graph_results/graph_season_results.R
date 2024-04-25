library(tidyverse)
library(jagsUI)
library(tidybayes)
library(ggpubr)
library(here)
library(ggnewscale)

season_results <- readRDS(here("bayesian_modeling", "gamma_season.rds"))
# season_results <- readRDS(here("bayesian_modeling", "gamma_season.rds"))

#examining mean flight altitudes
# mean is shape/rate
mean_altitude_fall <- (season_results$sims.list$shape_flight[, 1]/season_results$sims.list$rate_flight[,1])*2183.475
mean(mean_altitude_fall)
sd(mean_altitude_fall)
quantile(mean_altitude_fall, c(.025,.975))

mean_altitude_spring <- (season_results$sims.list$shape_flight[, 2]/season_results$sims.list$rate_flight[,2])*2183.475
mean(mean_altitude_spring)
sd(mean_altitude_spring)
quantile(mean_altitude_spring, c(.025,.975))

mean_altitude_fall <- tibble(season = "Fall", samples = mean_altitude_fall)
mean_altitude_spring <- tibble(season = "Spring", samples = mean_altitude_spring)
mean_altitude <- bind_rows(mean_altitude_fall, mean_altitude_spring)

# plot options: (going with basic for now)
# basic
# plot_mean_basic <- ggplot(mean_altitude, aes(x = samples, 
#                                              group = season,
#                                              color = season)) +
#   stat_halfeye(aes(fill = season),
#                slab_alpha = 0.5,
#                #slab_fill = "#3182bd",
#                slab_color = "black") +
#   stat_halfeye(slab_color = "black", 
#                fill = NA) +
#   theme_bw() +
#   labs(x = "Mean", 
#        y = "Probability density",
#        title  = "A") + 
#   theme(legend.position = "none") +
#   facet_wrap(vars(season),
#              nrow = 2,
#              ncol = 1) +
#   scale_fill_manual(values = c("Fall" = "#A82203FF", "Spring" = "#003967FF")) +
#   scale_color_manual(values = c("Fall" = "#A82203FF", "Spring" = "#003967FF"))

# overlap version
# note that plot uses median and highest density interval
plot_mean_basic <- ggplot(mean_altitude, aes(x = samples, 
                                             group = season,
                                             color = season)) +
  # stat_halfeye(aes(fill = season),
  #              slab_alpha = 0.5,
  #              #slab_fill = "#3182bd",
  #              slab_color = "black") +
  # stat_halfeye(slab_color = "black", 
  #              fill = NA) +
  stat_slab(aes(fill = season),
            slab_alpha = 0.5, slab_color = "black") +
  stat_slab(slab_color = "black",
            fill = NA) +
  stat_pointinterval(position = position_nudge(x = c(-0.03, -0.03, -0.08, -0.08), 
                                               y = c(-0.03, -0.03, -0.08, -0.08)),
                     .width	= c(0.5, 0.95)) +
  theme_bw() +
  labs(x = "Mean", 
       y = "Probability density",
       title  = "A",
       fill = "Season") + 
  scale_fill_manual(values = c("Fall" = "#e6550d", "Spring" = "#756bb1")) +
  scale_color_manual(values = c("Fall" = "#e6550d", "Spring" = "#756bb1")) +
  lims(x = c(0,750), y = c(-0.08, 1)) +
  guides(color = "none") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))
  

plot_mean_basic

# examining sd of flight altitudes
# sd is (shape/((rate)^2))^0.5
sd_altitude_fall <- ((season_results$sims.list$shape_flight[, 1]/((season_results$sims.list$rate_flight[, 1])^2))^0.5)*2183.475
mean(sd_altitude_fall)
sd(sd_altitude_fall)
quantile(sd_altitude_fall, c(.025,.975))

sd_altitude_spring <- ((season_results$sims.list$shape_flight[, 2]/((season_results$sims.list$rate_flight[, 2])^2))^0.5)*2183.475
mean(sd_altitude_spring)
sd(sd_altitude_spring)
quantile(sd_altitude_spring, c(.025,.975))

sd_altitude_fall <- tibble(season = "Fall", samples = sd_altitude_fall)
sd_altitude_spring <- tibble(season = "Spring", samples = sd_altitude_spring)
sd_altitude <- bind_rows(sd_altitude_fall, sd_altitude_spring)

plot_sd_basic <- ggplot(sd_altitude, aes(x = samples, 
                                             group = season,
                                             color = season)) +
  # stat_halfeye(aes(fill = season),
  #              slab_alpha = 0.5,
  #              #slab_fill = "#3182bd",
  #              slab_color = "black") +
  # stat_halfeye(slab_color = "black", 
  #              fill = NA) +
  stat_slab(aes(fill = season),
            slab_alpha = 0.5, slab_color = "black") +
  stat_slab(slab_color = "black",
            fill = NA) +
  stat_pointinterval(position = position_nudge(x = c(-0.03, -0.03, -0.08, -0.08), 
                                               y = c(-0.03, -0.03, -0.08, -0.08)),
                     .width	= c(0.5, 0.95)) +
  theme_bw() +
  labs(x = "Standard deviation", 
       y = "Probability density",
       title = "B",
       fill = "Season") + 
  scale_fill_manual(values = c("Fall" = "#e6550d", "Spring" = "#756bb1")) +
  scale_color_manual(values = c("Fall" = "#e6550d", "Spring" = "#756bb1")) +
  lims(x = c(0,750), y = c(-0.08, 1)) +
  guides(color = "none") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

plot_sd_basic

# combining mean and sd into a single plot
plot_mean_sd <- ggarrange(plot_mean_basic, plot_sd_basic,
                          legend = "bottom",
                          common.legend = TRUE) +
  bgcolor("white") +
  border("white")

ggsave(filename = here("graph_results", "plot_mean_sd_season.png"),
       plot = plot_mean_sd,
       width = 8/1.5,
       height = 7/1.5)

# graphing shape and rate
# simulating density graphs for each drawn combination of shape and rate 

# subsample to a reasonable number of draws
set.seed(8)
draws_sampled_fall <- sample(1:length(season_results$sims.list$shape_flight[,1]), 
                        size = 10000)

results_shape_rate_fall <- map(draws_sampled_fall, #
    function(index){
      res <- rgamma(n = 1000, 
             shape = season_results$sims.list$shape_flight[index,1],
             rate = season_results$sims.list$rate_flight[index,1]) %>% 
        density(n = 200, from = 0, to = 1)
      
      tibble(x = res$x, y = res$y) %>% 
        return()
    }, .progress = TRUE) %>% 
  bind_rows()

results_shape_rate_fall$x <- results_shape_rate_fall$x*2183.475

set.seed(8)
draws_sampled_spring <- sample(1:length(season_results$sims.list$shape_flight[,2]), 
                             size = 10000)

results_shape_rate_spring <- map(draws_sampled_spring, #
                               function(index){
                                 res <- rgamma(n = 1000, 
                                               shape = season_results$sims.list$shape_flight[index,2],
                                               rate = season_results$sims.list$rate_flight[index,2]) %>% 
                                   density(n = 200, from = 0, to = 1)
                                 
                                 tibble(x = res$x, y = res$y) %>% 
                                   return()
                               }, .progress = TRUE) %>% 
  bind_rows()

results_shape_rate_spring$x <- results_shape_rate_spring$x*2183.475

results_shape_rate_fall <- results_shape_rate_fall %>% 
  mutate(season = "Fall")

results_shape_rate_spring <- results_shape_rate_spring %>% 
  mutate(season = "Spring")

results_shape_rate_season <- bind_rows(results_shape_rate_fall, results_shape_rate_spring)

plot_shape_rate_season <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_season, season == "Fall"),
                  mapping = aes(x = x, y = y, group = season),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Credible intervals") +
  scale_fill_manual(values = c("#fee6ce", "#fdae6b", "#e6550d")) +
  theme(legend.position = "none") +
  facet_wrap(vars(season), nrow = 2, ncol = 1) +
  new_scale_fill() +
  stat_lineribbon(data = filter(results_shape_rate_season, season == "Spring"),
                  mapping = aes(x = x, y = y, group = season),
                  .width = c(.95, .8, .5), alpha = 0.5) +
  scale_fill_manual(values = c("#efedf5", "#bcbddc", "#756bb1")) +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

legend_fall <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_season, season == "Fall"),
                  mapping = aes(x = x, y = y, group = season),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Fall credible intervals") +
  scale_fill_manual(values = c("#fee6ce", "#fdae6b", "#e6550d")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

legend_fall <- get_legend(legend_fall)

legend_spring <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_season, season == "Spring"),
                  mapping = aes(x = x, y = y, group = season),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Spring credible intervals") +
  scale_fill_manual(values = c("#efedf5", "#bcbddc", "#756bb1")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

legend_spring <- get_legend(legend_spring)

plot_shape_rate_season_combined <- ggarrange(plot_shape_rate_season,
                                             legend_fall,
                                             legend_spring,
                                             nrow = 3, 
                                             ncol = 1,
                                             heights = c(10,1,1)) +
  bgcolor("white")

ggsave(plot = plot_shape_rate_season_combined, 
       filename = here("graph_results", "plot_shape_rate_season.png"),
       width = 7/1.5,
       height = 6/1.5)
