library(tidyverse)
library(jagsUI)
library(tidybayes)
library(ggpubr)
library(here)
library(ggnewscale)

sex_results <- readRDS(here("bayesian_modeling", "stan", "gamma_sex_stan.rds"))

#examining mean flight altitudes
# mean is shape/rate
mean_altitude_female <- (rstan::extract(sex_results, "shape_female")[[1]]/rstan::extract(sex_results, "rate_female")[[1]])*2183.475
median(mean_altitude_female) #333.6355

mean_altitude_male <- (rstan::extract(sex_results, "shape_male")[[1]]/rstan::extract(sex_results, "rate_male")[[1]])*2183.475
median(mean_altitude_male) #392.312

mean_altitude_female <- tibble(sex = "Female", samples = mean_altitude_female)
mean_altitude_male <- tibble(sex = "Male", samples = mean_altitude_male)
mean_altitude <- bind_rows(mean_altitude_female, mean_altitude_male)

# overlap version
# note that plot uses median and highest density interval
plot_mean_basic <- ggplot(mean_altitude, aes(x = samples, 
                                             group = sex,
                                             color = sex)) +
  stat_slab(aes(fill = sex),
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
       fill = "Sex") + 
  scale_fill_manual(values = c("Female" = "#e6550d", "Male" = "#756bb1")) +
  scale_color_manual(values = c("Female" = "#e6550d", "Male" = "#756bb1")) +
  lims(x = c(0,750), y = c(-0.08, 1)) +
  guides(color = "none") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))
  

plot_mean_basic

# examining sd of flight altitudes
# sd is (shape/((rate)^2))^0.5
sd_altitude_female <- ((rstan::extract(sex_results, "shape_female")[[1]]/((rstan::extract(sex_results, "rate_female")[[1]])^2))^0.5)*2183.475
median(sd_altitude_female) #288.9426

sd_altitude_male <- ((rstan::extract(sex_results, "shape_male")[[1]]/((rstan::extract(sex_results, "rate_male")[[1]])^2))^0.5)*2183.475
median(sd_altitude_male) #359.6968

sd_altitude_female <- tibble(sex = "Female", samples = sd_altitude_female)
sd_altitude_male <- tibble(sex = "Male", samples = sd_altitude_male)
sd_altitude <- bind_rows(sd_altitude_female, sd_altitude_male)

plot_sd_basic <- ggplot(sd_altitude, aes(x = samples, 
                                             group = sex,
                                             color = sex)) +
  # stat_halfeye(aes(fill = sex),
  #              slab_alpha = 0.5,
  #              #slab_fill = "#3182bd",
  #              slab_color = "black") +
  # stat_halfeye(slab_color = "black", 
  #              fill = NA) +
  stat_slab(aes(fill = sex),
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
       fill = "Sex") + 
  scale_fill_manual(values = c("Female" = "#e6550d", "Male" = "#756bb1")) +
  scale_color_manual(values = c("Female" = "#e6550d", "Male" = "#756bb1")) +
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

ggsave(filename = here("graph_results", "plot_mean_sd_sex_stan.png"),
       plot = plot_mean_sd,
       width = 8/1.5,
       height = 7/1.5)

# graphing shape and rate
# simulating density graphs for each drawn combination of shape and rate 

# subsample to a reasonable number of draws
set.seed(8)
draws_sampled_female <- 1:length(rstan::extract(sex_results, "shape_female")[[1]])

results_shape_rate_female <- map(draws_sampled_female, #
    function(index){
      res <- rgamma(n = 1000, 
             shape = rstan::extract(sex_results, "shape_female")[[1]][index],
             rate = rstan::extract(sex_results, "rate_female")[[1]][index]) %>% 
        density(n = 200, from = 0, to = 1)
      
      tibble(x = res$x, y = res$y) %>% 
        return()
    }, .progress = TRUE) %>% 
  bind_rows()

results_shape_rate_female$x <- results_shape_rate_female$x*2183.475

set.seed(8)
draws_sampled_male <- 1:length(rstan::extract(sex_results, "shape_male")[[1]])

results_shape_rate_male <- map(draws_sampled_male, #
                               function(index){
                                 res <- rgamma(n = 1000, 
                                               shape = rstan::extract(sex_results, "shape_male")[[1]][index],
                                               rate = rstan::extract(sex_results, "rate_male")[[1]][index]) %>% 
                                   density(n = 200, from = 0, to = 1)
                                 
                                 tibble(x = res$x, y = res$y) %>% 
                                   return()
                               }, .progress = TRUE) %>% 
  bind_rows()

results_shape_rate_male$x <- results_shape_rate_male$x*2183.475

results_shape_rate_female <- results_shape_rate_female %>% 
  mutate(sex = "Female")

results_shape_rate_male <- results_shape_rate_male %>% 
  mutate(sex = "Male")

results_shape_rate_sex <- bind_rows(results_shape_rate_female, results_shape_rate_male)

plot_shape_rate_sex <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_sex, sex == "Female"),
                  mapping = aes(x = x, y = y, group = sex),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Credible intervals") +
  scale_fill_manual(values = c("#fee6ce", "#fdae6b", "#e6550d")) +
  theme(legend.position = "none") +
  facet_wrap(vars(sex), nrow = 2, ncol = 1) +
  new_scale_fill() +
  stat_lineribbon(data = filter(results_shape_rate_sex, sex == "Male"),
                  mapping = aes(x = x, y = y, group = sex),
                  .width = c(.95, .8, .5), alpha = 0.5) +
  scale_fill_manual(values = c("#efedf5", "#bcbddc", "#756bb1")) +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

legend_female <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_sex, sex == "Female"),
                  mapping = aes(x = x, y = y, group = sex),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Female credible intervals") +
  scale_fill_manual(values = c("#fee6ce", "#fdae6b", "#e6550d")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

legend_female <- get_legend(legend_female)

legend_male <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_sex, sex == "Male"),
                  mapping = aes(x = x, y = y, group = sex),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Male credible intervals") +
  scale_fill_manual(values = c("#efedf5", "#bcbddc", "#756bb1")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

legend_male <- get_legend(legend_male)

plot_shape_rate_sex_combined <- ggarrange(plot_shape_rate_sex,
                                             legend_female,
                                             legend_male,
                                             nrow = 3, 
                                             ncol = 1,
                                             heights = c(10,1,1)) +
  bgcolor("white")

ggsave(plot = plot_shape_rate_sex_combined, #this is broken, but I'm also not going to use it so nbd
       filename = here("graph_results", "plot_shape_rate_sex_stan.png"),
       width = 7/1.5,
       height = 6/1.5)
