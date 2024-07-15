library(tidyverse)
library(jagsUI)
library(tidybayes)
library(ggpubr)
library(here)
library(ggnewscale)

age_results <- readRDS(here("bayesian_modeling", "stan", "gamma_age_stan.rds"))

#examining mean flight altitudes
# mean is shape/rate
mean_altitude_adult <- (rstan::extract(age_results, "shape_adult")[[1]]/rstan::extract(age_results, "rate_adult")[[1]])*2183.475
median(mean_altitude_adult) #397.6309

mean_altitude_juv <- (rstan::extract(age_results, "shape_juv")[[1]]/rstan::extract(age_results, "rate_juv")[[1]])*2183.475
median(mean_altitude_juv) #342.4547

mean_altitude_adult <- tibble(age = "Adult", samples = mean_altitude_adult)
mean_altitude_juv <- tibble(age = "Juvenile", samples = mean_altitude_juv)
mean_altitude <- bind_rows(mean_altitude_adult, mean_altitude_juv)

# overlap version
# note that plot uses median and highest density interval
plot_mean_basic <- ggplot(mean_altitude, aes(x = samples, 
                                             group = age,
                                             color = age)) +
  stat_slab(aes(fill = age),
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
       fill = "Age") + 
  scale_fill_manual(values = c("Adult" = "#de2d26", "Juvenile" = "#3182bd")) +
  scale_color_manual(values = c("Adult" = "#de2d26", "Juvenile" = "#3182bd")) +
  lims(y = c(-0.08, 1), x = c(0,750)) + #add a disclaimer that I cropped this
  guides(color = "none")  +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

plot_mean_basic

# examining sd of flight altitudes
# sd is (shape/((rate)^2))^0.5
sd_altitude_adult <- ((rstan::extract(age_results, "shape_adult")[[1]]/((rstan::extract(age_results, "rate_adult")[[1]])^2))^0.5)*2183.475
median(sd_altitude_adult) #359.4154

sd_altitude_juv <- ((rstan::extract(age_results, "shape_juv")[[1]]/((rstan::extract(age_results, "rate_juv")[[1]])^2))^0.5)*2183.475
median(sd_altitude_juv) #299.9753

sd_altitude_adult <- tibble(age = "Adult", samples = sd_altitude_adult)
sd_altitude_juv <- tibble(age = "Juvenile", samples = sd_altitude_juv)
sd_altitude <- bind_rows(sd_altitude_adult, sd_altitude_juv)

plot_sd_basic <- ggplot(sd_altitude, aes(x = samples, 
                                         group = age,
                                         color = age)) +
  stat_slab(aes(fill = age),
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
       fill = "Age") + 
  scale_fill_manual(values = c("Adult" = "#de2d26", "Juvenile" = "#3182bd")) +
  scale_color_manual(values = c("Adult" = "#de2d26", "Juvenile" = "#3182bd")) +
  lims(y = c(-0.08, 1), x = c(0,750)) +
  guides(color = "none") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

plot_sd_basic

# combining mean and sd into a single plot
plot_mean_sd <- ggarrange(plot_mean_basic, plot_sd_basic,
                          legend = "bottom",
                          common.legend = TRUE) +
  bgcolor("white") +
  border("white")

ggsave(filename = here("graph_results", "plot_mean_sd_age_stan.png"),
       plot = plot_mean_sd,
       width = 8/1.5,
       height = 7/1.5)

# graphing shape and rate
# simulating density graphs for each drawn combination of shape and rate 

# subsample to a reasonable number of draws
set.seed(8)
draws_sampled_adult <- 1:length(rstan::extract(age_results, "shape_adult")[[1]])

results_shape_rate_adult <- map(draws_sampled_adult, #
                               function(index){
                                 res <- rgamma(n = 1000, 
                                               shape = rstan::extract(age_results, "shape_adult")[[1]][index],
                                               rate = rstan::extract(age_results, "rate_adult")[[1]][index]) %>% 
                                   density(n = 200, from = 0, to = 1)
                                 
                                 tibble(x = res$x, y = res$y) %>% 
                                   return()
                               }, .progress = TRUE) %>% 
  bind_rows()

results_shape_rate_adult$x <- results_shape_rate_adult$x*2183.475

set.seed(8)
draws_sampled_juv <- 1:length(rstan::extract(age_results, "shape_juv")[[1]])

results_shape_rate_juv <- map(draws_sampled_juv, #
                                 function(index){
                                   res <- rgamma(n = 1000, 
                                                 shape = rstan::extract(age_results, "shape_juv")[[1]][index],
                                                 rate = rstan::extract(age_results, "rate_juv")[[1]][index]) %>% 
                                     density(n = 200, from = 0, to = 1)
                                   
                                   tibble(x = res$x, y = res$y) %>% 
                                     return()
                                 }, .progress = TRUE) %>% 
  bind_rows()

results_shape_rate_juv$x <- results_shape_rate_juv$x*2183.475

results_shape_rate_adult <- results_shape_rate_adult %>% 
  mutate(age = "Adult")

results_shape_rate_juv <- results_shape_rate_juv %>% 
  mutate(age = "Juvenile")

results_shape_rate_age <- bind_rows(results_shape_rate_adult, results_shape_rate_juv)

plot_shape_rate_age <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_age, age == "Adult"),
                  mapping = aes(x = x, y = y, group = age),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Credible intervals") +
  scale_fill_manual(values = c("#fee0d2", "#fc9272", "#de2d26")) +
  theme(legend.position = "none") +
  facet_wrap(vars(age), nrow = 2, ncol = 1) +
  new_scale_fill() +
  stat_lineribbon(data = filter(results_shape_rate_age, age == "Juvenile"),
                  mapping = aes(x = x, y = y, group = age),
                  .width = c(.95, .8, .5), alpha = 0.5) +
  scale_fill_manual(values = c("#deebf7", "#9ecae1", "#3182bd"))  +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

legend_adult <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_age, age == "Adult"),
                  mapping = aes(x = x, y = y, group = age),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Adult credible intervals") +
  scale_fill_manual(values = c("#fee0d2", "#fc9272", "#de2d26")) +
  theme(legend.position = "bottom")  +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

legend_adult <- get_legend(legend_adult)

legend_juv <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_age, age == "Juvenile"),
                  mapping = aes(x = x, y = y, group = age),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Juvenile credible intervals") +
  scale_fill_manual(values = c("#deebf7", "#9ecae1", "#3182bd")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

legend_juv <- get_legend(legend_juv)

plot_shape_rate_age_combined <- ggarrange(plot_shape_rate_age,
                                             legend_adult,
                                             legend_juv,
                                             nrow = 3, 
                                             ncol = 1,
                                             heights = c(10,1,1)) +
  bgcolor("white")

ggsave(plot = plot_shape_rate_age_combined, 
       filename = here("graph_results", "plot_shape_rate_age_stan.png"),
       width = 7/1.5,
       height = 6/1.5)
