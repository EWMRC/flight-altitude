library(tidyverse)
library(jagsUI)
library(tidybayes)
library(ggpubr)
library(here)
library(ggnewscale)

sex_results <- readRDS(here("bayesian_modeling", "gamma_sex_300k.rds"))

#examining mean flight altitudes
# mean is shape/rate
mean_altitude_female <- (sex_results$sims.list$shape_flight[, 1]/sex_results$sims.list$rate_flight[,1])*2183.475
mean(mean_altitude_female)
sd(mean_altitude_female)
quantile(mean_altitude_female, c(.025,.975))

mean_altitude_male <- (sex_results$sims.list$shape_flight[, 2]/sex_results$sims.list$rate_flight[,2])*2183.475
mean(mean_altitude_male)
sd(mean_altitude_male)
quantile(mean_altitude_male, c(.025,.975))

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
                                               y = c(-0.03, -0.03, -0.08, -0.08))) +
  theme_bw() +
  labs(x = "Mean", 
       y = "Probability density",
       title  = "A",
       fill = "sex") + 
  scale_fill_manual(values = c("Female" = "#de2d26", "Male" = "#3182bd")) +
  scale_color_manual(values = c("Female" = "#de2d26", "Male" = "#3182bd")) +
  lims(y = c(-0.08, 1), x = c(0,750)) + #add a disclaimer that I cropped this
  guides(color = "none")

plot_mean_basic

# examining sd of flight altitudes
# sd is (shape/((rate)^2))^0.5
sd_altitude_female <- ((sex_results$sims.list$shape_flight[, 1]/((sex_results$sims.list$rate_flight[, 1])^2))^0.5)*2183.475
mean(sd_altitude_female)
sd(sd_altitude_female)
quantile(sd_altitude_female, c(.025,.975))

sd_altitude_male <- ((sex_results$sims.list$shape_flight[, 2]/((sex_results$sims.list$rate_flight[, 2])^2))^0.5)*2183.475
mean(sd_altitude_male)
sd(sd_altitude_male)
quantile(sd_altitude_male, c(.025,.975))

sd_altitude_female <- tibble(sex = "Female", samples = sd_altitude_female)
sd_altitude_male <- tibble(sex = "Male", samples = sd_altitude_male)
sd_altitude <- bind_rows(sd_altitude_female, sd_altitude_male)

plot_sd_basic <- ggplot(sd_altitude, aes(x = samples, 
                                         group = sex,
                                         color = sex)) +
  stat_slab(aes(fill = sex),
            slab_alpha = 0.5, slab_color = "black") +
  stat_slab(slab_color = "black",
            fill = NA) +
  stat_pointinterval(position = position_nudge(x = c(-0.03, -0.03, -0.08, -0.08), 
                                               y = c(-0.03, -0.03, -0.08, -0.08))) +
  theme_bw() +
  labs(x = "Standard deviation", 
       y = "Probability density",
       title = "B",
       fill = "sex") + 
  scale_fill_manual(values = c("Female" = "#de2d26", "Male" = "#3182bd")) +
  scale_color_manual(values = c("Female" = "#de2d26", "Male" = "#3182bd")) +
  lims(y = c(-0.08, 1), x = c(0,750)) +
  guides(color = "none")

plot_sd_basic

# combining mean and sd into a single plot
plot_mean_sd <- ggarrange(plot_mean_basic, plot_sd_basic,
                          legend = "bottom",
                          common.legend = TRUE) +
  bgcolor("white")

ggsave(filename = here("graph_results", "plot_mean_sd_sex.png"),
       plot = plot_mean_sd,
       width = 7/1.5,
       height = 7/1.5)

# graphing shape and rate
# simulating density graphs for each drawn combination of shape and rate 

# subsample to a reasonable number of draws
set.seed(8)
draws_sampled_female <- sample(1:length(sex_results$sims.list$shape_flight[,1]), 
                              size = 10000)

results_shape_rate_female <- map(draws_sampled_female, #
                                function(index){
                                  res <- rgamma(n = 1000, 
                                                shape = sex_results$sims.list$shape_flight[index,1],
                                                rate = sex_results$sims.list$rate_flight[index,1]) %>% 
                                    density(n = 200, from = 0, to = 1)
                                  
                                  tibble(x = res$x, y = res$y) %>% 
                                    return()
                                }, .progress = TRUE) %>% 
  bind_rows()

results_shape_rate_female$x <- results_shape_rate_female$x*2183.475

set.seed(8)
draws_sampled_male <- sample(1:length(sex_results$sims.list$shape_flight[,2]), 
                            size = 10000)

results_shape_rate_male <- map(draws_sampled_male, #
                              function(index){
                                res <- rgamma(n = 1000, 
                                              shape = sex_results$sims.list$shape_flight[index,2],
                                              rate = sex_results$sims.list$rate_flight[index,2]) %>% 
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
  scale_fill_manual(values = c("#fee0d2", "#fc9272", "#de2d26")) +
  theme(legend.position = "none") +
  facet_wrap(vars(sex), nrow = 2, ncol = 1) +
  new_scale_fill() +
  stat_lineribbon(data = filter(results_shape_rate_sex, sex == "Male"),
                  mapping = aes(x = x, y = y, group = sex),
                  .width = c(.95, .8, .5), alpha = 0.5) +
  scale_fill_manual(values = c("#deebf7", "#9ecae1", "#3182bd"))

legend_female <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_sex, sex == "Female"),
                  mapping = aes(x = x, y = y, group = sex),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Female credible intervals") +
  scale_fill_manual(values = c("#fee0d2", "#fc9272", "#de2d26")) +
  theme(legend.position = "bottom")

legend_female <- get_legend(legend_female)

legend_male <- ggplot() +
  stat_lineribbon(data = filter(results_shape_rate_sex, sex == "Male"),
                  mapping = aes(x = x, y = y, group = sex),
                  .width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill = "Male credible intervals") +
  scale_fill_manual(values = c("#deebf7", "#9ecae1", "#3182bd")) +
  theme(legend.position = "bottom")

legend_male <- get_legend(legend_male)

plot_shape_rate_sex_combined <- ggarrange(plot_shape_rate_sex,
                                          legend_female,
                                          legend_male,
                                          nrow = 3, 
                                          ncol = 1,
                                          heights = c(10,1,1)) +
  bgcolor("white")

ggsave(plot = plot_shape_rate_sex_combined, 
       filename = here("graph_results", "plot_shape_rate_sex.png"),
       width = 7/1.5,
       height = 6/1.5)
