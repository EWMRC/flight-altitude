library(tidyverse)
library(jagsUI)
library(tidybayes)
library(ggpubr)
library(here)
library(ggimage)

# original_results <- readRDS(here("bayesian_modeling", "gamma_original.rds"))
original_results <- readRDS(here("bayesian_modeling", "gamma_original_stan.rds"))

#examining mean flight altitudes
# mean is shape/rate
mean_altitude <- (rstan::extract(original_results, "shape")[[1]]/rstan::extract(original_results, "rate")[[1]])*2183.475
median(mean_altitude) #361.9766

mean_altitude <- tibble(samples = mean_altitude)

# plot options: (going with basic for now)
# basic
plot_mean_basic <- ggplot(mean_altitude, aes(x = samples)) +
  stat_halfeye(slab_alpha = 0.5,
               slab_fill = "#636363",
               slab_color = "black",
               .width	= c(0.5, 0.95)) +
  stat_halfeye(slab_color = "black", fill = NA,
               .width	= c(0.5, 0.95)) +
  theme_bw() +
  labs(x = "Mean",
       y = "Probability density",
       title  = "A") +
  theme(legend.position = "none") +
  lims(x = c(0,750))  +
  scale_x_continuous(label = scales::label_number(suffix = "m"))


# plot_mean_basic
# 
# sd_altitude <- ((rstan::extract(original_results, "shape")[[1]]/((rstan::extract(original_results, "rate")[[1]])^2))^0.5)*2183.475
# median(sd_altitude) #339.7391
# 
# sd_altitude <- tibble(samples = sd_altitude)
# 
# plot_sd_basic <- ggplot(sd_altitude, aes(x = samples)) +
#   stat_halfeye(slab_alpha = 0.5,
#                slab_fill = "#636363",
#                slab_color = "black",
#                .width	= c(0.5, 0.95)) +
#   stat_halfeye(slab_color = "black", fill = NA,
#                .width	= c(0.5, 0.95)) +
#   theme_bw() +
#   labs(x = "Standard deviation", 
#        y = "Probability density",
#        title  = "B") + 
#   theme(legend.position = "none",
#         axis.title.y = element_blank()) +
#   lims(x = c(0,750)) +
#   scale_x_continuous(label = scales::label_number(suffix = "m"))
# 
# 
# plot_sd_basic
# 
# # combining mean and sd into a single plot
# plot_mean_sd <- ggarrange(plot_mean_basic, plot_sd_basic) #labels="AUTO"

# graphing shape and rate
# simulating density graphs for each drawn combination of shape and rate 

draws_sampled <- 1:length(rstan::extract(original_results, "shape")[[1]])

results_shape_rate <- map(draws_sampled, #
    function(index){
      res <- rgamma(n = 1000, 
                    shape = rstan::extract(original_results, "shape")[[1]][index],
                    rate = rstan::extract(original_results, "rate")[[1]][index]) %>% 
        density(n = 200, from = 0, to = 1)
      
      tibble(x = res$x, y = res$y) %>% 
        return()
    }, .progress = TRUE) %>% 
  bind_rows()

results_shape_rate$x <- results_shape_rate$x*2183.475

results_shape_rate$y <- results_shape_rate$y/max(results_shape_rate$y)

plot_shape_rate <- results_shape_rate %>% 
  ggplot(aes(x = x, y = y)) +
  stat_lineribbon(.width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill= "Credible intervals") +
  scale_fill_manual(values = rev(c("#636363", "#bdbdbd", "#f0f0f0"))) +
  #scale_fill_brewer() + 
  theme(legend.position="bottom") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))

# ggsave(plot = plot_shape_rate,
#        filename = here("graph_results", "plot_shape_rate_stan_2.png"),
#        width = 7/1.5,
#        height = 5/1.5)

# version that shows the minimum NEXRAD altitude
plot_shape_rate_nexrad <- plot_shape_rate +
  geom_vline(xintercept = 155, color = "red", linetype = 2, linewidth = 0.5, alpha = 0.5) +
  annotate(geom = "text", x = 400, y = 4.7, label = "Minimum NEXRAD\ndetection altitude", 
           color = "red", size = 2)

# ggsave(plot = plot_shape_rate_nexrad, 
#        filename = here("graph_results", "plot_shape_rate_nexrad_stan.png"),
#        width = 7/1.5,
#        height = 5/1.5)

# version that shows the altitudes of potential airspace obstacles

icon_tibble <- tibble(x = 2000, y = c(4.25/7.598946, 2.75/7.598946, 1.25/7.598946), 
                      image = c(here("graph_results", "icons", "building.png"), 
                                here("graph_results", "icons", "wind-turbine.png"),
                                here("graph_results", "icons", "signal.png")),
                      col = c("#f1724e", "#f9956e", "#ffcda0")) #"#f1724e", "#f9956e", "#ffcda0"

collision_plot <-  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 47, ymin = -1, ymax = 6), alpha = 0.6, fill = "#f03b20",
            color = "black", linewidth = 0.1, linetype = "dashed") + #cb0824
  geom_rect(aes(xmin = 47, xmax = 163.8, ymin = -1, ymax = 6), alpha = 0.6, fill = "#feb24c",
            color = "black", linewidth = 0.1, linetype = "dashed") + #f04122
  geom_rect(aes(xmin = 163.8, xmax = 305, ymin = -1, ymax = 6), alpha = 0.6, fill = "#ffe162",
            color = "black", linewidth = 0.1, linetype = "dashed") + #fe8211
  #geom_rect(aes(xmin = 1750, xmax = 2250, ymin = 0.05, ymax = 0.7), alpha = 0.4, fill = "darkgrey") +
  stat_lineribbon(data = results_shape_rate, mapping = aes(x = x, y = y), .width = c(.95, .8, .5), 
                  alpha = 0.8) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Probability density", fill= "Credible intervals") +
  scale_fill_manual(values = rev(c("#636363", "#bdbdbd", "#f0f0f0"))) +
  #scale_fill_brewer() + 
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0, 0.8)) +
  scale_x_continuous(label = scales::label_number(suffix = "m")) +
  geom_image(mapping = aes(x = x, y = y, image = image), 
             data = icon_tibble, 
             color = c("#f03b20", "#feb24c", "#f6c700"), #"#ef5a30", "#f87c4c", "#ffd523"
             size = 0.2,
             alpha = 0.8) +
  guides(color = "none") 

ggsave(plot = collision_plot, 
       filename = here("graph_results", "collision_plot_stan.png"),
       width = 7/1.5,
       height = 5/1.5)

 # smaller version for dissertation defense

icon_tibble_small <- tibble(x = c(25, 100, 230), y = 0.5, 
                            image = c(here("graph_results", "building.png"), 
                                      here("graph_results", "wind-turbine.png"),
                                      here("graph_results", "signal.png")),
                            col = c("#f1724e", "#f9956e", "#ffcda0"))

collision_plot_small <-  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 47, ymin = -1, ymax = 6), alpha = 0.4, fill = "#cb0824") +
  geom_rect(aes(xmin = 0, xmax = 305, ymin = -1, ymax = 6), alpha = 0.4, fill = "#fe8211") +
  geom_rect(aes(xmin = 0, xmax = 163.8, ymin = -1, ymax = 6), alpha = 0.4, fill = "#f04122") + #32.2
  stat_lineribbon(data = results_shape_rate, mapping = aes(x = x, y = y), .width = c(.95, .8, .5), 
                  alpha = 0.8) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Probability density", fill= "Credible intervals") +
  scale_fill_manual(values = rev(c("#636363", "#bdbdbd", "#f0f0f0"))) +
  #scale_fill_brewer() + 
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0, 5), xlim = c(0,350)) +
  scale_x_continuous(label = scales::label_number(suffix = "m")) +
  geom_image(mapping = aes(x = x, y = y, image = image), 
             data = icon_tibble_small, 
             color = c("#85250a", "#9c3006", "#ba5800"),
             size = 0.2) +
  guides(color = "none")

# ggsave(plot = collision_plot_small, 
#        filename = here("graph_results", "collision_plot_small_stan.png"),
#        width = 7/1.5,
#        height = 5/1.5)
