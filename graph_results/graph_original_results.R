library(tidyverse)
library(jagsUI)
library(tidybayes)
library(ggpubr)
library(here)
library(ggimage)

original_results <- readRDS(here("bayesian_modeling", "gamma_original_new.rds"))
original_results_old <- readRDS(here("bayesian_modeling", "gamma_original.rds"))

#examining mean flight altitudes
# mean is shape/rate
mean_altitude <- (original_results$sims.list$shape_flight/original_results$sims.list$rate_flight)*2183.475
mean(mean_altitude)
sd(mean_altitude)
quantile(mean_altitude, c(.025,.975))

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


plot_mean_basic

# intervals
# ggplot(mean_altitude, aes(x = samples)) +
#   stat_slab(aes(fill = after_stat(level)), .width = c(.66, .95, 1), 
#             color = "black") +
#   stat_pointinterval() +
#   theme_bw() +
#   lims(y = c(0,1)) +
#   labs(x = "Mean flight altitude", y = "Probability density") +
#   scale_fill_manual(values = c("1" = "#deebf7",
#                                "0.95" = "#9ecae1",
#                                "0.66" = "#3182bd")) + 
#   theme(legend.position = "none")

# gradient
# ggplot(mean_altitude, aes(x = samples)) +
#   stat_halfeye(aes(slab_alpha = after_stat(f)), fill_type = "gradient",
#                slab_fill = "#3182bd",
#                slab_color = "black") +
#   stat_halfeye(slab_color = "black", fill = NA) +
#   theme_bw() +
#   labs(x = "Mean flight altitude", y = "Probability density") + 
#   theme(legend.position = "none")

# examining sd of flight altitudes
# sd is (shape/((rate)^2))^0.5
sd_altitude <- ((original_results$sims.list$shape_flight/((original_results$sims.list$rate_flight)^2))^0.5)*2183.475
mean(sd_altitude)
sd(sd_altitude)
quantile(sd_altitude, c(.025,.975))

sd_altitude <- tibble(samples = sd_altitude)

plot_sd_basic <- ggplot(sd_altitude, aes(x = samples)) +
  stat_halfeye(slab_alpha = 0.5,
               slab_fill = "#636363",
               slab_color = "black",
               .width	= c(0.5, 0.95)) +
  stat_halfeye(slab_color = "black", fill = NA,
               .width	= c(0.5, 0.95)) +
  theme_bw() +
  labs(x = "Standard deviation", 
       y = "Probability density",
       title  = "B") + 
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  lims(x = c(0,750)) +
  scale_x_continuous(label = scales::label_number(suffix = "m"))


plot_sd_basic

# combining mean and sd into a single plot
plot_mean_sd <- ggarrange(plot_mean_basic, plot_sd_basic) #labels="AUTO"

ggsave(filename = here("graph_results", "plot_mean_sd_new.png"),
       plot = plot_mean_sd,
       width = 7/1.5,
       height = 5/1.5)

# graphing shape and rate
# simulating density graphs for each drawn combination of shape and rate 

# subsample to a reasonable number of draws
set.seed(8)
draws_sampled <- sample(1:length(original_results$sims.list$shape_flight), 
                        size = 10000)

results_shape_rate <- map(draws_sampled, #
    function(index){
      res <- rgamma(n = 1000, 
             shape = original_results$sims.list$shape_flight[index],
             rate = original_results$sims.list$rate_flight[index]) %>% 
        density(n = 200, from = 0, to = 1)
      
      tibble(x = res$x, y = res$y) %>% 
        return()
    }, .progress = TRUE) %>% 
  bind_rows()

results_shape_rate$x <- results_shape_rate$x*2183.475

plot_shape_rate <- results_shape_rate %>% 
  ggplot(aes(x = x, y = y)) +
  stat_lineribbon(.width = c(.95, .8, .5)) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill= "Credible intervals") +
  scale_fill_manual(values = rev(c("#636363", "#bdbdbd", "#f0f0f0"))) +
  #scale_fill_brewer() + 
  theme(legend.position="bottom") +
  scale_x_continuous(label = scales::label_number(suffix = "m"))


ggsave(plot = plot_shape_rate, 
       filename = here("graph_results", "plot_shape_rate_new.png"),
       width = 7/1.5,
       height = 5/1.5)

# version that shows the minimum NEXRAD altitude
plot_shape_rate_nexrad <- plot_shape_rate +
  geom_vline(xintercept = 155, color = "red", linetype = 2, linewidth = 0.5, alpha = 0.5) +
  annotate(geom = "text", x = 400, y = 4.7, label = "Minimum NEXRAD\ndetection altitude", 
           color = "red", size = 2)

ggsave(plot = plot_shape_rate_nexrad, 
       filename = here("graph_results", "plot_shape_rate_nexrad.png"),
       width = 7/1.5,
       height = 5/1.5)

# version that shows the altitudes of potential airspace obstacles

icon_tibble <- tibble(x = 2000, y = c(4.25, 2.75, 1.25), 
                      image = c(here("graph_results", "building.png"), 
                                here("graph_results", "wind-turbine.png"),
                                here("graph_results", "signal.png")),
                      col = c("#f1724e", "#f9956e", "#ffcda0"))

collision_plot <-  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 47, ymin = -1, ymax = 6), alpha = 0.4, fill = "#cb0824") +
  geom_rect(aes(xmin = 0, xmax = 244, ymin = -1, ymax = 6), alpha = 0.4, fill = "#fe8211") +
  geom_rect(aes(xmin = 0, xmax = 163.8, ymin = -1, ymax = 6), alpha = 0.4, fill = "#f04122") + #32.2
  stat_lineribbon(data = results_shape_rate, mapping = aes(x = x, y = y), .width = c(.95, .8, .5), 
                  alpha = 0.8) +
  theme_bw() +
  labs(x = "Flight altitude", y = "Density", fill= "Credible intervals") +
  scale_fill_manual(values = rev(c("#636363", "#bdbdbd", "#f0f0f0"))) +
  #scale_fill_brewer() + 
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0, 5)) +
  scale_x_continuous(label = scales::label_number(suffix = "m")) +
  geom_image(mapping = aes(x = x, y = y, image = image), 
             data = icon_tibble, 
             color = c("#ef5a30", "#f87c4c", "#ffb776"),
             size = 0.2) +
  guides(color = "none")


ggsave(plot = collision_plot, 
       filename = here("graph_results", "collision_plot.png"),
       width = 7/1.5,
       height = 5/1.5)
