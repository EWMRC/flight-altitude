library(tidyverse)
library(jagsUI)
library(tidybayes)
library(ggpubr)
library(here)
library(ggimage)

original_results <- readRDS(here("modeling", "results", "lnorm_original.rds"))

draws_sampled <- 1:length(rstan::extract(original_results, "mu_alt")[[1]])

results_shape_rate <- map(draws_sampled, #
    function(index){
      res <- rlnorm(n = 10000, 
                    meanlog = rstan::extract(original_results, "mu_alt")[[1]][index],
                    sdlog = rstan::extract(original_results, "sigma_alt")[[1]][index]) %>% 
        density(n = 200, from = 0, to = 1)
      
      tibble(x = res$x, y = res$y) %>% 
        return()
    }, .progress = TRUE) %>% 
  bind_rows()

results_shape_rate$x <- results_shape_rate$x*2183.475

results_shape_rate$y <- results_shape_rate$y/max(results_shape_rate$y)

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
  labs(x = "Flight altitude (m)", y = "Probability density", fill= "Credible intervals") +
  scale_fill_manual(values = rev(c("#636363", "#bdbdbd", "#f0f0f0"))) +
  #scale_fill_brewer() + 
  #theme(legend.position="bottom") +
  guides(fill="none") +
  coord_cartesian(ylim=c(0, 0.8)) +
  #scale_x_continuous(label = scales::label_number(suffix = "m")) +
  geom_image(mapping = aes(x = x, y = y, image = image), 
             data = icon_tibble, 
             color = c("#f03b20", "#feb24c", "#f6c700"), #"#ef5a30", "#f87c4c", "#ffd523"
             size = 0.2,
             alpha = 0.8) +
  guides(color = "none") +
  ggtitle("Woodcock migratory flight altitudes frequently \n coincide with airspace obstacles") +
  annotate("text", x = 1445, y = 0.55, label = "Low-rise buildings", color = "#f03b20") +
  annotate("text", x = 1525, y = 0.35, label = "Wind turbines", color = "#feb24c") +
  annotate("text", x = 1340, y = 0.15, label = "Communication towers", color = "#f6c700")
  

#collision_plot

ggsave(plot = collision_plot, 
       filename = here("graph_results", "figures", "graphical_abstract.png"),
       width = 7/1.5,
       height = 5/1.5,
       dpi = 600)
