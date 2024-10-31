

library(readr)
library(tidyverse)
library(viridis)

out_pareto_cells <- read_csv("AMPL/Multi Cells/out/out_pareto.csv") %>% mutate(p = round(p, 2))
out_pareto_zones <- read_csv("AMPL/Multi Base 2/out/out_pareto.csv") %>% mutate(p = round(p, 2))
out_weights_cells <- read_csv("AMPL/Multi Cells/out/out_weights.csv") %>% mutate(p = round(p, 2))
out_weights_zones <- read_csv("AMPL/Multi Base 2/out/out_weights.csv") %>% mutate(p = round(p, 2))
out_weights_nb <- read_csv("AMPL/Multi Cells Neighbors/out/out_weights.csv")


out_pareto_combined <-
  rbind(cbind(Model = "Zones", out_pareto_zones), cbind(Model = "Cells", out_pareto_cells))


out_pareto_zones %>% 
  filter(maxfarms == 5) %>% 
  ggplot() +
  geom_line(aes(x = MeanSD, y = -MeanCF))

out_pareto_combined %>% 
  filter(maxfarms == 5) %>% 
  ggplot(aes(x = MeanSD, y = -MeanCF, color = Model)) +
  geom_line(size = 1)

out_weights_cells %>% filter(maxfarms == 5, p == 0.5)


# the problem with Kmeans is that the clusters are geographically not
# necessarily connected, for example with seed 123, cluster 33 is spread out
# putting higher weights on the coordinates can decrease this problem
cells_NEZ_clusters <- readRDS("data/processed/cells_NEZ_clusters.rds")

cluster_centroids <- cells_NEZ_clusters %>% 
  data.frame() %>% 
  select(X, Y, cluster_KM) %>% 
  group_by(cluster_KM) %>% 
  summarize(X = floor(mean(X)),
            Y = floor(mean(Y)))

plot1 <- data.frame(cells_NEZ_clusters) %>% 
  ggplot(aes(x = X, y = Y, fill = as.factor(cluster_KM))) +
  geom_raster() +
  theme_minimal() +
  guides(fill = "none") +
  coord_fixed() +
  scale_fill_manual(values = sample(viridis_pal(option = "turbo")(length(unique(cells_NEZ_clusters[, "cluster_KM"]))))) +
  geom_text(data = cluster_centroids, aes(label = cluster_KM), color = "black", size = 6)

ggsave(plot1, file = "plots/clusters_100_KM_weights20_inclwind.png", height = 1149/50,width = 652/50)

# plot selected cells for a certain scenario
selected_zones <- out_weights %>% 
  filter(maxfarms == 10, p == 0.6) %>% pull(zone)

cluster_centroids_selected <- cluster_centroids %>% 
  filter(cluster_KM %in% selected_zones)

data.frame(cells_NEZ_clusters) %>% 
  mutate(selected = ifelse(cluster_KM %in% selected_zones, 1, 0)) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_raster(aes(fill = as.factor(selected))) +
  theme_minimal() +
  guides(fill = "none") +
  coord_fixed() +
  geom_text(data = cluster_centroids_selected, aes(label = cluster_KM), color = "black", size = 4)
