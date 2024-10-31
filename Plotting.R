
library(readr)
library(tidyverse)
library(viridis)
library(pheatmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

## LOAD DATA ----

# correlation matrix
cor_matrix <- read_csv("data/out/cor_matrix.csv")

# covariance matrix
cov_matrix <- read_csv("data/out/cov_matrix.csv")

# correlation matrix for one zone (within-zone-correlation)
cor_matrix_zone_25 <- read_csv("data/out/cor_matrix_zone_25.csv")

# general cells data for NEZ
cells_NEZ_clusters_PLOT <- data.frame(readRDS("data/processed/cells_NEZ_clusters.rds"))

# output files for Model 2
model_2_out_pareto <- read_csv("AMPL/Model 2/out/model_2_out_pareto.csv") %>% mutate(p = round(p, 2))
model_2_out_weights <- read_csv("AMPL/Model 2/out/model_2_out_weights.csv") %>% mutate(p = round(p, 2))

# output files for Model 3
model_3_out_pareto <- read_csv("AMPL/Model 3/out/model_3_out_pareto.csv") %>% mutate(p = round(p, 2))
model_3_out_weights <- read_csv("AMPL/Model 3/out/model_3_out_weights.csv") %>% mutate(p = round(p, 2))

# output files for Model 4
model_4_out_pareto <- read_csv("AMPL/Model 4/out/model_4_out_pareto.csv") %>% mutate(p = round(p, 2))
model_4_out_weights <- read_csv("AMPL/Model 4/out/model_4_out_weights.csv") %>% mutate(p = round(p, 2))


norway_outline <- norway_outline <- ne_countries(scale = "medium", country = "Norway", returnclass = "sf")



## All Zones ----

cluster_centroids <- cells_NEZ_clusters_PLOT %>% 
  select(X, Y, cluster_KM) %>% 
  group_by(cluster_KM) %>% 
  summarize(X = floor(mean(X)),
            Y = floor(mean(Y)))

cells_NEZ_clusters_PLOT %>% 
  ggplot() +
  geom_raster(aes(x = lat, y = lon, fill = as.factor(cluster_KM))) +
  theme_minimal() +
  guides(fill = "none") +
  coord_fixed() +
  scale_fill_manual(values = sample(viridis_pal(option = "turbo")(length(unique(cells_NEZ_clusters_PLOT$cluster_KM))))) +
  xlab("X Coordinate") + ylab("Y Coordinate") +
  #geom_text(data = cluster_centroids, aes(x = lat, y = lon, label = cluster_KM), color = "black", size = 4) +
  geom_sf(data = norway_outline, color = "black", fill = NA, size = 0.5)




## Selected Zones ----
# MODEL 2
pMVP <- 0.8
M <- 20

M2_selected_zones <- model_2_out_weights %>% 
  filter(maxfarms == M, p == pMVP) %>% pull(zone)

M2_cluster_centroids_selected <- cluster_centroids %>%
  filter(cluster_KM %in% M2_selected_zones)

cells_NEZ_clusters_PLOT %>% 
  mutate(selected = ifelse(cluster_KM %in% M2_selected_zones, 1, 0)) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_raster(aes(fill = as.factor(selected))) +
  theme_minimal() +
  guides(fill = "none") +
  coord_fixed() +
  geom_text(data = M2_cluster_centroids_selected, aes(label = cluster_KM), color = "black", size = 4) +
  labs(title = "Selected Zones Model 2",
       subtitle = paste0(pMVP, " * MVP + ", 1-pMVP, " * MCF , Maxfarms = ", M))



## Selected Cells ----
# MODEL 3
pMVP <- 0.5
M <- 10

M3_selected_cells <- model_3_out_weights %>% 
  filter(maxfarms == M, p == pMVP) %>% pull(cell)

M3_cells_coord_selected <- cells_NEZ_clusters_PLOT %>%
  select(ID, X, Y) %>% 
  filter(ID %in% M3_selected_cells)

M3_selected_zones <- model_3_out_weights %>% 
  filter(maxfarms == M, p == pMVP) %>% pull(zone)

M3_cluster_centroids_selected <- cluster_centroids %>%
  filter(cluster_KM %in% M3_selected_zones)

cells_NEZ_clusters_PLOT %>% 
  mutate(selected = ifelse(cluster_KM %in% M3_selected_zones, 1, 0)) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_raster(aes(fill = as.factor(selected))) +
  theme_minimal() +
  guides(fill = "none") +
  coord_fixed() +
  geom_text(data = M3_cluster_centroids_selected, aes(label = cluster_KM), color = "black", size = 4) +
  geom_point(data = M3_cells_coord_selected, aes(x = X, y = Y), color = "red", size = 2) +
  labs(title = "Selected Zones Model 3",
       subtitle = paste0(pMVP, " * MVP + ", 1-pMVP, " * MCF , Maxfarms = ", M))




## Selected Cells (multiple) ----
# MODEL 4

pMVP <- 0.75
M <- 10

M4_selected_cells <- model_4_out_weights %>% 
  filter(maxfarms == M, p == pMVP) %>% pull(cell)

M4_cells_coord_selected <- cells_NEZ_clusters_PLOT %>%
  select(ID, X, Y) %>% 
  filter(ID %in% M4_selected_cells)

M4_selected_zones <- model_4_out_weights %>% 
  filter(maxfarms == M, p == pMVP) %>% pull(zone)

M4_cluster_centroids_selected <- cluster_centroids %>%
  filter(cluster_KM %in% M4_selected_zones)

plot_M4 <- cells_NEZ_clusters_PLOT %>% 
  mutate(selected = ifelse(cluster_KM %in% M4_selected_zones, 1, 0)) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_raster(aes(fill = as.factor(selected))) +
  theme_minimal() +
  guides(fill = "none") +
  coord_fixed() +
  geom_text(data = M4_cluster_centroids_selected, aes(label = cluster_KM), color = "black", size = 4) +
  #geom_point(data = M4_cells_coord_selected, aes(x = X, y = Y), color = "red", size = 2) +
  labs(title = "Selected Zones Model 4",
       subtitle = paste0(pMVP, " * MVP + ", 1-pMVP, " * MCF , Maxfarms = ", M))

plotname <- paste0("plots/plot_Model4_", M, "Farms_", gsub("\\.", "-", pMVP), "xMVP.png")

ggsave(plot_M4, file = plotname, height = 1149/100,width = 652/100)


## Neighbors one Zone ----

pMVP <- 0.5
M <- 10

model_4_out_weights %>% filter(maxfarms == M, p == pMVP) %>% 
  pull(zone) %>% unique()

zones_to_display <- c(25)

M4_ZONE_selected_cells <- model_4_out_weights %>% 
  filter(maxfarms == M, p == pMVP, zone %in% c(zones_to_display)) %>%
  pull(cell)
  
M4_ZONE_all_cells <- cells_NEZ_clusters_PLOT %>%
  filter(cluster_KM %in% c(zones_to_display)) %>% 
  select(ID, X, Y) %>% 
  mutate(selected = ifelse(ID %in% M4_ZONE_selected_cells, 1, 0))

M4_ZONE_weights <- cluster_centroids %>% 
  filter(cluster_KM %in% zones_to_display) %>% 
  left_join(y = model_4_out_weights, by = c("cluster_KM" = "zone")) %>% 
  group_by(cluster_KM, maxfarms, X, Y) %>% 
  summarise(weight_z = mean(weight_z)) %>% 
  filter(maxfarms == M)

plot_M4_OneZone <- M4_ZONE_all_cells %>% 
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = as.factor(selected))) +
  theme_minimal() +
  guides(fill = "none") +
  coord_fixed() +
  geom_text(aes(x = X, y = Y, label = paste0("zone weight = ", round(weight_z, 2))), size = 4, data = M4_ZONE_weights) +
  labs(title = paste0("Selected Cells in Zone ", zones_to_display[1]))

plotname <- paste0("plots/plot_Model4_OneZone_", M, "Farms_", gsub("\\.", "-", pMVP), "xMVP.png")

ggsave(plot_M4_OneZone, file = plotname, height = 1149/200,width = 652/200)



## Pareto Front Model 2 ----

M <- 10

model_2_out_pareto %>% 
  filter(maxfarms == M) %>% 
  ggplot() +
  geom_line(aes(x = MeanSD, y = -MeanCF)) +
  geom_point(aes(x = 0.205, y = 0.589), color = "red", size = 4) +
  geom_text(aes(x = 0.25, y = 0.589, label = "Optimal Solution from Holleland et al."), color = "black")




## Pareto Front Model 3 ----

M <- 10

model_3_out_pareto %>% 
  filter(maxfarms == M) %>% 
  ggplot(aes(x = MeanSD, y = -MeanCF)) +
  geom_line()




## Pareto Front Model 4 ----

M <- 10

model_4_out_pareto %>% 
  filter(maxfarms == M) %>% 
  ggplot(aes(x = MeanSD, y = -MeanCF)) +
  geom_line()
  



## Correlation Matrix Zones ----

ggplot(cor_matrix, aes(x = z, y = y, fill = cor_value)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(x = "Zone ID", y = "Zone ID", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Correlation Matrix Cells ----
# Within one Zone

cor_matrix_zone_25 %>% 
  ggplot(aes(x = z, y = y, fill = cor_value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal()
  



## Wind Power (CF) Map ----

plot_CF_map <- cells_NEZ_clusters_PLOT %>% 
  ggplot(aes(x = X, y = Y, fill = CF_cells^6)) +
  geom_raster() +
  coord_fixed() +
  scale_fill_gradientn(colors = c("lightyellow", "orange", "red", "darkred")) +
  guides(fill = "none")

ggsave(plot_CF_map, file = "plots/plot_CF_map.png", height = 1149/200,width = 652/200)
