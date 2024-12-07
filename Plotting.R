###############################################################################-
# This file is to visualize all results shown in the thesis (and more)
###############################################################################-

library(readr)
library(tidyverse)
library(viridis)
library(pheatmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)  # for combining plots
library(ggbeeswarm) # cool scatter plots
library(ggforce)
library(gridExtra) # plot grids
library(concaveman) # for drawing polygons
library(readxl)


source("Functions.R")


## RULES FOR PLOTTING ----
# naming: plot_....png
# ggsave height: 10
# ggsave width: adjust accordingly
# ggsave dpi: 300
# ggsave units: "in"


## LOAD DATA ----

# correlation matrix
cor_matrix <- read_csv("data/out/cor_matrix.csv")

# covariance matrix
cov_matrix <- read_csv("data/out/cov_matrix.csv")

# correlation matrix for one zone (within-zone-correlation)
cor_matrix_zone_25 <- read_csv("data/out/cor_matrix_zone_25.csv")

# general cells data for NEZ
cells_NEZ_clusters_df <- data.frame(readRDS("data/processed/cells_NEZ_clusters.rds"))

# output files for Model 2
model_2_out_pareto <- read_csv("AMPL/Model 2 (MO, zone, included)/out/model_2_out_pareto.csv") %>% mutate(p = round(p, 2))
model_2_out_weights <- read_csv("AMPL/Model 2 (MO, zone, included)/out/model_2_out_weights.csv") %>% mutate(p = round(p, 2))

# output files for Model 3
model_3_out_pareto <- read_csv("AMPL/Model 3 (not included)/out/model_3_out_pareto.csv") %>% mutate(p = round(p, 2))
model_3_out_weights <- read_csv("AMPL/Model 3 (not included)/out/model_3_out_weights.csv") %>% mutate(p = round(p, 2))

# output files for Model 4
# comment in/out depending on WPSS scenario
model_4_out_pareto <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_pareto.csv") %>% mutate(p = round(p, 2))
model_4_out_weights <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_weights.csv") %>% mutate(p = round(p, 2))
# Fisherman
model_4_out_pareto <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Fisherman/model_4_out_pareto_fish.csv") %>% mutate(p = round(p, 2))
model_4_out_weights <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Fisherman/model_4_out_weights_fish.csv") %>% mutate(p = round(p, 2))
# Ecologist
model_4_out_pareto <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Ecologist/model_4_out_pareto_ecol.csv") %>% mutate(p = round(p, 2))
model_4_out_weights <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Ecologist/model_4_out_weights_ecol.csv") %>% mutate(p = round(p, 2))
# Investor
model_4_out_pareto <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Investor/model_4_out_pareto_inv.csv") %>% mutate(p = round(p, 2))
model_4_out_weights <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Investor/model_4_out_weights_inv.csv") %>% mutate(p = round(p, 2))
# All WPSS
model_4_out_pareto <- read_csv("AMPL/Model 4 (MO, cell, included)/out/AllWPSS/model_4_out_pareto_All.csv") %>% mutate(p = round(p, 2))
model_4_out_weights <- read_csv("AMPL/Model 4 (MO, cell, included)/out/AllWPSS/model_4_out_weights_All.csv") %>% mutate(p = round(p, 2))

# actual pareto front for model 4
model_4_out_pareto_actual <- read_csv("data/out/model_4_out_pareto_actual.csv") %>% mutate(p = round(p, 2))




# Statistics for correlations within all zones
cor_within_all_zones <- read_csv("data/out/cor_within_all_zones.csv")

# load NVE locations
NVE <- read_excel("data/NVE/NVE_20_locations.xlsx")


# norway_outline <- norway_outline <- ne_countries(scale = "medium", country = "Norway", returnclass = "sf")



## All Zones ----

cluster_centroids <- cells_NEZ_clusters_df %>% 
  select(X, Y, cluster_KM) %>% 
  group_by(cluster_KM) %>% 
  summarize(X = floor(mean(X)),
            Y = floor(mean(Y)))

plot_all_zones <- cells_NEZ_clusters_df %>% 
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = as.factor(cluster_KM))) +
  theme_minimal() +
  guides(fill = "none") +
  coord_fixed() +
  scale_fill_manual(values = sample(viridis_pal(option = "turbo")(length(unique(cells_NEZ_clusters_df$cluster_KM))))) +
  xlab("X Coordinate") + ylab("Y Coordinate") +
  geom_text(aes(x = 100, y = 400, label = "A"), color = "black", size = 6) +
  geom_text(data = cluster_centroids, aes(x = X, y = Y, label = cluster_KM), color = "black", size = 3)
  #geom_sf(data = norway_outline, color = "black", fill = NA, size = 0.5)

ggsave(plot_all_zones, file = "Plots/plot_150zones.png", height = 10)




### All Zones (Corr Groups) ----

mean_distNW_zones <- cells_NEZ_clusters_df %>% 
  group_by(cluster_KM) %>% 
  summarise(mean_dist_NW = mean(MinDistNW_NEZ)) %>% 
  ungroup() %>% 
  mutate(DistNW_GROUP = factor(
    ifelse(mean_dist_NW <= 15, "within 15 km",
           ifelse(mean_dist_NW <= 50, "15 to 50 km",
                  ifelse(mean_dist_NW <= 100, "50 to 100 km", "outside 100 km"))),
    levels = c("within 15 km", "15 to 50 km", "50 to 100 km", "outside 100 km")
  ))

plot_all_zones_2 <- cells_NEZ_clusters_df %>% 
  left_join(x = ., y = mean_distNW_zones) %>% 
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = DistNW_GROUP)) +
  theme_minimal() +
  coord_fixed() +
  xlab("X Coordinate") + ylab("Y Coordinate") +
  labs(fill = "Mean Distance To Norway") +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  #geom_text(aes(x = 100, y = 400, label = "B"), color = "black", size = 6) +
  geom_text(data = cluster_centroids, aes(x = X, y = Y, label = cluster_KM), color = "black", size = 3)

ggsave(plot_all_zones_2, file = "Plots/plot_150zones_ByDistance.png", height = 10, width = 8, dpi = 300, units = "in")



### All Zones (Combined) ----

plot_all_zones_grid <- plot_grid(plot_all_zones, plot_all_zones_2, ncol = 2)

ggsave("Plots/plot_150zones_grid.png", plot = plot_all_zones_grid, width = 14, height = 6, dpi = 300)






## Selected Zones ----
# MODEL 2
pMVP <- 0.5
M <- 20

M2_selected_zones <- model_2_out_weights %>% 
  filter(maxfarms == M, p == pMVP) %>% pull(zone)

M2_cluster_centroids_selected <- cluster_centroids %>%
  filter(cluster_KM %in% M2_selected_zones)

cells_NEZ_clusters_df %>% 
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

M3_cells_coord_selected <- cells_NEZ_clusters_df %>%
  select(ID, X, Y) %>% 
  filter(ID %in% M3_selected_cells)

M3_selected_zones <- model_3_out_weights %>% 
  filter(maxfarms == M, p == pMVP) %>% pull(zone)

M3_cluster_centroids_selected <- cluster_centroids %>%
  filter(cluster_KM %in% M3_selected_zones)

cells_NEZ_clusters_df %>% 
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

pMVP <- 0.5
M <- 20

M4_selected_cells <- model_4_out_weights %>% 
  filter(maxfarms == M, p == pMVP) %>% pull(cell)

M4_cells_coord_selected <- cells_NEZ_clusters_df %>%
  select(ID, X, Y) %>% 
  filter(ID %in% M4_selected_cells)

M4_selected_zones <- model_4_out_weights %>% 
  filter(maxfarms == M, p == pMVP) %>% pull(zone)

M4_cluster_centroids_selected <- cluster_centroids %>%
  filter(cluster_KM %in% M4_selected_zones)

plot_M4 <- cells_NEZ_clusters_df %>% 
  mutate(selected = ifelse(cluster_KM %in% M4_selected_zones, 1, 0)) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_raster(aes(fill = as.factor(selected))) +
  scale_fill_manual(values = c("lightgrey", "#7393B3")) +
  theme_minimal() +
  guides(fill = "none") +
  coord_fixed() +
  #geom_text(data = M4_cluster_centroids_selected, aes(label = cluster_KM), color = "black", size = 4) +
  #geom_point(data = M4_cells_coord_selected, aes(x = X, y = Y), color = "red", size = 2) +
  labs(title = "Selected Zones Model 4",
       subtitle = paste0(pMVP, " * MVP + ", 1-pMVP, " * MCF , Maxfarms = ", M))

plotname <- paste0("plots/plot_Model4_", M, "Farms_", gsub("\\.", "-", pMVP), "xMVP.png")

ggsave(plot_M4, file = plotname, height = 1149/100,width = 652/100)


## Selected one Zone ----

pMVP <- 0.8
M <- 20

model_4_out_weights %>% filter(maxfarms == M, p == pMVP) %>% 
  pull(zone) %>% unique()

zones_to_display <- c(16)

M4_ZONE_selected_cells <- model_4_out_weights %>% 
  filter(maxfarms == M, p == pMVP, zone %in% c(zones_to_display)) %>%
  pull(cell)
  
M4_ZONE_all_cells <- cells_NEZ_clusters_df %>%
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
  geom_text(aes(x = X, y = Y, label = paste0("zone weight = ", round(weight_z, 3))), size = 4, data = M4_ZONE_weights) +
  labs(title = paste0("Selected Cells in Zone ", zones_to_display[1]))

plotname <- paste0("plots/plot_Model4_OneZone_", M, "Farms_", gsub("\\.", "-", pMVP), "xMVP.png")

ggsave(plot_M4_OneZone, file = plotname, height = 1149/200,width = 652/200)






## Polygons ----

pMVP = 0.5
M = 20

# get all polygons
polygons <- get_polygons(pMVP = pMVP, M = M)

# get all zone IDs and centroids
zones <- model_4_out_weights %>% 
  filter(p == pMVP, maxfarms == M) %>% 
  pull(zone) %>% 
  unique()

# get the centroids of selected cells
cluster_centroids_selected <- model_4_out_weights %>% 
  filter(p == pMVP, maxfarms == M) %>% 
  left_join(x = ., y = cells_NEZ_clusters_df, by = c("cell" = "ID")) %>% 
  group_by(maxfarms, p, zone) %>% 
  summarise(X = mean(X), Y = mean(Y))

# the arrows are from the centroid to the right side of the plot
zones_arrow_df <- cluster_centroids_selected %>% 
  filter(zone %in% zones) %>% 
  arrange(Y) %>% 
  mutate(X_start = X,
         Y_start = Y,
         X_end = 700,
         Y_end = row_number() * (800 / length(zones)) + 350)

plot_M4_polygons <- ggplot(cells_NEZ_clusters_df) +
  geom_raster(aes(x = X, y = Y), show.legend = FALSE, fill = "lightgrey") +
  geom_sf(data = polygons, fill = "#7393B3", color = "black", size = 1) +
  # Add arrows with geom_segment
  #geom_segment(data = zones_arrow_df, aes(x = X_start, y = Y_start, xend = X_end, yend = Y_end), color = "black", linewidth = 0.2) +
  # Add text labels for the zone IDs
  #geom_text(data = zones_arrow_df, aes(x = X_end, y = Y_end, label = zone), hjust = 0, vjust = 0.5, size = 5) +
  #geom_text(aes(x = 100, y = 400, label = "A"), size = 8) + # change this depending on in which grid you use it
  #xlim(c(0, 800)) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())

ggsave(filename = "Plots/plot_M4_polygons_p05_M20_inv_ToCombine.png", plot = plot_M4_polygons, height = 10, dpi = 300, units = "in")


# With different personas without numbers in one plot grid
plot_M4_polygons_fish_img <- grid::rasterGrob(png::readPNG("Plots/plot_M4_polygons_p05_M20_fish_NoNumbers.png"), interpolate = TRUE)
plot_M4_polygons_ecol_img <- grid::rasterGrob(png::readPNG("Plots/plot_M4_polygons_p05_M20_ecol_NoNumbers.png"), interpolate = TRUE)
plot_M4_polygons_inv_img <- grid::rasterGrob(png::readPNG("Plots/plot_M4_polygons_p05_M20_inv_NoNumbers.png"), interpolate = TRUE)

plot_grid_polygon_WPSS_sensitivity <-
  grid.arrange(plot_M4_polygons_fish_img,
               plot_M4_polygons_ecol_img,
               plot_M4_polygons_inv_img,
               ncol = 3)

ggsave(filename = "Plots/plot_grid_polygon_WPSS_sensitivity.png", plot = plot_grid_polygon_WPSS_sensitivity, width = 18, height = 10, dpi = 300, units = "in")






## Polygons One Zone ----

pMVP = 0.75
M = 20
zone_to_display = 104

# get all polygons and select the zone that should be displayed
polygons <- get_polygons(pMVP = pMVP, M = M)
polygon_zone <- polygons %>% filter(zones == zone_to_display)

# get all selected cells
M4_ZONE_selected_cells <- model_4_out_weights %>% 
  filter(maxfarms == M, p == pMVP, zone == zone_to_display) %>%
  pull(cell)

# plot with only selected cells
plot_M4_OneZone <- cells_NEZ_clusters_df %>% 
  filter(cluster_KM == zone_to_display) %>% 
  mutate(selected = ifelse(ID %in% M4_ZONE_selected_cells, 1, 0)) %>% #M4_ZONE_selected_cells
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = as.factor(selected))) +
  #geom_text(aes(x = 225, y = 535, label = "B"), size = 6) + # change this depending on in which grid you use it
  scale_fill_manual(values = c("lightgrey", "#7393B3")) +
  coord_fixed() +
  theme_minimal() +
  guides(fill = "none") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

ggsave(filename = "Plots/plot_M4_Zone41.png", plot = plot_M4_OneZone, height = 10, dpi = 300, units = "in")


# plot with Polygon on top
plot_M4_OneZone_polygon <- cells_NEZ_clusters_df %>% 
  filter(cluster_KM == zone_to_display) %>% 
  mutate(selected = ifelse(ID %in% M4_ZONE_selected_cells, 1, 0)) %>% 
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = as.factor(selected))) +
  scale_fill_manual(values = c("lightgrey", "#7393B3")) +
  geom_sf(data = polygon_zone, fill = "#7393B3", color = "black", size = 1, alpha = 0.6) +
  #geom_text(aes(x = 225, y = 535, label = "C"), size = 6) + # change this depending on in which grid you use it
  coord_sf() +
  theme_minimal() +
  #labs(fill = "Selected") + # comment in/out depending on which plot grid 
  guides(fill = "none") + # comment in/out depending on which plot grid 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

ggsave(filename = "Plots/plot_M4_Zone41_Polygon.png", plot = plot_M4_OneZone_polygon, height = 10, dpi = 300, units = "in")

# combine one zone cell with one zone polygon
plot_M4_OneZone_grid <- plot_grid(plot_M4_OneZone, plot_M4_OneZone_polygon, ncol = 2)

plotname <- paste0("Plots/plot_Model4_OneZone_", M, "Farms_", gsub("\\.", "-", pMVP), "xMVP_AllWPSS.png")

ggsave(filename = plotname, plot = plot_M4_OneZone_grid, height = 10, width = 14, dpi = 300, units = "in")






## Process To Polygons ----
# get all polygons and select the zone that should be displayed
polygons <- get_polygons(pMVP = 0.5, M = 20)
polygon_zone <- polygons %>% filter(zones == 41)

plot_M4_zones <- cells_NEZ_clusters_df %>% 
  mutate(selected = ifelse(cluster_KM %in% zones, 1, 0)) %>% 
  ggplot(aes(x = X, y = Y)) +
  geom_raster(aes(fill = as.factor(selected))) +
  #geom_text(aes(x = 100, y = 400, label = "A"), size = 8) + # change this depending on in which grid you use it
  scale_fill_manual(values = c("lightgrey", "#7393B3")) +
  theme_minimal() +
  guides(fill = "none") +
  coord_fixed() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

ggsave(filename = "Plots/plot_M4_ProcessToPolygons_A.png", plot = plot_M4_zones, height = 10, width = 5.19, dpi = 300, units = "in")


plot_M4_polygons_noArrows <- ggplot(cells_NEZ_clusters_df) +
  geom_raster(aes(x = X, y = Y), show.legend = FALSE, fill = "lightgrey") +
  geom_sf(data = polygons, fill = "#7393B3", color = "black", size = 1) +
  #geom_text(aes(x = 100, y = 400, label = "D"), size = 8) + # change this depending on in which grid you use it
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

ggsave(filename = "Plots/plot_M4_ProcessToPolygons_D.png", plot = plot_M4_polygons_noArrows, height = 10, width = 5.19, dpi = 300, units = "in")

# plot on zone with only selected cells
M4_ZONE_selected_cells <- model_4_out_weights %>% 
  filter(maxfarms == M, p == pMVP, zone %in% c(41)) %>%
  pull(cell)

plot_M4_OneZone <- cells_NEZ_clusters_df %>% 
  filter(cluster_KM == 41) %>% 
  mutate(selected = ifelse(ID %in% M4_ZONE_selected_cells, 1, 0)) %>% #M4_ZONE_selected_cells
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = as.factor(selected))) +
  #geom_text(aes(x = 225, y = 535, label = "B"), size = 8) + # change this depending on in which grid you use it
  scale_fill_manual(values = c("lightgrey", "#7393B3")) +
  coord_fixed() +
  theme_minimal() +
  guides(fill = "none") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

ggsave(filename = "Plots/plot_M4_ProcessToPolygons_B.png", plot = plot_M4_OneZone, height = 10, width = 5.19, dpi = 300, units = "in")

# plot one zone with Polygon on top
plot_M4_OneZone_polygon <- cells_NEZ_clusters_df %>% 
  filter(cluster_KM == 41) %>% 
  mutate(selected = ifelse(ID %in% M4_ZONE_selected_cells, 1, 0)) %>% 
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = as.factor(selected))) +
  scale_fill_manual(values = c("lightgrey", "#7393B3")) +
  geom_sf(data = polygon_zone, fill = "#7393B3", color = "black", size = 1, alpha = 0.6) +
  #geom_text(aes(x = 225, y = 535, label = "C"), size = 8) + # change this depending on in which grid you use it
  coord_sf() +
  theme_minimal() +
  guides(fill = "none") + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16),
        panel.grid = element_blank())

ggsave(filename = "Plots/plot_M4_ProcessToPolygons_C.png", plot = plot_M4_OneZone_polygon, height = 10, width = 5.19, dpi = 300, units = "in")

# load all four plots as images
plot_M4_zones_img <- grid::rasterGrob(png::readPNG("Plots/plot_M4_zones.png"), interpolate = TRUE)
plot_M4_OneZone_img <- grid::rasterGrob(png::readPNG("Plots/plot_M4_Zone41.png"), interpolate = TRUE)
plot_M4_OneZone_polygon_img <- grid::rasterGrob(png::readPNG("Plots/plot_M4_Zone41_polygon.png"), interpolate = TRUE)
plot_M4_polygons_noArrows_img <- grid::rasterGrob(png::readPNG("Plots/plot_M4_Polygons.png"), interpolate = TRUE)

# Arrange the plots in a grid without further resizing
plot_processToPolygons <- grid.arrange(plot_M4_zones_img,
             plot_M4_OneZone_img,
             plot_M4_OneZone_polygon_img,
             plot_M4_polygons_noArrows_img,
             ncol = 4)

ggsave(filename = "Plots/plot_processToPolygons.png", plot = plot_processToPolygons, height = 10, width = 22, dpi = 300)




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

M <- 20

points_df <- model_4_out_pareto %>% 
  filter(maxfarms == M, p %in%  c(0.25, 0.5, 0.75)) 

plot_M4_20Farms_pareto <- model_4_out_pareto %>% 
  filter(maxfarms == M) %>% 
  ggplot() +
  geom_line(aes(x = MeanSD, y = -MeanCF)) +
  geom_point(aes(x = MeanSD, y = -MeanCF), data = points_df, size = 6, color = "blue") +
  geom_text(aes(x = MeanSD + c(-0.03, 0.03, 0.03), y = -MeanCF,
                label = c("CASE A2: 25/75 Weighting of Variance/CF",
                          "CASE A1: 50/50 Weighting of Variance/CF",
                          "CASE A3: 75/25 Weighting of Variance/CF")),
            data = points_df, size = 6) +
  theme_minimal() +
  labs(y = "Mean Capacity Factor",
       x = "Mean Standard Deviation") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24))
  
ggsave(filename = "Plots/plot_M4_20Farms_pareto.png", plot = plot_M4_20Farms_pareto, height = 10, width = 15, dpi = 300, units = "in")




## Pareto Front Actual ----
model_4_out_pareto_actual %>% 
  pivot_longer(cols = c("MeanSD", "MeanSD_actual", "MeanSD_outlined"),
               values_to = "SD",
               names_to = "SDCalcType") %>% 
  mutate(CF = ifelse(SDCalcType == "MeanSD_outlined", MeanCF_outlined, MeanCF)) %>% 
  ggplot(aes(x = SD, y = CF, color = SDCalcType)) +
  geom_line() +
  theme_minimal()

model_4_out_pareto_actual %>% 
  pivot_longer(cols = c("MeanSD", "MeanSD_actual", "MeanSD_outlined"),
               values_to = "SD",
               names_to = "SDCalcType") %>% 
  mutate(CF = ifelse(SDCalcType == "MeanSD_outlined", MeanCF_outlined, MeanCF)) %>% 
  filter(SDCalcType == "MeanSD_outlined")

## Correlation Matrix Zones ----

plot_corr_matrix_zones <- ggplot(cor_matrix) +
  geom_tile(aes(x = z, y = y, fill = cor_value)) +
  geom_hline(aes(yintercept = 40), color = "red") +
  geom_hline(aes(yintercept = 80), color = "red") +
  geom_vline(aes(xintercept = 40), color = "red") +
  geom_vline(aes(xintercept = 80), color = "red") +
  scale_fill_viridis_c(option = "plasma") +
  coord_fixed() +
  theme_minimal() +
  labs(x = "Zone ID", y = "Zone ID", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 16))

ggsave(filename = "Plots/plot_corr_matrix_zones.png", plot = plot_corr_matrix_zones, height = 10, width = 14, dpi = 300, units = "in")



## Correlation Matrix Cells ----
# Within one Zone

cor_matrix_zone_25 %>% 
  ggplot(aes(x = z, y = y, fill = cor_value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal()
  


## Correlation Within All Zones ----

# mean distance to Norway for each zone
mean_distNW_zones <- cells_NEZ_clusters_df %>% 
  group_by(cluster_KM) %>% 
  summarise(mean_dist_NW = mean(MinDistNW_NEZ)) %>% 
  ungroup() %>% 
  mutate(DistNW_GROUP = factor(
    ifelse(mean_dist_NW <= 15, "within 15 km",
           ifelse(mean_dist_NW <= 50, "15 to 50 km",
                  ifelse(mean_dist_NW <= 100, "50 to 100 km", "outside 100 km"))),
    levels = c("within 15 km", "15 to 50 km", "50 to 100 km", "outside 100 km")
  ))

plot_5pct_corr <- cor_within_all_zones %>% 
  left_join(x = ., y = mean_distNW_zones) %>% 
  ggplot(aes(x = DistNW_GROUP, color = DistNW_GROUP, y = cor_05)) +
  geom_beeswarm(size = 3) +
  theme_minimal() +
  labs(#title = "5th Percentile Correlation within Zones By Distance from Norway",
       x = "Mean Distance To Norway",
       y = "5th Percentile Correlation") +
  theme(
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text = element_text(size = 24)
  ) +
  guides(color = "none")

ggsave(filename = "Plots/plot_zones_5pct_correlation_scatter.png", plot = plot_5pct_corr, height = 10, width = 16, dpi = 300, units = "in")



## Wind Power (CF) Map ----

plot_CF_map <- cells_NEZ_clusters_df %>% 
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = CF_cells^6)) +
  #geom_text(aes(x = 100, y = 400, label = "A"), size = 6) +
  coord_fixed() +
  scale_fill_gradientn(colors = c("lightgrey", "lightblue", "#7393B3", "darkblue")) +
  guides(fill = "none") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

ggsave(plot_CF_map, file = "Plots/plot_CF_map.png", height = 10, dpi = 300, units = "in")



## WPSS Map ----

plot_WPSS_map <- cells_NEZ_clusters_df %>% 
  #filter(WPSS_NEZ >= 0.6386339) %>% 
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = WPSS_NEZ)) +
  #geom_text(aes(x = 100, y = 400, label = "B"), size = 6) +
  coord_fixed() +
  scale_fill_gradientn(colors = c("lightblue", "lightgreen", "darkgreen")) +
  guides(fill = "none") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

ggsave(plot_WPSS_map, file = "Plots/plot_WPSS_map.png", height = 10, dpi = 300, units = "in")




## WPSS Map Filtered ----

plot_WPSS_map_filtered <- cells_NEZ_clusters_df %>% 
  filter(WPSS_NEZ >= 0.6386339) %>% 
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = WPSS_NEZ)) +
  #geom_text(aes(x = 100, y = 400, label = "C"), size = 6) +
  coord_fixed() +
  scale_fill_gradientn(colors = c("lightblue", "lightgreen", "darkgreen")) +
  guides(fill = "none") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())

ggsave(plot_WPSS_map_filtered, file = "Plots/plot_WPSS_map_filtered.png", height = 10, dpi = 300, units = "in")



## CF + WPSS Map ----

plot_CF_WPSS <- plot_grid(plot_CF_map, plot_WPSS_map, plot_WPSS_map_filtered, ncol = 3)

ggsave(filename = "Plots/plot_CF_WPSS_map.png", plot = plot_CF_WPSS, height = 10, width = 17, dpi = 300, units = "in")





## ALL WPSS Map ----

# create longer format for all WPSS Scores of the different personas
WPSS_ALL <- cells_NEZ_clusters_df %>% 
  select(ID, X, Y, WPSS_NEZ, WPSS_fish_NEZ, WPSS_ecol_NEZ, WPSS_inv_NEZ) %>% 
  pivot_longer(cols = c("WPSS_NEZ", "WPSS_fish_NEZ", "WPSS_ecol_NEZ", "WPSS_inv_NEZ"),
               names_to = "Persona", values_to = "WPSS") %>% 
  mutate(Persona = factor(Persona, levels = c("WPSS_NEZ", "WPSS_fish_NEZ", "WPSS_ecol_NEZ", "WPSS_inv_NEZ"))) %>% 
  group_by(Persona) %>%
  mutate(WPSS_normalized = (WPSS - min(WPSS)) / (max(WPSS) - min(WPSS))) %>%
  ungroup()

# Custom labels to add as header to facetwrap
custom_labels <- c("WPSS_NEZ" = "Baseline",
                   "WPSS_fish_NEZ" = "Fisherman",
                   "WPSS_ecol_NEZ" = "Ecologist",
                   "WPSS_inv_NEZ" = "Investor")

plot_WPSS_All_map <- WPSS_ALL %>% 
  ggplot() +
  geom_raster(aes(x = X, y = Y, fill = WPSS_normalized), show.legend = F) +
  scale_fill_gradientn(colors = c("lightblue", "lightgreen", "darkgreen")) +
  coord_fixed() +
  facet_wrap(~Persona, ncol = 4, labeller = labeller(Persona = custom_labels)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(size = 20))

ggsave(filename = "Plots/plot_WPSS_All_map.png", plot = plot_WPSS_All_map, height = 10, width = 20, dpi = 300, units = "in")




## WPSS density ----

plot_WPSS_All_density <- WPSS_ALL %>% 
  ggplot() +
  geom_density(aes(x = WPSS, color = Persona), linewidth = 2) +
  scale_color_discrete(labels = custom_labels) +
  theme_minimal() +
  labs(x = "WPSS") +
  theme(axis.title = element_text(size = 24),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 24),
        axis.text = element_text(size = 24))

ggsave(filename = "Plots/plot_WPSS_All_density.png", plot = plot_WPSS_All_density, height = 10, width = 20, dpi = 300, units = "in")





## CF density ----

plot_CF_density <- cells_NEZ_clusters_df %>% 
  ggplot() +
  geom_density(aes(x = -CF_cells), linewidth = 2) +
  theme_minimal() +
  labs(x = "Capacity Factor") +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 24))

ggsave(filename = "Plots/plot_CF_density.png", plot = plot_CF_density, height = 10, width = 20, dpi = 300, units = "in")






## NVE ----

pMVP = 0.5
M = 20

# get out polygons
polygons <- get_polygons(pMVP = pMVP, M = M)

# NVE Polygons data frame
NVE_polygons <- NVE %>%
  select(name, lon, lat) %>% 
  group_by(name) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(polygon = map2(.x = data, .y = name, .f = get_polygons_NVE, NEZ_grid = cells_NEZ_clusters_df)) %>% 
  unnest(polygon) %>% 
  select(-data) %>% 
  st_as_sf()

# shapes of circles aroung the NVE regions
NVE_regions <- data.frame(
  X = c(400, 355, 270, 205, 150),   
  Y = c(470, 410, 505, 680, 1000),   
  label = c("Sønnavind (S)", "Sørvest (SV)", "Vestavind (VV)", "Nordvest (NV)", "Nordavind (N)"),
  X_label = c(420, 420, 420, 420, 420),
  Y_label = c(550, 410, 700, 850, 1000),
  a = c(35, 50, 30, 40, 120),          # Semi-major axis for each ellipse
  b = c(15, 40, 80, 100, 40),          # Semi-minor axis for each ellipse
  angle = c(pi/3, 0, pi/3.5, pi/8, pi/3) # Angle of rotation (in radians) for each ellipse
)

plot_NVE_M4_polygons <- cells_NEZ_clusters_df %>% 
  ggplot() +
  geom_raster(aes(x = X, y = Y), fill = "lightgrey", show.legend = FALSE) +
  geom_sf(data = NVE_polygons, fill = "yellow", color = "black", size = 1, alpha = 0.2) +
  geom_sf(data = polygons, fill = "green", color = "black", size = 1) + # color in other plots: #7393B3
  #geom_text(aes(x = 100, y = 400, label = "C"), size = 8) + # change this depending on in which value of p
  # Add circles/ovals with variable sizes and shapes
  geom_ellipse(data = NVE_regions, aes(x0 = X, y0 = Y, a = a, b = b, angle = angle), 
               fill = "blue", alpha = 0.05, color = "black") +
  # Add connecting lines from shape center to label
  geom_segment(data = NVE_regions, aes(x = X, y = Y, xend = X_label, yend = Y_label), 
               color = "black", linewidth = 0.5) +
  # Add text labels for the locations
  geom_text(data = NVE_regions, aes(x = X_label, y = Y_label, label = label), 
            hjust = 0, vjust = 0.5, size = 8) +
  xlim(c(0, 800)) +
  coord_sf() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# save for different values of p
ggsave(filename = "Plots/plot_NVE_M4_p05_M20.png", plot = plot_NVE_M4_polygons, height = 10, width = 10, dpi = 300, units = "in")
ggsave(filename = "Plots/plot_NVE_M4_p025_M20.png", plot = plot_NVE_M4_polygons, height = 10, dpi = 300, units = "in")  
ggsave(filename = "Plots/plot_NVE_M4_p075_M20.png", plot = plot_NVE_M4_polygons, height = 10, dpi = 300, units = "in")

# load all four plots as images
plot_NVE_M4_polygons_p05_img <- grid::rasterGrob(png::readPNG("Plots/plot_NVE_M4_p05_M20.png"), interpolate = TRUE)
plot_NVE_M4_polygons_p025_img <- grid::rasterGrob(png::readPNG("Plots/plot_NVE_M4_p025_M20.png"), interpolate = TRUE)
plot_NVE_M4_polygons_p075_img <- grid::rasterGrob(png::readPNG("Plots/plot_NVE_M4_p075_M20.png"), interpolate = TRUE)

# Arrange the plots in a grid without further resizing
plot_NVE_M4_all <- grid.arrange(plot_NVE_M4_polygons_p05_img,
                                       plot_NVE_M4_polygons_p025_img,
                                       plot_NVE_M4_polygons_p075_img,
                                       ncol = 3)

ggsave(filename = "Plots/plot_NVE_M4_all.png", plot = plot_NVE_M4_all, height = 10, width = 17, dpi = 300)



## Adjacent Zone Correlation ----

cor_matrix %>% 
  filter(z %in% c(9,14,18,19)) %>% 
  filter(y %in% c(9,14,18,19)) %>% 
  filter(!z == y) %>% 
  arrange(cor_value)



