
library(sp)
library(tidyverse)
library(ClustGeo)
library(rgeoda)
library(sf)
library(viridis)

monthly_NEZ <- readRDS("data/processed/monthly_NEZ.rds")

# define columns that should be included in the KMeans clustering
start_year <- 2019 # Start year to inlcude in KMeans clustering
end_year <- 2019 # end year to include in Kmeans clustering
# all monthly columns to include in the KMeans clustering
colnames <- 
  expand.grid(month = 1:12, year = start_year:end_year) %>% 
  mutate(colname = paste0(year, "_M", month)) %>% 
  pull(colname)
# add other columns to include in the KMeans clustering
colnames <- c("X", "Y", colnames)



# subset data to include only the above columns
monthly_NEZ_cl <- monthly_NEZ %>% 
  as.data.frame() %>% 
  filter(!ID2 == -1) %>% # in this file we just look at the filtered IDs (ID2)
  select(colnames)

# scale the data (only coords and power generation)
data_scaled <- scale(monthly_NEZ_cl)

# put weights on coordinates to have more continuous clusters
# this value as of now is arbitrarily chosen
data_scaled[, c("X", "Y")] <- data_scaled[, c("X", "Y")] * 20


## KMeans ----
# use only coords and power generation in clustering

set.seed(123)
clusters_KM <- kmeans(data_scaled[, c(1:14)], centers = 100, iter.max = 100)
monthly_NEZ <- cbind(cluster_KM = clusters_KM$cluster, monthly_NEZ)

# compute cluster centroids for each cluster
# recode the cluster according to the X and Y coordinate
cluster_centroids <- monthly_NEZ %>% 
  data.frame() %>% 
  select(X, Y, cluster_KM) %>% 
  group_by(cluster_KM) %>% 
  summarize(X = floor(mean(X)),
            Y = floor(mean(Y))) %>% 
  arrange(Y, X) %>% 
  mutate(new_cluster_KM = row_number())

# update the cluster ID in the monthly_NEZ matrix
# now the clusters are more or less geographically ordered
monthly_NEZ[, "cluster_KM"] <- monthly_NEZ[, "cluster_KM"] %>% 
  as_tibble() %>% 
  rename(cluster_KM = value) %>% 
  left_join(y = cluster_centroids, by = "cluster_KM") %>% 
  pull(new_cluster_KM)

saveRDS(monthly_NEZ, file = "data/processed/monthly_NEZ_clusters.rds")
saveRDS(monthly_NEZ[, 1:9], file = "data/processed/cells_NEZ_clusters.rds")



# the problem with Kmeans is that the clusters are geographically not
# necessarily connected, for example with seed 123, cluster 33 is spread out
# putting higher weights on the coordinates can decrease this problem
cells_NEZ_clusters <- readRDS("data/processed/cells_NEZ_clusters.rds")

plot1 <- data.frame(cells_NEZ_clusters) %>% 
  ggplot(aes(x = X, y = Y, fill = as.factor(cluster_KM))) +
    geom_raster() +
    theme_minimal() +
    guides(fill = "none") +
    coord_fixed() +
    scale_fill_viridis_d(option = "turbo", direction = 1) +
    geom_text(data = cluster_centroids, aes(label = new_cluster_KM), color = "black", size = 3)

ggsave(plot1, file = "plots/clusters_100_KM_weights20_inclwind.png", height = 1149/50,width = 652/50)

# Here we can just filter on one cluster to test how the clusters look
# check 33 and 67 with seed 123 on Kmeans
data.frame(cells_NEZ_clusters) %>% 
  mutate(check_cluster = ifelse(cluster_KM %in% c(4,8,10), 1, 0)) %>% 
  ggplot(aes(x = X, y = Y, fill = as.factor(check_cluster))) +
    geom_raster() +
    theme_minimal() +
    guides(color = "none") +
    coord_fixed()
