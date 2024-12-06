###############################################################################-
# This file is to cluster the grid cells into wind zones
###############################################################################-

library(tidyverse) # data wrangling
library(viridis) # color palette

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
  select(all_of(colnames))

# scale the data (only coords and power generation)
data_scaled <- scale(monthly_NEZ_cl)

# put weights on coordinates to have more continuous clusters
# this value as of now is arbitrarily chosen
# comment this out if you do not want to put more weight on coordinates
# data_scaled[, c("X", "Y")] <- data_scaled[, c("X", "Y")]


## KMeans ----
# use only coords and power generation in clustering

set.seed(123)
# in the final approach, we only include the spatial variables X and Y
clusters_KM <- kmeans(data_scaled[, c(1:2)], centers = 150, iter.max = 150)
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
saveRDS(monthly_NEZ[, 1:13], file = "data/processed/cells_NEZ_clusters.rds")




