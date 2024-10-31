
library(matrixStats) # for the rowMeans2 function
library(reshape2) # for the melt() function
library(readr) # for writing csv files
library(dplyr)
library(tidyverse)

# load NEZ data
cells_NEZ_clusters <- readRDS("data/processed/cells_NEZ_clusters.rds")


## Cluster AVG Hourly ----

for (c in 1:100) {

  # set cluster ID
  cluster_ID <- c
  
  # get all cells of the cluster
  # cluster_cells <- 1:1000
  cluster_cells <- cells_NEZ_clusters[cells_NEZ_clusters[, "cluster_KM"] == cluster_ID, "ID"]
  
  # create empty matrix with length(cluster_cells) columns
  cells_data <- matrix(NA, nrow = 8760, ncol = length(cluster_cells))
  
  # load all .rds files for the cells in the cluster and fill matrix
  for (i in 1:length(cluster_cells)) {
    filename <- paste0("data/processed/hourly_2019_cells/hourly_2019_cell_", as.character(cluster_cells[i]), ".rds")
    data_one_cell <- readRDS(filename)
    cells_data[, i] <- data_one_cell
  }
  
  # compute average over all cells in the cluster
  cluster_avg <- rowMeans2(cells_data)
  
  # set file name dynamically depending on cluster ID
  filename <- paste0("data/processed/hourly_2019_clusters/hourly_2019_cluster_", as.character(cluster_ID), ".rds")
  
  # save cluster wind power data to .rds file
  saveRDS(cluster_avg, filename)
  
  # print to keep track of process
  print(c)
  
}



## Load Zone Data ----

clusters_hourly <-
  sapply(1:100, function (i) {
    filename = paste0("data/processed/hourly_2019_clusters/hourly_2019_cluster_",as.character(i), ".rds")
    readRDS(filename)
  })




## CORR Zones ----

# calculate capacity factor
clusters_hourly_CF <- clusters_hourly / 15000000

# correlation matrix
cor_matrix <- cor(clusters_hourly_CF)
# Create row and column indices
row_idx <- rep(1:nrow(cor_matrix), times = ncol(cor_matrix))
col_idx <- rep(1:ncol(cor_matrix), each = nrow(cor_matrix))
# Create a long format of cor matrix as dataframe
cor_matrix_long <- data.frame(z = row_idx, y = col_idx, cor_value = as.vector(cor_matrix)) %>% 
  mutate_if(is.numeric, ~ format(., scientific = FALSE))

write_csv(cor_matrix_long, file = "data/out/cor_matrix.csv")




## COV Zones ----

# covariance matrix
cov_matrix <- cov(clusters_hourly_CF)
# Create a long format of cor matrix as dataframe
cov_matrix_long <- data.frame(z = row_idx, y = col_idx, cov_value = as.vector(cov_matrix)) %>% 
  mutate_if(is.numeric, ~ format(., scientific = FALSE))

write_csv(cov_matrix_long, file = "data/out/cov_matrix.csv")




## Variance Zones ----

# calculate the variance for each cluster
# write to csv file
clusters_var <-  sapply(1:100, function (i) {
  var(clusters_hourly_CF[, i])
})
clusters_var_df <- data.frame(cbind(z = 1:100, var = clusters_var)) %>% 
  mutate_if(is.numeric, ~ format(., scientific = FALSE))
write_csv(clusters_var_df, file = "data/out/var.csv")




## CORR WITHIN Zones ----

zone <- 25

zone_cells <- data.frame(cells_NEZ_clusters) %>% 
  filter(cluster_KM == zone) %>% 
  pull(ID)

zone_cells_hourly <-
  sapply(zone_cells, function (i) {
    filename = paste0("data/processed/hourly_2019_cells/hourly_2019_cell_",as.character(i), ".rds")
    readRDS(filename)
  })

zone_cells_hourly_CF <- zone_cells_hourly / 15000000

# correlation matrix for one zone
cor_matrix_zone <- cor(zone_cells_hourly_CF)
# Create row and column indices
row_idx <- rep(1:nrow(cor_matrix_zone), times = ncol(cor_matrix_zone))
col_idx <- rep(1:ncol(cor_matrix_zone), each = nrow(cor_matrix_zone))
# Create a long format of cor matrix as dataframe
cor_matrix_zone_long <- data.frame(z = as.numeric(row_idx), y = as.numeric(col_idx), cor_value = as.numeric(as.vector(cor_matrix_zone)))

write_csv(cor_matrix_zone_long, file = paste0("data/out/cor_matrix_zone_", zone, ".csv"))



cor_within_all_zones <- data.frame(
  cluster_KM = as.numeric(),
  cor_min = as.numeric(),
  cor_05 = as.numeric(),
  cor_025 = as.numeric()
)

for (z in 1:100) {

  zone_cells <- data.frame(cells_NEZ_clusters) %>% 
    filter(cluster_KM == z) %>% 
    pull(ID)
  
  zone_cells_hourly <-
    sapply(zone_cells, function (i) {
      filename = paste0("data/processed/hourly_2019_cells/hourly_2019_cell_",as.character(i), ".rds")
      readRDS(filename)
    })
  
  zone_cells_hourly_CF <- zone_cells_hourly / 15000000
  
  # correlation matrix for one zone
  cor_matrix_zone <- cor(zone_cells_hourly_CF)
  
  cor_within_all_zones <-
    rbind(cor_within_all_zones,
          data.frame(
            cluster_KM = z,
            cor_min = min(cor_matrix_zone),
            cor_05 = as.numeric(quantile(cor_matrix_zone, 0.05)),
            cor_025 = as.numeric(quantile(cor_matrix_zone, 0.025))
          ))

  print(z)
  
}

## COV WITHIN Zones ----





## Coordinates Cells ----

# get X and Y coordinate for each cell and write to csv
write_csv(data.frame(i = cells_NEZ_clusters[, "ID"],
                     X = cells_NEZ_clusters[, "X"]),
          file = "data/out/X_coord.csv")
write_csv(data.frame(i = cells_NEZ_clusters[, "ID"],
                     Y = cells_NEZ_clusters[, "Y"]),
          file = "data/out/Y_coord.csv")




## Zone ID Cells ----

# get the Zone ID for each cell and write to csv
write_csv(data.frame(i = cells_NEZ_clusters[, "ID"],
                     ZID = cells_NEZ_clusters[, "cluster_KM"]),
          file = "data/out/ZID.csv")




## WPSS Cells ----

# write WPSS for each cell
write_csv(data.frame(i = cells_NEZ_clusters[, "ID"],
                     WPSS = cells_NEZ_clusters[, "WPSS_NEZ"]),
          file = "data/out/WPSS.csv")




## CF Cells ----

# write CF for each cell
write_csv(data.frame(i = cells_NEZ_clusters[, "ID"],
                     CF = cells_NEZ_clusters[, "CF_cells"]),
          file = "data/out/CF.csv")




## CF Zones ----

# Capacity Factor for each zone
CF_zones <- data.frame(cells_NEZ_clusters) %>% 
  group_by(cluster_KM) %>% 
  summarise(CF_zones = mean(CF_cells)) %>%
  arrange(cluster_KM)

CF_zones_df <- data.frame(z = 1:100, CF = CF_zones)

write_csv(CF_zones_df, file = "data/out/CF_Zones.csv")




## Distance to NW ----

# Distance to NW for each cell
write_csv(data.frame(i = cells_NEZ_clusters[, "ID"],
                     DistNW = cells_NEZ_clusters[, "MinDistNW_NEZ"]),
          file = "data/out/DistNW.csv")





## Neighboring Cells ----

# Set of neighbor cells
df_coord <- data.frame(cells_NEZ_clusters) %>% 
  select(ID, X, Y)

neighbors_list <-
  sapply(1:71021, function (i) {
    X_b <- df_coord[i,]$X
    Y_b <- df_coord[i,]$Y
    
    neighbors <-
      which(
        df_coord$X == X_b - 1 & df_coord$Y == Y_b |
          df_coord$X == X_b - 1 & df_coord$Y == Y_b - 1 |
          df_coord$X == X_b - 1 & df_coord$Y == Y_b + 1 |
          df_coord$X == X_b + 1 & df_coord$Y == Y_b |
          df_coord$X == X_b + 1 & df_coord$Y == Y_b - 1 |
          df_coord$X == X_b + 1 & df_coord$Y == Y_b + 1 |
          df_coord$X == X_b & df_coord$Y == Y_b + 1 |
          df_coord$X == X_b & df_coord$Y == Y_b - 1
        )
    
    neighbors <- c(neighbors, rep(0, 8 - length(neighbors)))
    print(i)
    return(neighbors)
  })

neighbors_matrix <- t(neighbors_list)

colnames(neighbors_matrix) <- c(1, 2, 3, 4, 5, 6, 7, 8)

neighbors_matrix <- cbind(ID = 1:71021, neighbors_matrix)

# Pivot the matrix to a longer format
neighbors_long <- as.data.frame(neighbors_matrix) %>%
  pivot_longer(
    cols = -ID,        # All columns except row_id
    names_to = "N_NeighB",    # Name for the column names
    values_to = "neighborID"     # Name for the values
  )

write_csv(neighbors_long, file = "data/out/NID.csv")




## Euclidean Dist Cells ----

get_eucl_dist <- function (cell1, cell2) {
  X1 <- as.numeric(cells_NEZ_clusters["ID" = cell1, "X"])
  X2 <- as.numeric(cells_NEZ_clusters["ID" = cell2, "X"])
  Y1 <- as.numeric(cells_NEZ_clusters["ID" = cell1, "Y"])
  Y2 <- as.numeric(cells_NEZ_clusters["ID" = cell2, "Y"])
  return(sqrt((X1 - X2)^2 + (Y1 - Y2)^2))
}


get_eucl_dist(4960, 7006)


i <- 10

euclidean_dist <- data.frame()

for (i in 1:100) {
  cells <- cells_NEZ_clusters %>% data.frame() %>% 
    filter(cluster_KM == i) %>% pull(ID)
  
  cell_combinations <- expand.grid(cell1 = cells, cell2 = cells) %>% 
    filter(!cell1 == cell2)
  
  euclidean_dist_zone <- cell_combinations %>% 
    mutate(dist = get_eucl_dist(cell1, cell2)) %>% 
    filter(!cell1 == cell2)
  
  euclidean_dist <- rbind(euclidean_dist, euclidean_dist_zone)
  
  print(i)
}

write_csv(euclidean_dist, file = "data/out/EUCL_Dist.csv")




cells_NEZ_clusters %>% data.frame() %>% 
  filter(cluster_KM == 10) %>% 
  filter(ID %in% c(4960, 7006))


nrow(euclidean_dist %>% filter(dist <= 12))
gc()
