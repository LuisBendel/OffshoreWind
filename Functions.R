
## Get Eucl Dist ----

get_eucl_dist <- function (cell1, cell2) {
  X1 <- as.numeric(cells_NEZ_clusters["ID" = cell1, "X"])
  X2 <- as.numeric(cells_NEZ_clusters["ID" = cell2, "X"])
  Y1 <- as.numeric(cells_NEZ_clusters["ID" = cell1, "Y"])
  Y2 <- as.numeric(cells_NEZ_clusters["ID" = cell2, "Y"])
  return(sqrt((X1 - X2)^2 + (Y1 - Y2)^2))
}





## Cluster AVG Hourly ----

# Define the function to process each cluster
process_cluster <- function(cluster_ID) {
  
  # Get all cells of the cluster
  cluster_cells <- cells_NEZ_clusters[cells_NEZ_clusters[, "cluster_KM"] == cluster_ID, "ID"]
  
  # Create empty matrix with 8760 rows (hours) and as many columns as there are cells in the cluster
  cells_data <- matrix(NA, nrow = 8760, ncol = length(cluster_cells))
  
  # Load all .rds files for the cells in the cluster and fill the matrix
  for (i in 1:length(cluster_cells)) {
    filename <- paste0("data/processed/hourly_2019_cells/hourly_2019_cell_", as.character(cluster_cells[i]), ".rds")
    data_one_cell <- readRDS(filename)
    cells_data[, i] <- data_one_cell
  }
  
  # Compute the average over all cells in the cluster
  cluster_avg <- rowMeans2(cells_data, na.rm = TRUE)
  
  # Set file name dynamically depending on cluster ID
  filename <- paste0("data/processed/hourly_2019_clusters/hourly_2019_cluster_", as.character(cluster_ID), ".rds")
  
  # Save cluster wind power data to .rds file
  saveRDS(cluster_avg, filename)
  
  # Print cluster ID to track progress
  print(paste("Completed cluster:", cluster_ID))
}





## SD Actual ----

get_sd_actual <- function (MFarms, MVPweight, weightsdata) {
  # get all cells to the current scenario
  cells <- weightsdata %>% 
    filter(p == MVPweight, maxfarms == MFarms) %>% 
    pull(cell)
  
  # get all hourly data for the cells
  cells_hourly_CF <-
    sapply(1:length(cells), function (i) {
      filename = paste0("data/processed/hourly_2019_cells/hourly_2019_cell_",as.character(cells[i]), ".rds")
      readRDS(filename) / 15000000
    })
  
  # create covariance matrix
  cov_cells <- cov(cells_hourly_CF)
  
  # calculate portfolio variance (weights are the same for all cells)
  portfolio_variance <- sum(cov_cells * ((50 / 30000)^2))
  
  # return the standard deviation (sqrt of variance)
  return(sqrt(portfolio_variance))
}


#get_sd_actual(30, 0.7, model_4_out_weights)




## Get Mean CF ----

get_mean_CF <- function (MFarms, MVPweight, weightsdata) {
  cells <- weightsdata %>% 
    filter(p == MVPweight, maxfarms == MFarms) %>% 
    pull(cell)
  
  cells_NEZ_clusters_df %>% 
    filter(ID %in% cells) %>% 
    pull(CF_cells) %>% 
    mean()
}



## Get Number of Locations ----

get_n_locations <- function (MFarms, MVPweight, weightsdata) {
  weightsdata %>% 
    filter(p == MVPweight, maxfarms == MFarms) %>% 
    pull(zone) %>% unique() %>% 
    length()
}





## Create Polygons ----

get_polygons <- function (pMVP, M) {
  # initialize an object that contains all polygons
  all_polygons <- data.frame()
  
  # get all zones for the current scenario
  zones <- model_4_out_weights %>% 
    filter(p == pMVP, maxfarms == M) %>% 
    pull(zone) %>% 
    unique()
  
  # for each zone, create the buffered polygon
  for (i in 1:length(zones)) {
    # get selected cells
    selected_cells <- model_4_out_weights %>% 
      filter(maxfarms == M, p == pMVP, zone == zones[i]) %>%
      pull(cell)
    
    # create an SF object for selected cells
    selected_cells_sf <- cells_NEZ_clusters_df %>% 
      filter(ID %in% selected_cells) %>% 
      select(X, Y) %>% 
      st_as_sf(., coords = c("X", "Y"))
    
    # create a buffered hull
    buffered_hull <- st_buffer(concaveman(selected_cells_sf), dist = 0.5)
    
    # combine in all_polygons dataframe
    all_polygons <- rbind(all_polygons, buffered_hull)
  }
  
  return(cbind(zones, all_polygons))
}






## NVE Polygons ----

get_polygons_NVE <- function (data, name, NEZ_grid) {
  
  # Convert data to an sf object with lon/lat as coordinates
  data_sf <- st_as_sf(data, coords = c("lon", "lat"), crs = 4326)
  
  # Convert NEZ_grid to an sf object with lon/lat as coordinates
  NEZ_grid_sf <- st_as_sf(NEZ_grid %>% select(X, Y, lon, lat),
                      coords = c("lon", "lat"), crs = 4326)
  
  NEZ_grid_sf_xy <- st_as_sf(NEZ_grid %>% select(X, Y, lon, lat),
                            coords = c("X", "Y"))
  
  # Find the nearest grid cell X/Y for each each point in data point
  nearest_indices <- st_nearest_feature(data_sf, NEZ_grid_sf)
  data_xy <- data %>%
    mutate(X = NEZ_grid_sf$X[nearest_indices], Y = NEZ_grid_sf$Y[nearest_indices])

  # add the first point so polygon is closed
  data_xy <- rbind(data_xy, data_xy[1, ])
  # Create a polygon from the points
  polygon <- st_polygon(list(data_xy %>%
                              select(X, Y) %>%
                              as.matrix()))
  
  polygon_sf <- st_sfc(polygon, crs = st_crs(NEZ_grid_sf_xy))
  return(st_sf(geometry = polygon_sf))
  
}



## Cells Outline ----

get_cells_in_polygon <- function (shapes) {
  
  # Convert all cells to an sf object
  all_cells_sf <- st_as_sf(cells_NEZ_clusters_df %>% select(ID, X, Y),
                           coords = c("X", "Y"),
                           crs = st_crs(shapes))
  
  # Find cells within each polygon and add the zone ID to them
  cells_within_outline <- all_cells_sf %>%
    st_join(shapes, join = st_within) %>%  # Join cells with outlines they fall within
    filter(!is.na(zones))  # Filter to keep only cells within an outline
  
  return(cells_within_outline)
  
}


