
library(matrixStats) # for rowmeans2 function
library(ncdf4) # reading .nc files
library(tidyverse) # data wrangling
library(R.matlab) # reading .mat files


## Load WPSS ----
WPSS <- readMat("data/raw/SCORES/SCORE_baseline.mat")$SCORE
WPSS_long <- as.vector(WPSS)
WPSS_NEZ <- WPSS_long[!is.na(WPSS_long)]


## Load MinDistNW ----
MinDistNW <- readMat("data/raw/SCORES/min_dist_to_NORWAY.mat")$dd.min.resh
MinDistNW_NEZ <- as.vector(MinDistNW)[!is.na(WPSS_long)]



## Load Monthly Power ----
start_year <- 1996 # Start year to load monthly data
end_year <- 2019 # end year to load monthly data (included)
start_month <- (start_year - 1996) * 12 + 1 # Start month ID
count_months <- (end_year - start_year + 1) * 12 # number of months to load

# load the .nc file and extract only the power generation variable
# The monthly_power matrix will have dimensions [1:652, 1:1149, 1:12]
monthly <- nc_open("data/raw/WindPower_generation_monthly.nc")
var_name <- "WindPower_generation_monthly"
monthly_power <- ncvar_get(monthly, var_name, start = c(1, 1, 3, 1, start_month), count = c(-1, -1, 1, 1, count_months))
lon <- as.vector(ncvar_get(monthly, varid = "lon", start = c(1, 1), count = c(-1, -1)))[!is.na(WPSS_long)]
lat <- as.vector(ncvar_get(monthly, varid = "lat", start = c(1, 1), count = c(-1, -1)))[!is.na(WPSS_long)]
nc_close(monthly)

# Get X and Y coordinates for each grid cell in a data frame format
# Filter for only coordinates in NEZ
coords_all <- as.matrix(expand.grid(X = 1:652, Y = 1:1149))
coords_NEZ <- coords_all[!is.na(WPSS_long), ]

# convert monthly_power to 2D-matrix (1 column = 1 month)
# Adjust column names of matrix
monthly_data <- sapply(1:count_months, function(i) {
  as.vector(monthly_power[ , , i])
})
rm(monthly_power, monthly)
gc()

colnames(monthly_data) <- 
  expand.grid(month = 1:12, year = start_year:end_year) %>% 
  mutate(colname = paste0(year, "_M", month)) %>% 
  pull(colname)

monthly_data <- monthly_data[!is.na(WPSS_long), ]

# Add average column (Average for each month over the years)
for (m in 1:12) {
  col_indexes <- c(m, m + c(1:23) * 12)
  monthly_data <- cbind(monthly_data, rowMeans2(monthly_data[, col_indexes]))
}

colnames(monthly_data)[c(289:300)] <- paste0("AVG_M", c(1:12))



## Combine ----

# combine both, X and Y coordinates, WPSS, and monhtly power columns in one data frame
# save combined matrix as .rds
monthly_NEZ <-
  cbind(ID = c(1:nrow(coords_NEZ)), # unique cell ID from 1 to 71021
        coords_NEZ, # X and Y grid coordinates
        lon, # cell longitude 
        lat, # cell latitude
        WPSS_NEZ, # WPSS scores for NEZ
        MinDistNW_NEZ) # minimum distance to Norway




## Load Hourly Power ----
# Note: this script will run very long depending on start and end Cell ID
# for 71021, it took approx. 40 hours on my PC (L.B.) to load all data

# load cells NEZ data
cells_NEZ <- monthly_NEZ[, 1:7]

# open the 2019 bourly wind power .nc file
hourly_2019 <- nc_open("C:/Users/luisr/Downloads/WindPower_generation_hourly_2019.nc")
var_name <- "WindPower_generation_hourly"

# set start and end cell ID
start <- 70001
end <- 71021
for (i in start:end) {
  # X and Y coords from the spatially clustered NEZ data for the cell ID
  X <- cells_NEZ[cells_NEZ$ID == i, "X"]
  Y <- cells_NEZ[cells_NEZ$ID == i, "Y"]
  # Set filename dynamically according to cell ID
  filename <- paste0("data/processed/hourly_2019_cells/hourly_2019_cell_", as.character(i), ".rds")
  
  # load cell XY, turbine 3 (15MW) all 8760 hours of 2019
  hourly_2019_cell <-
    ncvar_get(hourly_2019,
              varid = var_name,
              start = c(X, Y, 3, 1, 1),
              count = c(1, 1, 1, 1, 8760))
  
  # save the resulting vector as .rds file for faster processing
  saveRDS(hourly_2019_cell, file = filename)
  
  # keep track of process
  print(i)
}

# close .nc file
nc_close(hourly_2019)




## Add Mean CF ----

# Capacity Factor for each cell
CF_cells <- sapply(1:71021, function (i) {
  filename <- paste0("data/processed/hourly_2019_cells/hourly_2019_cell_", as.character(i), ".rds")
  hourly_cell <- readRDS(filename)
  print(i)
  return(-mean(hourly_cell / 15000000))
})


## Combine everything ----
monthly_NEZ <-
  cbind(ID = c(1:nrow(coords_NEZ)), # unique cell ID from 1 to 71021
        coords_NEZ, # X and Y grid coordinates
        lon, # cell longitude 
        lat, # cell latitude
        WPSS_NEZ, # WPSS scores for NEZ
        MinDistNW_NEZ, # minimum distance to Norway
        CF_cells, # mean capacity factor for cells
        monthly_data) # all monthly average wind power data

saveRDS(monthly_NEZ, file = "data/processed/monthly_NEZ.rds")
saveRDS(monthly_NEZ[, 1:8], file = "data/processed/cells_NEZ.rds")








