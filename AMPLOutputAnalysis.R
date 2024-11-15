library(readr)
library(tidyverse)
library(viridis)
library(sp)
library(sf)

# Source functions file
source("Functions.R")

# general cells data for NEZ
cells_NEZ_clusters_df <- data.frame(readRDS("data/processed/cells_NEZ_clusters.rds"))

# output files for Model 4
model_4_out_pareto <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_pareto.csv") %>% mutate(p = round(p, 2))
model_4_out_weights <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_weights.csv") %>% mutate(p = round(p, 2))

# test if weights add up to 2000 turbines
test <- model_4_out_weights %>% 
  group_by(maxfarms, zone, p) %>% 
  summarise(weight_z = mean(weight_z)) %>% 
  mutate(n_turbines = weight_z * 2000) %>% 
  group_by(maxfarms, p) %>% 
  summarise(total_turbines = sum(n_turbines),
            weight_z_total = sum(weight_z))

# test if cell weight adds up to 1
test2 <- model_4_out_weights %>% 
  group_by(maxfarms, p) %>% 
  summarise(weight_total = sum(weight_c))

# test if aggregated cell weight is equal to zone weight
test3 <- model_4_out_weights %>% 
  group_by(maxfarms, p, zone) %>% 
  summarise(sum_cell_weight = sum(weight_c),
            weight_z = mean(weight_z)) %>% 
  mutate(diff = weight_z - sum_cell_weight)

# test if it is a multiple of 500 MW
test4 <- model_4_out_weights %>% 
  mutate(MW = weight_c * 30000) %>% 
  group_by(maxfarms, p, zone) %>% 
  summarise(MW = sum(MW))

test4 %>% 
  filter(p == 0.5, maxfarms == 20)





## 50/50 Baseline Summary

polygons <- get_polygons(pMVP = 0.5, M = 20)

# Convert all cells to an sf object
all_cells_sf <- st_as_sf(cells_NEZ_clusters_df %>% select(ID, X, Y, WPSS_NEZ),
                         coords = c("X", "Y"),
                         crs = st_crs(polygons))

# Find cells within each polygon and add the zone ID to them
cells_within_outline <- all_cells_sf %>%
  st_join(polygons, join = st_within) %>%  # Join cells with outlines they fall within
  filter(!is.na(zones))  # Filter to keep only cells within an outline

# join cells_NEZ_clusters_df and calculate mean CF and WPSS per zone
# for cells that are contained in each outline
cells_summary_outline <- cells_within_outline %>% 
  left_join(x = ., y = (cells_NEZ_clusters_df %>% select(-WPSS_NEZ)), by = c("ID" = "ID")) %>% 
  #filter(WPSS_NEZ >= 0.6386339) %>% 
  mutate(WPSS_threshold = ifelse(WPSS_NEZ >= 0.6386339, 1, 0)) %>% 
  group_by(zones) %>% 
  summarise(CF_outline = round(-mean(CF_cells) * 100, 1),
            WPSS_outline = round(mean(WPSS_NEZ), 3),
            WPSS_fish_outline = round(mean(WPSS_fish_NEZ), 3),
            WPSS_ecol_outline = round(mean(WPSS_ecol_NEZ), 3),
            WPSS_inv_outline = round(mean(WPSS_inv_NEZ), 3),
            n_cells_outline = n(),
            n_cells_outline_WPSSfiltered = sum(WPSS_threshold)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  select(-geometry)

# next, calculate everything for the selected cells only
cells_summary_selected <- model_4_out_weights %>% 
  filter(maxfarms == 20, p == 0.5) %>% 
  mutate(MW = weight_c * 30000) %>% 
  left_join(x = ., y = cells_NEZ_clusters_df, by = c("cell" = "ID")) %>% 
  group_by(zone) %>% 
  summarise(MW = round(sum(MW)),
            CF_selected = round(-mean(CF_cells) * 100, 1),
            WPSS_selected = round(mean(WPSS_NEZ), 3),
            WPSS_fish_selected = round(mean(WPSS_fish_NEZ), 3),
            WPSS_ecol_selected = round(mean(WPSS_ecol_NEZ), 3),
            WPSS_inv_selected = round(mean(WPSS_inv_NEZ), 3),
            n_cells_selected = n()) %>% 
  ungroup()

# combine both
zone_summary <- cells_summary_selected %>% 
  left_join(x = ., y = cells_summary_outline, by = c("zone" = "zones")) %>% 
  mutate(MW_pct = round(100 * (MW / 30000), 1),
         n_turbines = round((MW / 30000) * 2000)) %>% 
  select(zone, MW, MW_pct, n_turbines,
         CF_selected, CF_outline,
         WPSS_selected, WPSS_outline,
         n_cells_selected, n_cells_outline, n_cells_outline_WPSSfiltered)

write_csv(zone_summary, file = "data/out/model_4_05_M20_ZoneSummary.csv")
  



## SD Actual vs Model ----

model_4_out_pareto_actual <- model_4_out_pareto %>% 
  mutate(MeanSD_actual = map2_dbl(maxfarms, p, get_sd_actual, weightsdata = model_4_out_weights))

write_csv(model_4_out_pareto_actual, file = "data/out/model_4_out_pareto_actual.csv")






## Log File Analysis ----

# Read the log file
#log_lines <- readLines("AMPL/Model 4 (MO, cell, included)/out/20241113/model_4_log_20241113")
log_lines <- readLines("AMPL/Model 4 (MO, cell, included)//model_4_log")

# Initialize lists to store extracted data
solve_status <- c()
best_objective <- c()
best_bound <- c()
gap <- c()

# Loop over the log lines to find the relevant information
for (i in seq_along(log_lines)) {
  # Check if the line indicates whether the solution was optimal or reached the time limit
  if (grepl("Optimal solution found", log_lines[i])) {
    solve_status <- c(solve_status, "Optimal")
  } else if (grepl("Time limit reached", log_lines[i])) {
    solve_status <- c(solve_status, "Time limit reached")
  } else {
    next
  }
  
  # Find the next line with the best objective, best bound, and gap information
  if (grepl("Best objective", log_lines[i + 1])) {
    # Extract numerical values using regular expressions
    best_obj <- as.numeric(sub(".*Best objective ([^,]+).*", "\\1", log_lines[i + 1]))
    best_bnd <- as.numeric(sub(".*best bound ([^,]+).*", "\\1", log_lines[i + 1]))
    gap_val <- as.numeric(sub(".*gap ([^%]+)%.*", "\\1", log_lines[i + 1]))
    
    # Append the extracted values to the lists
    best_objective <- c(best_objective, best_obj)
    best_bound <- c(best_bound, best_bnd)
    gap <- c(gap, gap_val)
  }
}

# Create a data frame with the extracted information
log_df <- data.frame(
  Solve_Status = solve_status,
  Best_Objective = round(best_objective, 4),
  Best_Bound = round(best_bound, 4),
  Gap = round(gap, 4)
)

log_df <- data.frame(
  maxfarms = c(0, 0, model_4_out_pareto$maxfarms),
  p = c(0, 0, model_4_out_pareto$p),
  log_df
)

write_csv(log_df, file = "data/out/model_4_log_results.csv")

