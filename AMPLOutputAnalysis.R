###############################################################################-
# This file is to analyse the output from solving the model in AMPL
###############################################################################-

library(readr)
library(tidyverse)
library(viridis)
library(sp)
library(sf)
library(concaveman)

# Source functions file
source("Functions.R")


## LOAD DATA ----

# general cells data for NEZ
cells_NEZ_clusters_df <- data.frame(readRDS("data/processed/cells_NEZ_clusters.rds"))

# output files for Model 4
model_4_out_pareto <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_pareto.csv") %>% mutate(p = round(p, 2))
model_4_out_weights <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_weights.csv") %>% mutate(p = round(p, 2))


## Tests ----

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





## Zone Summary Table ----

pMVP = 0.95

polygons <- get_polygons(pMVP = pMVP, M = 20)

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
  mutate(WPSS_threshold = ifelse(WPSS_NEZ >= 0.6386339, 1, 0),
         WPSS_threshold_fish = ifelse(WPSS_fish_NEZ >=  0.7678101, 1, 0),
         WPSS_threshold_ecol = ifelse(WPSS_ecol_NEZ >= 0.8056636, 1, 0),
         WPSS_threshold_inv = ifelse(WPSS_inv_NEZ >= 0.6209197, 1, 0)) %>% 
  mutate(WPSS_threshold_All = ifelse(WPSS_threshold == 1 &
                                       WPSS_threshold_fish == 1 &
                                       WPSS_threshold_ecol == 1 &
                                       WPSS_threshold_inv == 1, 1, 0)) %>% 
  group_by(zones) %>% 
  summarise(CF_outline = round(-mean(CF_cells) * 100, 1),
            WPSS_outline = round(mean(WPSS_NEZ), 3),
            WPSS_fish_outline = round(mean(WPSS_fish_NEZ), 3),
            WPSS_ecol_outline = round(mean(WPSS_ecol_NEZ), 3),
            WPSS_inv_outline = round(mean(WPSS_inv_NEZ), 3),
            n_cells_outline = n(),
            n_cells_outline_WPSSfiltered = sum(WPSS_threshold), # comment in for baseline WPSS
            #n_cells_outline_WPSSfiltered = sum(WPSS_threshold_fish), # comment in for fisherman WPSS
            #n_cells_outline_WPSSfiltered = sum(WPSS_threshold_ecol), # comment in for ecologist WPSS
            #n_cells_outline_WPSSfiltered = sum(WPSS_threshold_inv), # comment in for investor WPSS
            n_cells_outline_WPSSfilteredAll = sum(WPSS_threshold_All)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  select(-geometry)

# next, calculate everything for the selected cells only
cells_summary_selected <- model_4_out_weights %>% 
  filter(maxfarms == 20, p == pMVP) %>% 
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
         WPSS_fish_selected, WPSS_fish_outline,
         WPSS_ecol_selected, WPSS_ecol_outline,
         WPSS_inv_selected, WPSS_inv_outline,
         n_cells_selected, n_cells_outline, n_cells_outline_WPSSfiltered, n_cells_outline_WPSSfilteredAll)

write_csv(zone_summary, file = "data/out/model_4_05_M20_ZoneSummary.csv")
write_csv(zone_summary, file = "data/out/model_4_025_M20_ZoneSummary.csv")
write_csv(zone_summary, file = "data/out/model_4_075_M20_ZoneSummary.csv")

write_csv(zone_summary, file = "data/out/model_4_05_M20_ZoneSummary_AllWPSS.csv")
  



## SD Actual vs Outline vs Model ----

model_4_out_weights_outlined <- model_4_out_pareto %>% 
  mutate(polygons = map2(.x = p, .y = maxfarms, .f = get_polygons)) %>% 
  mutate(cells_outline = map(.x = polygons, .f = get_cells_in_polygon)) %>% 
  unnest(cells_outline) %>% 
  select(maxfarms, p, ID) %>% 
  rename(cell = ID)

# this runs quite long due to the get_sd_actual function
model_4_out_pareto_actual <- model_4_out_pareto %>% 
  mutate(MeanSD_actual = map2_dbl(maxfarms, p, get_sd_actual, weightsdata = model_4_out_weights)) %>% 
  mutate(MeanSD_outlined = map2_dbl(maxfarms, p, get_sd_actual, weightsdata = model_4_out_weights_outlined)) %>% 
  mutate(MeanCF_outlined = map2_dbl(maxfarms, p, get_mean_CF, weightsdata = model_4_out_weights_outlined)) %>% 
  mutate(MeanCF = round(-MeanCF*100, 1),
         MeanCF_outlined = round(-MeanCF_outlined*100, 1),
         MeanSD = round(MeanSD, 3),
         MeanSD_actual = round(MeanSD_actual, 3),
         MeanSD_outlined = round(MeanSD_outlined, 3)) %>% 
  mutate(n_locations = map2_dbl(maxfarms, p, get_n_locations, weightsdata = model_4_out_weights))

write_csv(model_4_out_pareto_actual, file = "data/out/model_4_out_pareto_actual.csv")

  

test %>% unnest(polygons)
  






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





## CASE summary ----


#### A
CASEA1 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_pareto.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.5)

CASEA2 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_pareto.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.25)

CASEA3 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_pareto.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.75)

#### B
CASEB1 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Fisherman/model_4_out_pareto_fish.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.5)

CASEB2 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Ecologist/model_4_out_pareto_ecol.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.5)

CASEB3 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Investor/model_4_out_pareto_inv.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.5)

#### C
CASEC1 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/AllWPSS/model_4_out_pareto_All.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.5)

CASEC2 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/AllWPSS/model_4_out_pareto_All.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.75)

AllCases <- rbind(
  CASEA1, CASEA2, CASEA3, CASEB1, CASEB2, CASEB3, CASEC1, CASEC2
)


#### Weights
n_CASEA1 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_weights.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.5) %>% pull(zone) %>% unique() %>% length()

n_CASEA2 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_weights.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.75) %>% pull(zone) %>% unique() %>% length()

n_CASEA3 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/model_4_out_weights.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.25) %>% pull(zone) %>% unique() %>% length()

#### B
n_CASEB1 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Fisherman/model_4_out_weights_fish.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.5) %>% pull(zone) %>% unique() %>% length()

n_CASEB2 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Ecologist/model_4_out_weights_ecol.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.5) %>% pull(zone) %>% unique() %>% length()

n_CASEB3 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/Investor/model_4_out_weights_inv.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.5) %>% pull(zone) %>% unique() %>% length()

#### C
n_CASEC1 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/AllWPSS/model_4_out_weights_All.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.5) %>% pull(zone) %>% unique() %>% length()

n_CASEC2 <- read_csv("AMPL/Model 4 (MO, cell, included)/out/AllWPSS/model_4_out_weights_All.csv") %>% 
  mutate(p = round(p, 2)) %>% 
  filter(p == 0.75) %>% pull(zone) %>% unique() %>% length()

n_AllCases <- c(
  n_CASEA1, n_CASEA2, n_CASEA3, n_CASEB1, n_CASEB2, n_CASEB3, n_CASEC1, n_CASEC2
)


#### Manual Columns
col_CASEName <- c(
  "CASE A1", "CASE A2", "CASE A3",
  "CASE B1", "CASE B2", "CASE B3",
  "CASE C1", "CASE C2"
)

col_WPSSConstraint <- c(
  "Baseline",
  "Baseline",
  "Baseline",
  "Fisherman",
  "Ecologist",
  "Investor",
  "All WPSS",
  "All WPSS"
)

col_WeightMVP <- c(
  50, 75, 25, 50, 50, 50, 50, 75
)

col_WeightCF <- c(
  50, 25, 75, 50, 50, 50, 50, 25
)

CASE_summary <- cbind(
  CASE = col_CASEName,
  WPSSCOnstraint = col_WPSSConstraint,
  WeightMVP = col_WeightMVP,
  WeightCF = col_WeightCF,
  n_locations = n_AllCases,
  AllCases
) %>% 
  select(-maxfarms, -MeanVariance, -p, -BiObj) %>% 
  mutate(MeanSD = round(MeanSD, 3),
         MeanCF = round(100*(-MeanCF), 1))

write_csv(CASE_summary, file = "data/out/model_4_case_summary.csv")

