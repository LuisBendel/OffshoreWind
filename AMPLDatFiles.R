library(readr)

cov_matrix <- read_csv("data/out/cov_matrix.csv")
cor_matrix <- read_csv("data/out/cor_matrix.csv")
var <- read_csv("data/out/var.csv")
X_coord <- read_csv("data/out/X_coord.csv")
Y_coord <- read_csv("data/out/Y_coord.csv")
ZID <- read_csv("data/out/ZID.csv")
WPSS <- read_csv("data/out/WPSS.csv")
CF <- read_csv("data/out/CF.csv")
CF_Zones <- read_csv("data/out/CF_Zones.csv")
DistNW <- read_csv("data/out/DistNW.csv")
# NID <- read_csv("data/out/NID.csv")
# euclidean_dist <- read_csv("data/out/EUCL_Dist.csv")




## COV Matrix ----

# define filename and open file connection
filename <- "AMPL/cov_matrix.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param COV:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(cov_matrix)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(cov_matrix$z[i:end],
                          cov_matrix$y[i:end],
                          cov_matrix$cov_value[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)





## CORR Matrix ----

filename <- "AMPL/cor_matrix.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param CORR:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(cor_matrix)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(cor_matrix$z[i:end],
                          cor_matrix$y[i:end],
                          cor_matrix$cor_value[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)




## VAR ----

filename <- "AMPL/var.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param VAR:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(var)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(var$z[i:end],
                          var$var[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)




## X Coordinate ----

filename <- "AMPL/X_coord.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param X:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(X_coord)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(X_coord$i[i:end],
                          X_coord$X[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)




## Y Coordinate ----

filename <- "AMPL/Y_coord.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param Y:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(Y_coord)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(Y_coord$i[i:end],
                          Y_coord$Y[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)





## ZID ---- 

filename <- "AMPL/ZID.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param ZID:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(ZID)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(ZID$i[i:end],
                          ZID$ZID[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)




## WPSS ---- 

filename <- "AMPL/WPSS.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param WPSS:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(WPSS)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(WPSS$i[i:end],
                          WPSS$WPSS[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)




## CF ---- 

filename <- "AMPL/CF.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param CF:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(CF)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(CF$i[i:end],
                          CF$CF[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)




## CF Zones ---- 

filename <- "AMPL/CF_Zones.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param CF_Zones:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(CF_Zones)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(CF_Zones$z[i:end],
                          CF_Zones$CF[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)





## DistNW ---- 

filename <- "AMPL/DistNW.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param DistNW:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(DistNW)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(DistNW$i[i:end],
                          DistNW$DistNW[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)




## NID ---- 

filename <- "AMPL/NID.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param NID:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(NID)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(NID$ID[i:end],
                          NID$N_NeighB[i:end],
                          NID$neighborID[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)




## EUCL_Dist ----

filename <- "AMPL/EUCL_Dist.dat"
file_conn <- file(filename, "w")
# Write the header in AMPL format
writeLines("param EUCL_Dist:=", con = file_conn)
# Write in chunks
chunk_size <- 1000  # Adjust this based on your available memory and data size
n <- nrow(euclidean_dist)
# Loop through the data in chunks
for (i in seq(1, n, by = chunk_size)) {
  end <- min(i + chunk_size - 1, n)
  lines_to_write <- paste(euclidean_dist$cell1[i:end],
                          euclidean_dist$cell2[i:end],
                          euclidean_dist$dist[i:end],
                          sep = "\t")
  writeLines(lines_to_write, con = file_conn)
}
# Close the parameter definition
writeLines(";", con = file_conn)
# Close the file connection
close(file_conn)





## MZ_lower, MZ_upper ----
gen <- function(n) {
  (n * (500/30000) - 0.0025 + 1/30000) * 30000
}

gen2 <- function(n) {
  (n * (500/30000) + 0.0025 - 1/30000) * 30000
}


sapply(1:60, gen)
sapply(1:60, gen2)



