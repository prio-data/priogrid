
# Source helper functions (do this only ONCE)
source("~/Desktop/Gitprodjects/priogrid/R/utility.R")
source("~/Desktop/Gitprodjects/priogrid/R/options.R")

# --- TEST ---

# Read data
start_read <- Sys.time()
r_data <- read_sedac_global_infant_mortality_rates()
time_read <- Sys.time() - start_read
print(paste("Time to read data:", time_read))

# Process data
start_gen <- Sys.time()
r_processed <- gen_sedac_global_infant_mortality_rates(data = r_data)
time_gen <- Sys.time() - start_gen
print(paste("Time to process data:", time_gen))

# Create blank grid and dataframe
pg <- prio_blank_grid()
df <- as.data.frame(c(pg, r_processed))

# Plot processed raster
plot(r_processed)
