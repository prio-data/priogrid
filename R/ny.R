
read_sedac_global_infant_mortality_rates <- function() {

  ## URL not avaliable
  #https://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-infant-mortality-rates-v2-01/data-download
  fname <- "povmap-global-subnational-infant-mortality-rates-v2-01-geotiff-2/povmap_global_subnational_infant_mortality_rates_v2_01.tif"
  r <- terra::rast(fname)

  r <- terra::clamp(r, 0, values = FALSE)

  return(r)
}

gen_sedac_global_infant_mortality_rates <- function(data = read_sedac_global_infant_mortality_rates()) {
  pg <- prio_blank_grid()

  if (terra::crs(data) != terra::crs(pg)) {
    data <- terra::project(data, terra::crs(pg))
  }

  if (terra::ext(data) != terra::ext(pg)) {
    tmp <- terra::rast(terra::ext(pg),
                       crs = terra::crs(data),
                       ncol = terra::ncol(pg),
                       nrow = terra::nrow(pg))
    data <- terra::resample(data, tmp, method = "near", threads = TRUE)
  }

  data <- terra::resample(data, pg, method = "near", threads = TRUE)

  names(data) <- "sedac_global_infant_mortality_rate"

  return(data)
}

# --- TEST ---

start_read <- Sys.time()
r_data <- read_sedac_global_infant_mortality_rates()
time_read <- Sys.time() - start_read
print(paste("Time to read data:", time_read))

start_gen <- Sys.time()
r_processed <- gen_sedac_global_infant_mortality_rates(data = r_data)
time_gen <- Sys.time() - start_gen
print(paste("Time to process data:", time_gen))

pg <- prio_blank_grid()
df <- as.data.frame(c(pg, r_processed))
plot(r_processed)
