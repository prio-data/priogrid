read_corruption <- function() {
  fname <- "/Users/ingvildsmestad/Downloads/GDL-Comprehensive-Subnational-Corruption-Index-(SCI)-of-region-2.csv"
  r <- terra::rast(fname)

  r <- terra::clamp(r, 0, values = FALSE)

  return(r)
}

corruption <- function(data = read_corruption()) {
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

  names(data) <- "corruption"

  return(data)
}

start_read <- Sys.time()
r_data <- read_corruption()
time_read <- Sys.time() - start_read
print(paste("Time to read data:", time_read))

start_gen <- Sys.time()
r_processed <- corruption(data = r_data)
time_gen <- Sys.time() - start_gen
print(paste("Time to process data:", time_gen))

pg <- prio_blank_grid()
df <- as.data.frame(c(pg, r_processed))
plot(r_processed)

