library(ncdf4)

input_folder <- "~/data/priogrid_data/"
output_folder <- "~/data/priogrid_res/"

#### Generate all individual data as .rds files in output_folder ###
make_pg(input_folder = input_folder, output_folder = output_folder)

# Combine into netcdf files
RASTER_FILES <- list.files(path = output_folder, full.names = TRUE)
RASTER_FILES <- RASTER_FILES[!grepl("priogrid.nc", RASTER_FILES)]

#### Yearly NetCDF ####
yearly_netcdf <- paste(output_folder, "priogrid_yearly.nc", sep = "")

# Define dimensions #
xvals <- seq(-179.75, 179.75, by = 0.5)
yvals <- seq(-89.75, 89.75, by = 0.5)
years <- 1946:2016

nx <- length(xvals)
ny <- length(yvals)
nt <- length(years)

xdim <- ncdim_def( 'lon', 'degrees_east', xvals )
ydim <- ncdim_def( 'lat', 'degrees_north', yvals )
tdim <- ncdim_def( 'year', 'integer', 0, unlim=TRUE )

# Define variables #
integer_mv <- -99     # missing value
float_mv <- 1.e30     # missing value

var_gwcode <- ncvar_def(name = "gwcode", units = 'gwcode', dim = list(xdim,ydim,tdim), missval = integer_mv, compression = 7)
var_gwcode2 <- ncvar_def(name = "gwcode2", units = 'gwcode', dim = list(xdim,ydim,tdim), missval = integer_mv, compression = 7)

# Make new output file #
ncid_new <- nc_create(yearly_netcdf, list(var_gwcode, var_gwcode2), force_v4 = TRUE)

# Populate netcdf file #
gwcode <- readRDS(RASTER_FILES[grepl("gwcode", RASTER_FILES)])
ncvar_put(ncid_new, var_gwcode, as.array(gwcode), start=c(1,1,1), count=c(nx,ny,nt))

# Close netcdf file #
nc_close( ncid_new )

#### Static NetCDF ####

#### Seasonal NetCDF ####
