library(ncdf4)


input_folder <- "c:/Users/jonves/data/priogrid_data/"
output_folder <- "c:/Users/jonves/data/priogrid_res/"

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
years <- 1946:2020 # We have data at least until 2020. Expand this if needed.

nx <- length(xvals)
ny <- length(yvals)
nt <- length(years)

xdim <- ncdim_def( 'lon', 'degrees_east', xvals )
ydim <- ncdim_def( 'lat', 'degrees_north', yvals )
tdim <- ncdim_def( 'year', 'integer', 0, unlim=TRUE )

# Define variables #
integer_mv <- -99     # missing value
float_mv <- 1.e30     # missing value

gwcode <- ncvar_def(name = "gwcode", units = 'gwcode', dim = list(xdim,ydim,tdim), missval = integer_mv, compression = 7)
pop_gpw_sum <- ncvar_def(name = "pop_gpw_sum", units = 'population', dim = list(xdim,ydim,tdim), missval = integer_mv, compression = 7)
pop_gpw_sd <- ncvar_def(name = "pop_gpw_sd", units = 'standard deviation', dim = list(xdim,ydim,tdim), missval = float_mv, compression = 7)
pop_gpw_min <- ncvar_def(name = "pop_gpw_min", units = 'population', dim = list(xdim,ydim,tdim), missval = integer_mv, compression = 7)
pop_gpw_max <- ncvar_def(name = "pop_gpw_max", units = 'population', dim = list(xdim,ydim,tdim), missval = integer_mv, compression = 7)

# Make new output file #
ncid_new <- nc_create(yearly_netcdf, list(gwcode,
                                          pop_gpw_sum,
                                          pop_gpw_sd,
                                          pop_gpw_min,
                                          pop_gpw_max), force_v4 = TRUE)



# Populate netcdf file #
rast <- readRDS(RASTER_FILES[grepl("gwcode", RASTER_FILES)])
ncvar_put(ncid_new, gwcode, aperm(as.array(rast), c(2, 1, 3)), start=c(1,1,1), count=c(nx,ny,nt-4))

rast <- readRDS(RASTER_FILES[grepl("pop_gpw", RASTER_FILES)])
rast <- subset(rast, which(grepl("sum", names(rast))))
for(year in c(2000, 2005, 2010, 2015, 2020)){
  rast_year <- subset(rast, which(grepl(year, names(rast))))
  ncvar_put(ncid_new, pop_gpw_sum, t(as.matrix(rast_year)), start=c(1,1,which(years == year)), count=c(nx,ny,1))
}
rast <- readRDS(RASTER_FILES[grepl("pop_gpw", RASTER_FILES)])
rast <- subset(rast, which(grepl("sd", names(rast))))
for(year in c(2000, 2005, 2010, 2015, 2020)){
  rast_year <- subset(rast, which(grepl(year, names(rast))))
  ncvar_put(ncid_new, pop_gpw_sd, t(as.matrix(rast_year)), start=c(1,1,which(years == year)), count=c(nx,ny,1))
}
rast <- readRDS(RASTER_FILES[grepl("pop_gpw", RASTER_FILES)])
rast <- subset(rast, which(grepl("min", names(rast))))
for(year in c(2000, 2005, 2010, 2015, 2020)){
  rast_year <- subset(rast, which(grepl(year, names(rast))))
  ncvar_put(ncid_new, pop_gpw_min, t(as.matrix(rast_year)), start=c(1,1,which(years == year)), count=c(nx,ny,1))
}
rast <- readRDS(RASTER_FILES[grepl("pop_gpw", RASTER_FILES)])
rast <- subset(rast, which(grepl("max", names(rast))))
for(year in c(2000, 2005, 2010, 2015, 2020)){
  rast_year <- subset(rast, which(grepl(year, names(rast))))
  ncvar_put(ncid_new, pop_gpw_max, t(as.matrix(rast_year)), start=c(1,1,which(years == year)), count=c(nx,ny,1))
}

# Close netcdf file #
nc_close( ncid_new )

# Example of use
tst <- priogrid::get_array(yearly_netcdf, "gwcode", fillvarname = "_FillValue", lon = "lon", lat = "lat",
                           start = c(1, 1, which(years == 2000) ), count = c(-1, -1, 1) )
tst <- priogrid::make_raster(tst, transpose = T)

#### Static NetCDF ####
static_netcdf <- paste(output_folder, "priogrid_static.nc", sep = "")

# Define dimensions #
xvals <- seq(-179.75, 179.75, by = 0.5)
yvals <- seq(-89.75, 89.75, by = 0.5)

nx <- length(xvals)
ny <- length(yvals)

xdim <- ncdim_def( 'lon', 'degrees_east', xvals )
ydim <- ncdim_def( 'lat', 'degrees_north', yvals )

diamprim_s <- ncvar_def(name = "diamprim_s", units = "n sites", dim = list(xdim, ydim), missval = integer_mv, compression = 7)
diamsec_s <- ncvar_def(name = "diamsec_s", units = "n sites", dim = list(xdim, ydim), missval = integer_mv, compression = 7)
grow_start <- ncvar_def(name = "grow_start", units = "month", dim = list(xdim, ydim), missval = integer_mc, compression = 7)
grow_end <- ncvar_def(name = "grow_end", units = "month", dim = list(xdim, ydim), missval = integer_mc, compression = 7)

# Make new output file #
ncid_new <- nc_create(static_netcdf, list(diamprim_s, diamsec_s), force_v4 = TRUE)

# Populate netcdf file #
rast <- readRDS(RASTER_FILES[grepl("diamonds", RASTER_FILES)])
rast <- subset(rast, which(grepl("diamprim_s", names(rast))))
ncvar_put(ncid_new, diamprim_s, t(as.matrix(rast)), start=c(1,1), count=c(nx,ny))
rast <- readRDS(RASTER_FILES[grepl("diamonds", RASTER_FILES)])
rast <- subset(rast, which(grepl("diamsec_s", names(rast))))
ncvar_put(ncid_new, diamsec_s, t(as.matrix(rast)), start=c(1,1), count=c(nx,ny))
rast <- readRDS(RASTER_FILES[grepl("mirca_grow_agg", RASTER_FILES)])
rast <- subset(rast, which(grepl("grow_start", names(rast))))
ncvar_put(ncid_new, grow_start, t(as.matrix(rast)), start=c(1,1), count=c(nx,ny))
rast <- readRDS(RASTER_FILES[grepl("mirca_grow_agg", RASTER_FILES)])
rast <- subset(rast, which(grepl("grow_end", names(rast))))
ncvar_put(ncid_new, grow_end, t(as.matrix(rast)), start=c(1,1), count=c(nx,ny))



# Close netcdf file #
nc_close( ncid_new )

# Example of use
tst <- priogrid::get_array(static_netcdf, "diamprim_s", fillvarname = "_FillValue", lon = "lon", lat = "lat",
                           start = c(1, 1), count = c(-1, -1) )
tst <- priogrid::make_raster(tst, transpose = T)


#### Seasonal NetCDF ####

seasonal_netcdf <- paste(output_folder, "priogrid_seasonal.nc", sep = "")

# Define dimensions #
xvals <- seq(-179.75, 179.75, by = 0.5)
yvals <- seq(-89.75, 89.75, by = 0.5)
months <- 1:12

nx <- length(xvals)
ny <- length(yvals)
nt <- length(months)

xdim <- ncdim_def( 'lon', 'degrees_east', xvals )
ydim <- ncdim_def( 'lat', 'degrees_north', yvals )
tdim <- ncdim_def( 'month', 'integer', months)


mirca_growseason <- ncvar_def(name = "mirca_growseason", units = "weight", dim = list(xdim, ydim, tdim), missval = float_mv, compression = 7)

# Make new output file #
ncid_new <- nc_create(seasonal_netcdf, list(mirca_growseason), force_v4 = TRUE)

# Populate netcdf file #


rast <- readRDS(RASTER_FILES[grepl("mirca_growseas", RASTER_FILES)])
for(month in months){
  rast_month <- subset(rast, which(tolower(month.abb[month]) == names(rast)))
  ncvar_put(ncid_new, mirca_growseason, t(as.matrix(rast_month)), start=c(1,1,which(months == month)), count=c(nx,ny,1))
}

# Close netcdf file #
nc_close( ncid_new )

# Example of use
tst <- priogrid::get_array(seasonal_netcdf, "mirca_growseason", fillvarname = "_FillValue", lon = "lon", lat = "lat",
                           start = c(1, 1, which(month.abb == "Jan")), count = c(-1, -1, 1) )
tst <- priogrid::make_raster(tst, transpose = T)
