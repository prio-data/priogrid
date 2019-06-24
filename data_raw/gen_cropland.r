#rm(list=ls())
library(priogrid)

temp <- tempdir()
temp_file <- tempfile()
# make a good way to download the file.
#url <- ??
#fname <- paste(temp, temp_file, sep="\")
#download.file(url, fname)
#cropland <- fname

cropland <- "C:/Users/jonas/Downloads/CroplandPastureArea2000_NetCDF.zip"
filenames <- unzip(cropland, list = T)
cropland <- unzip(cropland, files = filenames$Name[1], exdir = temp)

cropland <- get_array(cropland, variable="farea", fillvarname ="missing_value",
                      lon = "longitude", lat = "latitude")
cropland <- make_raster(cropland, transpose = T,
                        crs=CRS(prio_crs()))


# aggregate
cropland <- extend(cropland, prio_extent())
aggregation_factor <- round(prio_resolution() / res(cropland) )
# here we can also add max/min/sd aggregation if that can be seen as useful
cropland <- aggregate(cropland, fact = aggregation_factor, fun = mean)
# bilinear resampling can be used to upsample data with lower resolution.
# here we only use nearest neighbor resampling to make extents correct (resolution is the same)
s <- raster(nrow=prio_nrow(), ncol=prio_ncol())
cropland <- resample(cropland, s, method = 'ngb')

save(cropland, file = "data_raw/cropland.rda", compress = TRUE)

# Delete temporary directory
unlink(temp, recursive = T)
