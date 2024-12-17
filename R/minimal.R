# install.packages("terra")
# library(terra)
#
# fname <- "/Users/ingvildsmestad/Downloads/food-food-insecurity-hotspots-inputs-geographic-geotiff/food-food-insecurity-hotspots-inputs-geographic-geotiff/food-food-insecurity-hotspots_200907_geographic.tif"
# r <- terra::rast(fname)
# plot(r) #
#
# pg <- prio_blank_grid()
#
#
#
# terra::res(r) < terra::res(pg)
#
# ragg <- terra::aggregate(r,
#                          fact = terra::res(pg)/terra::res(r),
#                          fun = "max",
#                          cores = 4)
# plot(ragg)
#
# tmp <- terra::rast(terra::ext(pg),
#                    crs = terra::crs(ragg),
#                    ncol = ncol(pg),
#                    nrow = nrow(pg))
# ragg_resample <- terra::resample(ragg, pg, method = "near", threads = T)
# plot(ragg_resample)
#
# df <- as.data.frame(c(pg, ragg_resample))
#
