devtools::load_all()

cs <- read_cshapes()

pg <- prio_blank_grid()
points <- as.points(pg) |> sf::st_as_sf()


time_slices <- pg_dates()
measurement_date <- time_slices[165]
features <- cs |> dplyr::filter(measurement_date %within% date_interval)

sf::st_geometry(features) |> plot()
sf::sf_use_s2(TRUE)

points$bdist2 <- units::set_units(NA, "km")

d6 <- list(c(0,0),
           c(90, 0),
           c(-90, 0),
           c(180,0),
           c(0,90),
           c(0,-90))

d12 <- list(
  c(41.81, 7.26),
  c(-6.73, 34.46),
  c(90, 69.10),
  c(90, -54.11),
  c(0, -7.26),
  c(-135, 35.26),
  c(-136.81, 0),
  c(-155.91, -50.77),
  c(-148.28, -46.44),
  c(160.69, -33.48),
  c(-148.28, 0),
  c(118.34, 0)
)

d20 <- list(
  c(0, 90), # North Pole
  c(0, -90), # South Pole

  c(0, -54),
  c(-120, -54),
  c(120, -54),

  c(-180, -18),
  c(-120, -18),
  c(-60, -18),
  c(0, -18),
  c(60, -18),
  c(120, -18),

  c(-150, 18),
  c(-90, 18),
  c(-30, 18),
  c(30, 18),
  c(90, 18),
  c(150, 18),

  c(0, 54),
  c(-120, 54),
  c(120, 54)
)

d12 <- lapply(d12, sf::st_point) |> sf::st_sfc(crs = 4326) |> sf::st_as_sf()
d20 <- lapply(d20, sf::st_point) |> sf::st_sfc(crs = 4326) |> sf::st_as_sf()


strategic_points <- lapply(strategic_points, sf::st_point) |> sf::st_sfc(crs = 4326) |> sf::st_as_sf()

points$nearest_strategy <- sf::st_nearest_feature(points, d20)

stereographic_proj <- "+proj=stere +lat_0=90 +lon_0=0"

fT <- sf::st_transform(features, crs = stereographic_proj)
pT <- sf::st_transform(points, crs = stereographic_proj)

cS <- d20[1,] |> sf::st_transform(crs = stereographic_proj)
pB <- sf::st_buffer(cS, dist = units::set_units(4000, "km")) |> sf::st_combine() |> sf::st_boundary()
fC <- sf::st_crop(sf::st_make_valid(fT), pB)

plot(sf::st_geometry(features))
points |> dplyr::filter(nearest_strategy == 1) |> sf::st_geometry() |> plot(add = T, col = "red")
points |> dplyr::filter(nearest_strategy == 2) |> sf::st_geometry() |> plot(add = T, col = "pink")
points |> dplyr::filter(nearest_strategy == 3) |> sf::st_geometry() |> plot(add = T, col = "skyblue")
points |> dplyr::filter(nearest_strategy == 4) |> sf::st_geometry() |> plot(add = T, col = "green")
points |> dplyr::filter(nearest_strategy == 5) |> sf::st_geometry() |> plot(add = T, col = "darkgreen")
points |> dplyr::filter(nearest_strategy == 6) |> sf::st_geometry() |> plot(add = T, col = "yellow")
points |> dplyr::filter(nearest_strategy == 7) |> sf::st_geometry() |> plot(add = T, col = "magenta")
points |> dplyr::filter(nearest_strategy == 8) |> sf::st_geometry() |> plot(add = T, col = "blue")
points |> dplyr::filter(nearest_strategy == 9) |> sf::st_geometry() |> plot(add = T, col = "green")
points |> dplyr::filter(nearest_strategy == 10) |> sf::st_geometry() |> plot(add = T, col = "purple")
points |> dplyr::filter(nearest_strategy == 11) |> sf::st_geometry() |> plot(add = T, col = "skyblue")
points |> dplyr::filter(nearest_strategy == 12) |> sf::st_geometry() |> plot(add = T, col = "magenta")
points |> dplyr::filter(nearest_strategy == 13) |> sf::st_geometry() |> plot(add = T, col = "green")
points |> dplyr::filter(nearest_strategy == 14) |> sf::st_geometry() |> plot(add = T, col = "darkgreen")
points |> dplyr::filter(nearest_strategy == 15) |> sf::st_geometry() |> plot(add = T, col = "yellow")
points |> dplyr::filter(nearest_strategy == 16) |> sf::st_geometry() |> plot(add = T, col = "magenta")
points |> dplyr::filter(nearest_strategy == 17) |> sf::st_geometry() |> plot(add = T, col = "green")
points |> dplyr::filter(nearest_strategy == 18) |> sf::st_geometry() |> plot(add = T, col = "blue")
points |> dplyr::filter(nearest_strategy == 19) |> sf::st_geometry() |> plot(add = T, col = "darkblue")
points |> dplyr::filter(nearest_strategy == 20) |> sf::st_geometry() |> plot(add = T, col = "pink")

start <- Sys.time()
result <- list()
sf::sf_use_s2(TRUE)
for(i in 1:nrow(d20)){
  print(i)
  sub_points <- points |> dplyr::filter(nearest_strategy == i)
  xy <- sf::st_coordinates(d20[i,])
  stereographic_proj <- stringr::str_glue("+proj=stere +lat_0={xy[2]} +lon_0={xy[1]}") |> as.character()
  pT <- sf::st_transform(sub_points, crs = stereographic_proj)
  pB <- sf::st_buffer(pT, dist = units::set_units(4000, "km")) |> sf::st_combine() |> sf::st_boundary()

  pTT <- sf::st_transform(points, crs = stereographic_proj)
  pTT <- sf::st_crop(pTT, pB)

  fT <- sf::st_transform(features, crs = stereographic_proj)
  fC <- sf::st_crop(sf::st_make_valid(fT), pB)

  fC <- sf::st_boundary(fC)
  fC <- sf::st_simplify(fC) |> sf::st_make_valid()
  fIdx <- sf::st_nearest_feature(pTT, fC)
  nearest_features <- fC[fIdx,]
  nearest_dyad <- sf::st_nearest_points(pTT, nearest_features, pairwise = TRUE)
  pTT$bdist2 <- sf::st_length(nearest_dyad) |> units::set_units("km")
  sf::st_geometry(pTT) <- NULL
  result[[i]] <- pTT |> dplyr::select(pgid, bdist2)
}
end <- Sys.time()
print(end-start)

all <- dplyr::bind_rows(result)

plot(all["bdist2"])

all_vect <- terra::vect(all)
terra::rasterize(all_vect, pg, field = "bdist2") |> plot()

plot(sf::st_geometry(fC))
plot(sf::st_geometry(pT), pch = ".", col = "red", add = T)

sf::sf_use_s2(TRUE)
fIdx <- sf::st_nearest_feature(pT, fC)
nearest_dyad <- sf::st_nearest_points(pT, fC[fIdx,], pairwise = TRUE)
plot(sf::st_geometry(nearest_dyad), col = "blue", add = T)

sf::st_length(nearest_dyad)

pointsT <- sf::st_transform(points,"+proj=longlat +datum=WGS84 +pm=180") |> sf::st_wrap_dateline()
featuresT <- sf::st_transform(features,"+proj=longlat +datum=WGS84 +pm=180") %>% sf::st_wrap_dateline()

# https://github.com/r-spatial/sf/issues/1762 cshapes does does not support spherical geometry
#features <- spdep::poly2nb(sf::st_make_valid(features))

sf::sf_use_s2(FALSE)
nearest_feature <- sf::st_nearest_feature(points, features)
nearest_feature <- features[nearest_feature,]

points_vect <- terra::vect(points)
nearest_feature_vect <- terra::vect(nearest_feature |> dplyr::select(cntry_name, gwcode, geometry))

tst <- terra::distance(points, features, pairwise = TRUE)

for(i in 1:nrow(points)) {
  sf::st_nearest_points(points[i,], features[nearest_feature[i],])
}

nearest_point <- sf::st_nearest_points(points, features[nearest_feature,], pairwise = TRUE)
nearest_featureT <- sf::st_nearest_feature(pointsT, featuresT)
nearest_pointT <- sf::st_nearest_points(pointsT, featuresT[nearest_featureT,], pairwise = TRUE)

distances <- sf::st_length(nearest_point)
distancesT <- sf::st_length(nearest_pointT)

distances <- apply(cbind(distances, distancesT), 1, min)
