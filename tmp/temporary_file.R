devtools::load_all()

cs <- read_cshapes()

pg <- prio_blank_grid()
points <- as.points(pg) |> sf::st_as_sf()


time_slices <- pg_dates()
measurement_date <- time_slices[165]
features <- cs |> dplyr::filter(measurement_date %within% date_interval)

sf::sf_use_s2(TRUE)

points$bdist2 <- units::set_units(NA, "km")
points <- dplyr::bind_cols(points, sf::st_coordinates(points) |> dplyr::as_tibble())

sample_points <- function(n = 500, min_distance = 5e5){
  longs <- seq(-180, 180, 0.01)
  lats <- seq(-90, 90, 0.01)

  sf::sf_use_s2(TRUE)

  sample_point <- function(x) sf::st_point(c(sample(longs, 1), sample(lats, 1)))

  ps <- list()
  i <- 1
  q <- 0
  while(i < n) {
    p <- sample_point(i)

    if(i > 1){
      tp <- sf::st_sfc(p, crs = 4326)
      tst <- sf::st_sf(tp = sf::st_sfc(ps, crs = 4326))
      if(!any(sf::st_distance(tp, tst) < units::set_units(min_distance, "m"))){
        ps[[i]] <- p
        i <- i + 1
      } else{
        q <- q + 1
      }
    } else {
      ps[[i]] <- p
      i <- i + 1
    }
    if(q > 5000){
      stop("Too many iterations, try a smaller n or smaller min_distance")
    }
  }

  ps <- sf::st_sf(p = sf::st_sfc(ps, crs = 4326))
  return(ps)
}

set.seed(42)
m <- matrix(1:25, nrow=10, ncol=60)
rm <- rast(m, crs = "ESRI:54009", ext = ext(-180, 180, -90, 90)) |>
  terra::as.points() |> sf::st_as_sf() |>
  sf::st_transform(crs = 4326)

survey_points <- prio_blank_grid(ncol = 60, nrow = 10, extent = NULL, crs_string = "ESRI:54009") |> terra::as.points() |> sf::st_as_sf()
#survey_points <- sample_points(n = 20, min_distance = 2e6)

sf::st_geometry(features) |> plot()
plot(sf::st_geometry(survey_points), pch = ".", col = "red", add = T)
plot(sf::st_geometry(rm), pch = ".", col = "red", add = T)


A <- points[seq(1, nrow(points), 4),]
B <- points[seq(1, nrow(points), 4)+1,]
C <- points[seq(1, nrow(points), 4)+2,]
D <- points[seq(1, nrow(points), 4)+3,]

plot(sf::st_geometry(A), pch = ".", col = "red", add = T)
plot(sf::st_geometry(B), pch = ".", col = "red", add = T)
plot(sf::st_geometry(C), pch = ".", col = "red", add = T)
plot(sf::st_geometry(D), pch = ".", col = "red", add = T)


start <- Sys.time()
result <- list()
sf::sf_use_s2(TRUE)
for(i in 1:nrow(survey_points)){
  print(i)
  subset_id <- i%%4 + 1
  if(subset_id == 1){
    sub_points <- A
  } else if(subset_id == 2){
    sub_points <- B
  } else if(subset_id == 3){
    sub_points <- C
  } else if(subset_id == 4){
    sub_points <- D
  } else{
    stop("Error in subset id")
  }
  #sub_points <- points[sample(1:nrow(points), nrow(points)/100 |> ceiling()),]
  #sub_points <- points

  xy <- sf::st_coordinates(survey_points[i,])
  stereographic_proj <- stringr::str_glue("+proj=stere +lat_0={xy[2]} +lon_0={xy[1]}") |> as.character()
  sP <- sf::st_transform(survey_points[i,], crs = stereographic_proj)
  sB <- sf::st_buffer(sP, dist = units::set_units(1000, "km"))

  pT <- sf::st_transform(sub_points, crs = stereographic_proj)
  pT <- sf::st_crop(pT, sB) # crops the points to the bounding box of the buffer

  # Outer boundary
  pB <- sf::st_buffer(pT, dist = units::set_units(4000, "km")) |> sf::st_combine() |> sf::st_boundary()

  fT <- sf::st_transform(features, crs = stereographic_proj)
  fC <- sf::st_crop(sf::st_make_valid(fT), pB) |>
    sf::st_boundary() |>
    sf::st_simplify() |>
    sf::st_make_valid()

  fIdx <- sf::st_nearest_feature(pT, fC)
  nearest_features <- fC[fIdx,]
  nearest_dyad <- sf::st_nearest_points(pT, nearest_features, pairwise = TRUE)
  pT$bdist2 <- sf::st_length(nearest_dyad) |> units::set_units("km")
  sf::st_geometry(pT) <- NULL
  result[[i]] <- pT |> dplyr::select(pgid, bdist2)
}
end <- Sys.time()
print(end-start)

all <- dplyr::bind_rows(result)
min_sampled_distances <- all |> dplyr::group_by(pgid) |>
  dplyr::summarize(bdist2 = min(bdist2))

pg <- prio_blank_grid()
pg_dist <- pg
values(pg_dist) <- dplyr::if_else(values(pg_dist) %in% min_sampled_distances$pgid, values(pg_dist), NA)
pg_dist <- terra::classify(pg_dist, min_sampled_distances)
names(pg_dist) <- "bdist2"

terra::interpIDW(pg, pg_dist |> as_points(), radius = 1)

interpolate_gstat <- function(model, x, crs = 4326, ...) {
  v <- sf::st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}

dist_samples <- terra::as.points(pg_dist) |> sf::st_as_sf()
v <- gstat::variogram(log(bdist2) ~ 1, data = dist_samples)
m <- gstat::fit.variogram(v, gstat::vgm(1, "Sph", 2000, 1))
g <- gstat::gstat(NULL, "log.bdist2", log(bdist2)~1, dist_samples, model=m)
plot(v, model = m)

z <- terra::interpolate(pg_dist, g, fun = interpolate_gstat)

plot(vc)

res <- gstat::krige(formula = log(bdist2) ~ 1,
                    locations = dist_samples,
                    newdata = terra::as.points(pg) |> sf::st_as_sf(),
                    nmax = 10,
                    nsim = 5,
                    model = m)
sp::spplot(res)

resp <- predict(res, terra::as.points(pg) |> sf::st_as_sf())
resp$x <- st_coordinates(resp)[,1]
resp$y <- st_coordinates(resp)[,2]
resp$pred <- resp$var1.pred

k <- gstat::gstat(formula = bdist2 ~ 1, data = dist_samples, model = gstat::vgm("Sph"))
kpred <- predict(k, terra::as.points(pg) |> sf::st_as_sf())

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
