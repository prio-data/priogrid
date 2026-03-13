#' Compute SPEI from very large arrayed data sets in netCDF format.
#' @param sca       Integer. The time scale of the SPEI.
#' @param inPre     Character vector. Path to the input precipitation netCDF file.
#' @param outFile   Character vector. Path of the output netCDF file.
#' @param inEtp     Character vector. Path to the input evapotranspiration netCDF file.
#' @param title     Character vector. Title of the dataset.
#' @param comment   Character vector. A comment.
#' @param version   Character vector. Dataset version number.
#' @param inMask    Character vector. Path to a netCDF mask file.
#' @param block     Integer. Number of latitude blocks to be processed at the
#' same time. Must be an integer dividend of 360.
#'
#' @return Computes the SPEI time series and stores it in outFile following
#' the same data structure of inPre.
#'
#' @section References:
#' Code from Beguería S. (2017) SPEIbase: R code used in generating the SPEI global database, doi:10.5281/zenodo.834462.
spei.nc <- function(sca, inPre, outFile, inEtp=NA, title=NA, comment=NA,
                    version=NA, inMask=NA, block=18, tlapse=NA) {
  require(SPEI)
  require(snowfall)
  require(ncdf4)
  require(Hmisc)

  # Read data
  pre.nc <- nc_open(inPre) # nc_open does not work with file connections
  if (!is.na(inEtp)) { # there is ETP input data, so compute the SPEI
    etp.nc <- nc_open(inEtp)
    isSPEI <- TRUE
  } else {
    isSPEI <- FALSE
  }

  if (!is.na(inMask)) { # there is a mask
    mask.nc <- nc_open(inMask)
    isMask <- TRUE
  } else {
    isMask <- FALSE
  }

  # Create variable and output file
  if (!is.na(tlapse[1])) {
    spi.dim.tme <- ncdim_def(pre.nc$dim$time$name, pre.nc$dim$time$units,
                             pre.nc$dim$time$vals[tlapse[1]:tlapse[2]], unlim=TRUE, calendar= "gregorian")
  } else {
    spi.dim.tme <- ncdim_def(pre.nc$dim$time$name, pre.nc$dim$time$units,
                             pre.nc$dim$time$vals, unlim=TRUE, calendar= "gregorian")
  }
  if (isSPEI) {
    nam <- 'spei'
    longnam <- 'Standardized Precipitation-Evapotranspiration Index'
  } else {
    nam <- 'spi'
    longnam <- 'Standardized Precipitation Index'
  }

  # Create SPEI/SPI variable
  out.nc.var <- ncvar_def(
    name=nam,
    units='1', # units value 1 is used for dimensionless variables
    dim=list(pre.nc$dim$lon, pre.nc$dim$lat, spi.dim.tme),
    missval=1.e30,
    longname=longnam,
    prec='float',
    compression=9
  )

  # Create the CRS variable
  crs_def <- ncvar_def(
    name = "crs",
    units = "",
    dim = list(),
    longname = "CRS definition",
    prec = "integer"
  )

  out.nc <- nc_create(outFile, vars = list(out.nc.var, crs_def))

  # Link the spei/spi variable to the crs variable
  ncatt_put(out.nc, nam, "grid_mapping", "crs")

  # Add attributes to the CRS variable to specify the CRS parameters
  # crs_wkt definition is supplementary (optional) to the grid_mapping attributes and it is recommended to be a single line in Well-known-Text version 2
  ncatt_put(out.nc, "crs", "grid_mapping_name", "latitude_longitude")
  ncatt_put(out.nc, "crs", "semi_major_axis", 6378137.0)
  ncatt_put(out.nc, "crs", "inverse_flattening", 298.257223563)
  ncatt_put(out.nc, "crs", "crs_wkt", 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]]')

  # Add lon attributes
  ncatt_put(out.nc,'lon','standard_name','longitude')
  ncatt_put(out.nc,'lon','axis','X')

  # Add lat attributes
  ncatt_put(out.nc,'lat','standard_name','latitude')
  ncatt_put(out.nc,'lat','axis','Y')

  # Add time attributes
  ncatt_put(out.nc,'time','standard_name','time')
  ncatt_put(out.nc,'time','axis','T')

  # Add Global attributes
  ncatt_put(out.nc,0,'conventions','CF-1.11')
  ncatt_put(out.nc,0,'title',title)
  ncatt_put(out.nc,0,'version',version)
  ncatt_put(out.nc,0,'id',outFile)
  ncatt_put(out.nc,0,'summary',paste('Global dataset of the Standardized Precipitation-Evapotranspiration Index (SPEI) at the ', sca,'-month', ifelse(sca==1,'','s'), ' time scale. ', comment, sep=''))
  ncatt_put(out.nc,0,'keywords','drought, climatology, SPEI, Standardized Precipitation-Evapotranspiration Index')
  ncatt_put(out.nc,0,'institution','Consejo Superior de Investigaciones Científicas, CSIC')
  ncatt_put(out.nc,0,'source','http://sac.csic.es/spei')
  ncatt_put(out.nc,0,'creators','Santiago Beguería <santiago.begueria@csic.es> and Sergio Vicente-Serrano <svicen@ipe.csic.es>')
  ncatt_put(out.nc,0,'software','Created in R using the SPEI package (https://cran.r-project.org/web/packages/SPEI/;https://github.com/sbegueria/SPEI)')
  b <- match.call()
  ncatt_put(out.nc,0,'call',
            paste(b[[1]],'(',names(b)[[2]],'=',b[[2]],', ',names(b)[[3]],'=',b[[3]],
                  ', ',names(b)[[4]],'=',b[[4]],')',sep=''))
  ncatt_put(out.nc,0,'date',date())
  ncatt_put(out.nc,0,'reference','Beguería S., Vicente-Serrano S., Reig F., Latorre B. (2014) Standardized precipitation evapotranspiration index (SPEI) revisited: Parameter fitting, evapotranspiration models, tools, datasets and drought monitoring. International Journal of Climatology 34, 3001-3023.')
  ncatt_put(out.nc,0,'reference2','Vicente-Serrano S.M., Beguería S., López-Moreno J.I. (2010) A Multiscalar Drought Index Sensitive to Global Warming: The Standardized Precipitation Evapotranspiration Index. Journal of Climate 23, 1696–1718.')
  ncatt_put(out.nc,0,'reference3','Beguería S., Vicente-Serrano S., Angulo-Martínez M. (2010) A multi-scalar global drought data set: the SPEIbase. Bulletin of the American Meteorological Society 91(10), 1351–1356.')

  # Dimensions
  dimlon <- out.nc.var$dim[[1]]$len
  dimlat <- out.nc.var$dim[[2]]$len
  dimtme <- out.nc.var$dim[[3]]$len

  # A vector with the number of days per month
  if (isSPEI) {
    ndays <- Hmisc::monthDays(as.Date(out.nc.var$dim[[3]]$vals,
                                      origin='1900-1-1'))
  }

  n <- block # latitudes block (number of lines read each time)
  for (j in seq(dimlat,1,-n)-n+1) { # j=265 (lat=42.25º), lon=361 (lon=0.25)
    start <- c(1,j,ifelse(is.na(tlapse[1]),1,tlapse[1]))
    count <- c(dimlon,n,dimtme)
    if (isSPEI) {
      # Read precipitation and ET data and calculate balance
      x <- ncvar_get(etp.nc, varid=etp.nc$var[[1]]$name, start, count) * ndays
      x <- ncvar_get(pre.nc, varid=pre.nc$var[[1]]$name, start, count) - x
    } else {
      # Read precipitation data
      x <- ncvar_get(pre.nc, varid=pre.nc$var[[1]]$name, start, count)
    }
    if (isMask) { # use a mask file
      msk <- ncvar_get(mask.nc, 'mask', c(1,j), c(dimlon,n))
    }
    # Convert from matrix to list
    x.list <- vector('list', dimlon*n)
    for (i in 1:dimlon) {
      for (h in 1:n-1) {
        if (!isMask) {
          x.list[[h*dimlon+i]] <- x[i,{h+1},]
        } else {
          if (msk[i,{h+1}]==1) {
            x.list[[h*dimlon+i]] <- x[i,{h+1},]
          } else {
            x.list[[h*dimlon+i]] <- rep(NA,dimtme)
          }
        }
      }
    }

    # Compute series of anomalies
    speii <- function(d,s) {SPEI::spei(d, s, na.rm=TRUE, verbose=FALSE)$fitted}
    x.list <- sfLapply(x.list, speii, sca)
    # Convert back to matrix
    for (i in 1:dimlon) {
      for (h in 1:n-1) {
        part <- x.list[[h*dimlon+i]]
        part[is.nan(part/part)] <- NA
        x[i,{h+1},] <- part
      }
    }

    # Store into output netCDF file
    ncvar_put(out.nc, nam, x, c(1,j,1), count)
    nc_sync(out.nc)
  } # next j

  # close output netCDF file
  nc_close(out.nc)
}


#' Read and Process Global SPEI database data
#'
#' While this data can be downloaded from the Global SPEI database, it is only open
#' for download through manual operations on the web-page. The code for reproducing
#' SPEIbase is open, however. We use this here to process CRU PET and PRE data. The
#' input for SPEIbase 2.11 is CRU v.4.09.
#'
#' We use R-code from Beguería S. (2017) SPEIbase: R code used in generating the SPEI global database, doi:10.5281/zenodo.834462.
#'
#' @param interval Integer. The month interval to calculate SPEI over. E.g., if the interval is
#'  6, then monthly precipitation and potential evapotranspiration are aggregated over the last 6 months,
#'  then the SPEI (anomaly) is calculated. This is done for each month.
#'
#' @seealso
#' \code{\link{get_pgfile}} for file retrieval functionality,
#' \code{\link{pg_date_intervals}} for PRIO-GRID temporal boundaries
#'
#' @export
#' @references
#' \insertRef{begueriaSPEIbase2024}{priogrid}
#' \insertRef{begueriaStandardizedPrecipitationEvapotranspiration2014}{priogrid}
read_speibase <- function(interval = 6) {

  spei_interval <- paste0("spei", formatC(interval, width=2, format='d', flag='0'), ".nc")
  out_path <- file.path(pgoptions$get_rawfolder(),
                        "Global SPEI database",
                        "2.11",
                        "14839384-623a-4cf7-9241-6166a8ac465b",
                        spei_interval)

  if(!file.exists(out_path)){
    # Calculate SPEI. This is a parallel process using snowfall.
    cru_pet_gz <- get_pgfile(
      source_name = "CRU Climate pet",
      source_version = "v4.09",
      id = "95399c70-7db4-47f0-95e5-2e279b6b2054")

    cru_pre_gz <- get_pgfile(
      source_name = "CRU Climate pre",
      source_version = "v4.09",
      id = "00575260-ad1c-4e87-a575-3922bc151f50")

    cru_pet <- tools::file_path_sans_ext(cru_pet_gz)
    cru_pre <- tools::file_path_sans_ext(cru_pre_gz)

    require(snowfall)
    sfInit(parallel=TRUE, cpus=parallel::detectCores()-2)
    sfExport(list='spei', namespace='SPEI')

    spei.nc(
      sca=interval,
      inPre=cru_pre,
      inEtp=cru_pet,
      outFile=out_path,
      title=paste('Global ',interval,'-month',
                  ifelse(interval==1,'','s'),' SPEI, z-values, 0.5 degree',sep=''),
      comment='Using CRU TS 4.09 precipitation and potential evapotranspiration data',
      version='2.11',
      block=36,
      inMask=NA,
      tlapse=NA
    )
    gc()
    sfStop()
  }

  r <- terra::rast(out_path)
  names(r) <- terra::time(r)

  pg_period <- lubridate::interval(
    pg_date_intervals() |> lubridate::int_start() |> min(),
    pg_date_intervals() |> lubridate::int_end()   |> max()
  )

  terra::subset(r, which(terra::time(r) %within% pg_period))
}

#' Make SPEIbase data PRIO-GRID compatible
#'
#' The function takes monthly SPEIbase data and transforms it to the temporal
#' resolution defined by PRIO-GRID date intervals (which may be quarterly, yearly,
#' or other intervals), while also performing spatial aggregation to the PRIO-GRID resolution.
#'
#' @param interval Integer. The month interval to calculate SPEI over. E.g., if the interval is
#'  6, then monthly precipitation and potential evapotranspiration are aggregated over the last 6 months,
#'  then the SPEI (anomaly) is calculated. This is done for each month.
#' @param time_agg_fun Character. Either "mean" or "max". Original data is monthly, so if PRIO-GRID is lower
#'  resolution, then we need to aggregate over time. Currently, mean and max functions are implemented.
#'
#'
#' @return A \code{SpatRaster} object (from the \pkg{terra} package) with spatio-temporal
#' resolution as defined in PRIO-GRID.
#'
#' @examples
#' \dontrun{
#' # SPEI-6 averaged over PRIO-GRID time-interval using the mean.
#' spei6_mean <- speibaseN(interval = 6, time_agg_fun = "mean")
#' }
#'
#' @seealso
#' \code{\link{read_speibase}} for reading and calculating SPEIbase,
#' \code{\link{read_cru_pet}} for input-data to SPEIbase,
#' \code{\link{read_cru_pre}} for input-data to SPEIbase,
#' \code{\link{pg_date_intervals}} for PRIO-GRID temporal boundaries,
#' \code{\link{robust_transformation}} for spatial aggregation,
#' \code{\link{get_pgfile}} for file management
#'
#' @export
#' @references
#' \insertRef{begueriaSPEIbase2024}{priogrid}
#' \insertRef{begueriaStandardizedPrecipitationEvapotranspiration2014}{priogrid}
speibaseN <- function(interval, time_agg_fun){
  r <- read_speibase(interval = interval)

  spei_time_interval <- lubridate::interval(terra::time(r) |> min(),
                                           terra::time(r) |> max())

  pg_intervals <- pg_date_intervals()[pg_date_intervals() %within% spei_time_interval]

  res <- list()
  # SPEI data comes in monthly resolution. Aggregate to lower resolution if PRIO-GRID is lower (e.g,. quarterly, yearly)
  time_groups <- lapply(pg_intervals, function(x) which(terra::time(r) %within% x))
  i <- 1
  for(tgroup in time_groups){
    rt <- terra::subset(r, tgroup)
    if(time_agg_fun == "mean"){
      rt <- terra::mean(rt)
    } else if(time_agg_fun == "max"){
      rt <- terra::max(rt)
    } else{
      stop("Only 'mean' and 'max' time aggregation functions are implemented.")
    }

    res[[i]] <- rt
    i <- i + 1
  }
  res <- terra::rast(res)
  names(res) <- lubridate::int_end(pg_intervals)

  res <- robust_transformation(res, agg_fun = "mean")

  return(res)
}

#' Generate SPEI-6 with mean-temporal aggregation compatible with PRIO-GRID
#'
#' The function takes monthly SPEIbase data and transforms it to the temporal
#' resolution defined by PRIO-GRID date intervals (which may be quarterly, yearly,
#' or other intervals), while also performing spatial aggregation to the PRIO-GRID resolution.
#'
#'
#' @return A \code{SpatRaster} object (from the \pkg{terra} package) with spatio-temporal
#' resolution as defined in PRIO-GRID.
#'
#' @examples
#' \dontrun{
#' # SPEI-6 averaged over PRIO-GRID time-interval using the mean.
#' spei6_mean <- gen_speibase6_mean()
#' }
#'
#' @seealso
#' \code{\link{read_speibase}} for reading and calculating SPEIbase,
#' \code{\link{read_cru_pet}} for input-data to SPEIbase,
#' \code{\link{read_cru_pre}} for input-data to SPEIbase,
#' \code{\link{pg_date_intervals}} for PRIO-GRID temporal boundaries,
#' \code{\link{robust_transformation}} for spatial aggregation,
#' \code{\link{get_pgfile}} for file management
#'
#' @export
#' @references
#' \insertRef{begueriaSPEIbase2024}{priogrid}
#' \insertRef{begueriaStandardizedPrecipitationEvapotranspiration2014}{priogrid}
gen_speibase6_mean <- function(){
  speibaseN(interval = 6, time_agg_fun = "mean")
}
