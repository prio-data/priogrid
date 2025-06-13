#’ Reads the Li Nighttime data
#’
#’
#’
#’ @return an object of class sf
#’ @export
#’
#’ @references
#’ \insertRef{liHarmonizedGlobalNighttime2020}{priogrid}
read_linight <- function(){
  zip_file <- get_pgfile(source_name="Li Nighttime",
                         source_version="v8",
                         id="24d76a3b-927e-42ad-b8a5-2e7443e6a275")

  extractdir <-  paste0(stringr::str_sub(zip_file,start = 1,end = stringr::str_length(zip_file)-1))

  unzip(zipfile = zip_file, overwrite = FALSE, exdir = paste0(stringr::str_sub(zip_file,start = 1,end = stringr::str_length(zip_file)-1)))

  allfiles <- list.files(extractdir,pattern = "Harmonized")

  stdext <- c(-180,180,-90,90)

  for(i in 1:length(allfiles)){
    print("Harmonizing extent of harmonized files")
    print(paste("File",i,sep=" "))
    rsub <- terra::rast(x = paste0(extractdir,allfiles[i]))
    terra::ext(rsub) <- stdext
    terra::writeRaster(x = rsub, filename = paste0(extractdir,"extent_",allfiles[i]),overwrite=TRUE)
  }

  extfiles <- list.files(extractdir,pattern = "extent_")

  r <- terra::rast(paste0(extractdir,extfiles))

  # for(i in 1:length(allfiles)){
  #   if(i==1){
  #     print("1")
  #     r <- rsub <- terra::rast(allfiles[1])
  #   }
  #   if(i>1){
  #     print(i)
  #     rsub <- terra::rast(allfiles[i])
  #     terra::ext(rsub) <- terra::ext(terra::rast(allfiles[1]))
  #     terra::add(r) <- terra::rast(rsub)
  #   }
  # }
  yearnames <- readr::parse_number(allfiles)
  yearnames <- lubridate::as_date(paste(yearnames,"-12-31"))
  names(r) <- yearnames
  return(r)
}

#' Generate Li Nighttime Light
#'
#' This aggregates the high-resolution Li Nighttime Light to PRIO-GRID level across all years (1992-2021).
#'
#' This does take some time.
#'
#' A slight nearest neighbor resampling was applied to get the exact PRIO-GRID extent.
#'
#' @return SpatRast
#' @export
#'
#' @examples
#' # r <- gen_linight_grid()
#' @references
#' \insertRef{liHarmonizedGlobalNighttime2020}{priogrid}
gen_linight_grid <- function(){
  r <- read_linight()
  pg <- prio_blank_grid()

  temporary_directory <- file.path(pgoptions$get_rawfolder(), "tmp", tempdir() |> basename())
  dir.create(temporary_directory)

  tmp3 <- tempfile(pattern = "aggregate", fileext = ".tif", tmpdir = temporary_directory)

  r <- terra::aggregate(r,
                        fact = terra::res(pg)/terra::res(r),
                        fun = mean,
                        filename = tmp3,
                        gdal=c("COMPRESS=LZW"),
                        cores = cores)

  r <- terra::resample(r, pg, method = "near", threads = T)

  return(r)
}

