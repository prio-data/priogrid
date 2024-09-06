download_file_httr2 <- function(url, filepath){
  url |>
    httr2::request() |>
    httr2::req_perform(path = filepath)
}

pg_rawfiles <- function(use_mirror = TRUE){
  if(use_mirror){
    urls <- pgmeta |>
      dplyr::mutate(url = dplyr::if_else(prio_mirror_url != "", prio_mirror_url, data_url)) |>
      dplyr::select(src_name, version, url)
  } else{
    urls <- pgmeta |> dplyr::select(src_name, version, url = data_url)
  }

  # Unnest because url-column can be a list of urls
  urls <- tidyr::unnest(urls, cols = c(url))
  urls <- urls |> dplyr::mutate(filename = file.path(paste(src_name, version, sep = "_"), basename(url)))
  return(urls)
}

get_pgfile <- function(src_name, version){
  f <- pg_rawfiles() |> dplyr::filter(src_name == !!rlang::enquo(src_name),
                                      version == !!rlang::enquo(version)) |> dplyr::pull(filename)
  destfolder <- pgoptions$get_rawfolder()
  if(length(f) == 0){
    return(message("No files in metadata with that name and version."))
  }

  if(!dir.exists(destfolder)){
    stop(paste(destfolder, "does not exist. Please pgoptions$set_rawfolder()."))
  }

  full_file_path <- file.path(destfolder, f)

  file_found <- file.exists(full_file_path)
  if(!all(file_found)){
    stop(paste("Some files were not found in", destfolder, ":", f[!file_found]))
  }

  return(full_file_path)
}

download_pg_rawdata <- function(overwrite = FALSE){
  destfolder <- pgoptions$get_rawfolder()

  if(!dir.exists(destfolder)){
    accept <- readline(paste("Destination folder", destfolder, "does not exist. Do you want to create? (Y)es: "))
    if(accept == "Y" | accept == "Yes"){
      dir.create(destfolder)
    } else(
      return(message("No folder to store data."))
    )
  }

  file_info <- pg_rawfiles()
  file_info$file_exists <- file.exists(file.path(destfolder, file_info$filename))
  file_info$subdir_exists <- dir.exists(file.path(destfolder, dirname(file_info$filename)))

  if(!overwrite){
    file_info <- file_info |> dplyr::filter(!file_exists)
  }

  total_files <- nrow(file_info)
  if(total_files==0){
    return(message(paste("All raw-files are already in", destfolder)))
  }

  subdirs <- file_info |> dplyr::filter(!subdir_exists) |> dplyr::pull(filename) |> dirname() |> unique()
  if(length(subdirs) > 0){
    dir.create(file.path(destfolder, subdirs))
  }

  pb <- utils::txtProgressBar(min = 0, max = total_files, style = 3)
  mapply(download_file_httr2, url = file_info$url, filepath = file.path(destfolder, file_info$filename))
  close(pb)
}
