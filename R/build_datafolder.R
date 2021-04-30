library(yaml)
library(tidyverse)

add_folders <- function(basepath, foldername){
  if(!dir.exists(file.path(basepath, foldername, "data"))){
    dir.create(file.path(basepath, foldername, "data"), recursive = TRUE)
  }
  if(!dir.exists(file.path(basepath, foldername, "docs"))){
    dir.create(file.path(basepath, foldername, "docs"), recursive = TRUE)
  }
}

add_datasource <- function(basepath, datasource){
  # create new folder
  add_folder(basepath, datasource$name)

  files_in_folder <- list.files(file.path(basepath, datasource$name, "data"), recursive = TRUE, all.files = TRUE)

  filename <- sapply(stringr::str_split(datasource$link, "/"), tail, 1)

  if(!filename %in% files_in_folder){
    # download file
    full_file_path <- file.path(basepath, datasource$name, "data", filename)
    download.file(url = datasource$link, destfile = full_file_path)
  }

  file_types_to_extract <- stringr::str_split(datasource$filefilter, "\\|")[[1]] %>% gsub("\\(|\\)", "", x=.)
  file_types_to_extract <- gsub("\\(|\\)", "", x=datasource$filefilter)

  if(tools::file_ext(filename) == "zip"){
    # read zip file and extract files to data folder
    zip_files_to_extract <- unzip(full_file_path, list = TRUE) %>% filter(grepl(file_types_to_extract, Name)) %>% pull(Name)

    # filter away existing files
    zip_files_to_extract <- zip_files_to_extract[!(zip_files_to_extract %in% files_in_folder)]

    if(length(zip_files_to_extract) > 0){
      # extract files that was not found in the folder
      unzip(full_file_path, files = zip_files_to_extract, exdir = file.path(basepath, datasource$name, "data"))
    }
  }
}

BASE_PATH <- "~/pgtest"

test <- yaml.load_file("~/test.yaml")


for(datasource in test$sites){
  add_datasource(BASE_PATH, datasource)
}








