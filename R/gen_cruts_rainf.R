#' gen_cruts_rainf_sum
#'
#' @description Generate monthly sum rainfall variable, available from 1901-present.
#' 
#' Please cite: University of East Anglia Climatic Research Unit; Harris, I.C.; Jones, P.D.; Osborn, T. (2023): CRU TS4.07: Climatic Research Unit (CRU) Time-Series (TS) version 4.07 of high-resolution gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2022). NERC EDS Centre for Environmental Data Analysis, date of citation. https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753
#' Please cite: Harris, I., Osborn, T.J., Jones, P. et al. Version 4 of the CRU TS monthly high-resolution gridded multivariate climate dataset. Sci Data 7, 109 (2020). https://doi.org/10.1038/s41597-020-0453-3
#' 
#' source: https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753 & https://crudata.uea.ac.uk/cru/data/hrg/
#'
#' @param input_folder path to [pg-folder].
#'
#' @export
gen_cruts_rainf_sum <- function(input_folder, fun = "sum"){

  fnames <- stringr::str_replace(Sys.glob("input/cru/*.pre.dat.nc"), "input/cru/", "")
  
  var <- paste0("cruts_rainf_", as.character(fun), "_mm_month")
  
  cruts_rainf <- file.path(input_folder, "input", "cru", fnames)
  
  cruts_rainf <- cruts_rainf %>% purrr::map(raster::brick, varname = "pre") %>%
    raster::stack() %>%
    priogrid::raster_to_pg(aggregation_function = fun) %>%
    priogrid::raster_to_tibble(add_pg_index = TRUE) %>%
    dplyr::group_by(pgid) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(c("X1", "X2")),
                        values_to = paste0(var),
                        names_to = "yearmonth") %>%
    dplyr::mutate(yearmonth = lubridate::ymd(stringr::str_replace(yearmonth, "X", ""))) %>%
    dplyr::ungroup()
  
  return(cruts_rainf)
}


#' gen_cruts_rainf_sd
#'
#' @description Generate monthly standard deviation rainfall variable, available from 1901-present.
#' 
#' Please cite: University of East Anglia Climatic Research Unit; Harris, I.C.; Jones, P.D.; Osborn, T. (2023): CRU TS4.07: Climatic Research Unit (CRU) Time-Series (TS) version 4.07 of high-resolution gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2022). NERC EDS Centre for Environmental Data Analysis, date of citation. https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753
#' 
#' source: https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753 & https://crudata.uea.ac.uk/cru/data/hrg/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_cruts_rainf_sd <- function(input_folder){
  gen_cruts_rainf_sum(input_folder, fun = "sd")
}


#' gen_cruts_rainf_max
#'
#' @description Generate monthly max rainfall variable, available from 1901-present.
#' 
#' Please cite: University of East Anglia Climatic Research Unit; Harris, I.C.; Jones, P.D.; Osborn, T. (2023): CRU TS4.07: Climatic Research Unit (CRU) Time-Series (TS) version 4.07 of high-resolution gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2022). NERC EDS Centre for Environmental Data Analysis, date of citation. https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753
#' 
#' source: https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753 & https://crudata.uea.ac.uk/cru/data/hrg/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_cruts_rainf_max <- function(input_folder){
  gen_cruts_rainf_sum(input_folder, fun = "max")
}


#' gen_cruts_rainf_min
#'
#' @description Generate monthly min rainfall variable, available from 1901-present.
#' 
#' Please cite: University of East Anglia Climatic Research Unit; Harris, I.C.; Jones, P.D.; Osborn, T. (2023): CRU TS4.07: Climatic Research Unit (CRU) Time-Series (TS) version 4.07 of high-resolution gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2022). NERC EDS Centre for Environmental Data Analysis, date of citation. https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753
#' 
#' source: https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753 & https://crudata.uea.ac.uk/cru/data/hrg/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_cruts_rainf_min <- function(input_folder){
  gen_cruts_rainf_sum(input_folder, fun = "min")
}


#' gen_cruts_rainf_mean
#'
#' @description Generate monthly mean rainfall variable, available from 1901-present.
#' 
#' Please cite: University of East Anglia Climatic Research Unit; Harris, I.C.; Jones, P.D.; Osborn, T. (2023): CRU TS4.07: Climatic Research Unit (CRU) Time-Series (TS) version 4.07 of high-resolution gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2022). NERC EDS Centre for Environmental Data Analysis, date of citation. https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753
#' 
#' source: https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753 & https://crudata.uea.ac.uk/cru/data/hrg/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_cruts_rainf_mean <- function(input_folder){
  gen_cruts_rainf_sum(input_folder, fun = "mean")
}


#' gen_cruts_rainf_med
#'
#' @description Generate monthly median rainfall variable, available from 1901-present.
#' 
#' Please cite: University of East Anglia Climatic Research Unit; Harris, I.C.; Jones, P.D.; Osborn, T. (2023): CRU TS4.07: Climatic Research Unit (CRU) Time-Series (TS) version 4.07 of high-resolution gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2022). NERC EDS Centre for Environmental Data Analysis, date of citation. https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753
#' 
#' source: https://catalogue.ceda.ac.uk/uuid/5fda109ab71947b6b7724077bf7eb753 & https://crudata.uea.ac.uk/cru/data/hrg/
#'
#' @param input_folder path to [pg-folder].
#' @param fun one of c("sum", "sd", "min", "max", "mean", "median").
#'
#' @export
gen_cruts_rainf_med <- function(input_folder){
  gen_cruts_rainf_sum(input_folder, fun = "median")
}

